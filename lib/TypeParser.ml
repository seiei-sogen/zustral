(*
   Part of the Austral project, under the Apache License v2.0 with LLVM Exceptions.
   See LICENSE file for details.

   SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
*)

open Identifier
open BuiltIn
open Type
open TypeSignature
open TypeParameter
open TypeParameters
open TypeSystem
open Region
open RegionMap
open Stages.Ast
open Env
open EnvTypes
open Error
open Id

let decl_type_signature (decl: decl): type_signature option =
  match decl with
  | Constant _ ->
     None
  | Record { id; name; typarams; universe; _ } ->
     Some (TypeSignature (name, typarams, universe, RecordTypeId id))
  | Union { id; name; typarams; universe; _ } ->
     Some (TypeSignature (name, typarams, universe, UnionTypeId id))
  | UnionCase _ ->
     None
  | Function _ ->
     None
  | TypeClass _ ->
     None
  | TypeClassMethod _ ->
     None
  | Instance _ ->
     None

let parse_function_pointer (place: place) (args: ty list): ty =
  match args with
  | [] ->
     parse_error place FunctionPointerNoArgs
  | first::rest ->
     let (args, rt): ty list * ty = Util.split_last first rest in
     (* The parameters are read in reverse (#127). *)
     FnPtr (List.rev args, rt)

let parse_built_in_type (place: place) (name: qident) (args: ty list): ty option =
  if is_address_type name then
    match args with
    | [ty] ->
       Some (Address ty)
    | _ ->
       generic_error place (TypeWrongArity {
           typename = make_ident BuiltInNames.address_name;
           expected = 1;
           actual = None;
         })
  else
    if is_pointer_type name then
      match args with
      | [ty] ->
         Some (Pointer ty)
      | _ ->
         generic_error place (TypeWrongArity {
             typename = make_ident BuiltInNames.pointer_name;
             expected = 1;
             actual = None;
           })
    else
      match BuiltInNames.type_of_name (ident_string (original_name name)) with
      | Some BuiltInNames.Unit ->
         Some Unit
      | Some BuiltInNames.Bool ->
         Some Boolean
      | Some BuiltInNames.Nat8 ->
         Some (Integer (Unsigned, Width8))
      | Some BuiltInNames.Nat16 ->
         Some (Integer (Unsigned, Width16))
      | Some BuiltInNames.Nat32 ->
         Some (Integer (Unsigned, Width32))
      | Some BuiltInNames.Nat64 ->
         Some (Integer (Unsigned, Width64))
      | Some BuiltInNames.Int8 ->
         Some (Integer (Signed, Width8))
      | Some BuiltInNames.Int16 ->
         Some (Integer (Signed, Width16))
      | Some BuiltInNames.Int32 ->
         Some (Integer (Signed, Width32))
      | Some BuiltInNames.Int64 ->
         Some (Integer (Signed, Width64))
      | Some BuiltInNames.ByteSize ->
         Some (Integer (Unsigned, WidthByteSize))
      | Some BuiltInNames.Index ->
         Some (Integer (Unsigned, WidthIndex))
      | Some BuiltInNames.Float32 ->
         Some SingleFloat
      | Some BuiltInNames.Float64 ->
         Some DoubleFloat
      | Some BuiltInNames.Static ->
         Some (RegionTy static_region)
      | Some BuiltInNames.Fn ->
         Some (parse_function_pointer place args)
      | None ->
         None

let rec effective_universe (place: place) name (typarams: typarams) declared_universe args =
  (* Algorithm:

     1. If the declared universe is Free then none of the type parameters can be Linear or Type.

         1.1 Unless it's `Pointer[T]` or `Address[T]`. But we don't have to take
         care of that here, since those are built in types, and the
         `type_universe` function determines their universe.

     2. If the declared universe is Linear, then no type parameter can change this. Therefore, the effective
        universe is Linear.

     3. If the declared universe is Region, that's the declaration error
        RegionUsedAsType: a region is not a type.

     4. Finally, if the universe is Type:
         4.1. First, assert that there is at least one type parameter. This should have been checked by the
              compiler when declarations are processed.

         4.2. If any argument's universe is Linear, the effective universe is Linear.

         4.3. Otherwise, if any argument's universe is Type, the effective universe is Type. The ordering
              here is because Linear is stronger (i.e. lower in the poset of universes) than Type.

         4.4. Otherwise, the effective universe is Free.
   *)
  match declared_universe with
  | FreeUniverse ->
     if all_arguments_are_free args then
       FreeUniverse
     else
       generic_error place (FreeTypeWithNonFreeArgument name)
  | LinearUniverse ->
     LinearUniverse
  | RegionUniverse ->
     (* A type declared later in the module in the Region universe: the type
        parser reads its signature before extraction rejects the declaration
        with TypesAreNotRegions (#141). *)
     declaration_error place (RegionUsedAsType (original_name name))
  | TypeUniverse ->
     assert ((typarams_size typarams) > 0);
     if any_arg_is_linear args then
       LinearUniverse
     else
       if any_arg_is_type args then
         TypeUniverse
       else
         FreeUniverse

(** Given a list of types, check that all of them are either in the Free or
    Region universes. *)
and all_arguments_are_free (args: ty list): bool =
  let is_compatible_with_free = function
    | FreeUniverse ->
       true
    | RegionUniverse ->
       true
    | LinearUniverse | TypeUniverse ->
       false
  in
  List.for_all is_compatible_with_free (List.map type_universe args)

(* Type signature retrieval *)

let get_type_signature (place: place) (env: env) (sigs: type_signature list) (name: qident) =
  let get_local_type_signature (sigs: type_signature list) (name: qident): type_signature option =
    List.find_opt (fun (TypeSignature (n, _, _, _)) -> n = (local_name name)) sigs

  and get_foreign_type_signature (env: env) (name: qident): type_signature option =
    match get_decl_by_name place env (qident_to_sident name) with
    | (Some decl) ->
       decl_type_signature decl
    | None ->
       None
  in
  match get_local_type_signature sigs name with
  | (Some (TypeSignature (n, typarams, universe, local_id))) ->
     (* The local signature is found by the local name, also for a name that
        the module imports and declares as a type (#28): the checks read the
        local type, and the type is the imported declaration. So the ID is the
        one of the declaration the name stands for, when that is a record or a
        union, and the reserved one of the local type otherwise: a type of the
        module that is not registered yet, or a name that also stands for a
        union case or a typeclass method (#86). *)
     let id: type_decl_id =
       match get_foreign_type_signature env name with
       | Some (TypeSignature (_, _, _, id)) -> id
       | None -> local_id
     in
     Some (TypeSignature (n, typarams, universe, id))
  | None ->
     get_foreign_type_signature env name

(* Parsing *)

let rec parse_type (place: place) (env: env) (sigs: type_signature list) (rm: region_map) (typarams: typarams) (ts: qtypespec): ty =
  match ts with
  | QTypeSpecifier (name, args) ->
     let args' = List.map (parse_type place env sigs rm typarams) args in
     (match parse_built_in_type place name args' with
      | Some ty ->
         ty
      | None ->
         (match is_region rm name with
          | Some ty ->
             ty
          | None ->
             (match is_param typarams name with
              | Some ty ->
                 ty
              | None ->
                 parse_user_defined_type place env sigs name args')))
  | QReadRef (ty, r) ->
     let ty' = parse_type place env sigs rm typarams ty
     and r' = parse_type place env sigs rm typarams r
     in
     ReadRef (ty', ref_region_of_ty r')
  | QWriteRef (ty, r) ->
     let ty' = parse_type place env sigs rm typarams ty
     and r' = parse_type place env sigs rm typarams r
     in
     WriteRef (ty', ref_region_of_ty r')
  | QSpan (ty, r) ->
     let ty' = parse_type place env sigs rm typarams ty
     and r' = parse_type place env sigs rm typarams r
     in
     Span (ty', ref_region_of_ty r')
  | QSpanWrite (ty, r) ->
     let ty' = parse_type place env sigs rm typarams ty
     and r' = parse_type place env sigs rm typarams r
     in
     SpanMut (ty', ref_region_of_ty r')

(* Is the given name a type parameter in the list of type paramters? If so,
   return it as a type variable. *)
and is_param (typarams: typarams) (name: qident): ty option =
  let name' = original_name name in
  match get_typaram typarams name' with
  | Some tp ->
     Some (TyVar (typaram_to_tyvar tp))
  | None ->
     None

(* Is the given name a region parameter? If so, return it as a RegionTy instance
   by finding the region in the region map. *)
and is_region (rm: region_map) (name: qident): ty option =
  match get_region rm (original_name name) with
  | (Some r) -> Some (RegionTy r)
  | None -> None

and parse_user_defined_type (place: place) (env: env) (sigs: type_signature list) (name: qident) (args: ty list): ty =
  match get_type_signature place env sigs name with
  | Some ts ->
     parse_user_defined_type' place ts name args
  | None ->
     type_error place (UnresolvedType (original_name name))

and parse_user_defined_type' (place: place) (ts: type_signature) (name: qident) (args: ty list): ty =
  let (TypeSignature (_, ts_params, declared_universe, id)) = ts in
  (* Check: the number of type parameters in the signature matches the number of
     type arguments *)
  check_param_arity_matches place ts_params args name;
  (* Check: the universe of each type argument matches the universe of each type
     parameter in the type signature. *)
  check_universes_match place ts_params args;
  (* Construct the named type *)
  let universe = effective_universe place name ts_params declared_universe args in
  NamedType (name, id, args, universe)

and check_param_arity_matches (place: place) (params: typarams) (args: ty list) (ty_name: qident) : unit =
  let expected: int = typarams_size params
  and got: int = List.length args
  in
  if expected = got then
    ()
  else
    generic_error place (TypeWrongArity { typename = original_name ty_name; expected; actual = Some got })

and check_universes_match (place: place) (params: typarams) (args: ty list): unit =
  let _ = List.map2 (check_universes_match' place) (typarams_as_list params) args in ()

and check_universes_match' (place: place) (tp: type_parameter) (arg: ty): unit =
  let param_u = typaram_universe tp
  and arg_u = type_universe arg in
  if universe_compatible param_u arg_u then
    ()
  else
    type_error place (TypeArgumentWrongUniverse { param = tp; expected = param_u; actual = arg_u })

and universe_compatible param arg =
  (* The check here is:

     1. If the parameter universe is Free, the argument universe must be Free.
     2. If the parameter universe is Linear, the argument universe must be Linear.
     3. If the parameter universe is Type, the argument universe must be any one of Free, Linear, or
        type.
     4. If the parameter universe is Region, the argument universe must be Region.
   *)
  match param with
  | FreeUniverse ->
     (match arg with
      | FreeUniverse -> true
      | LinearUniverse | TypeUniverse | RegionUniverse -> false)
  | LinearUniverse ->
     (match arg with
      | LinearUniverse -> true
      | FreeUniverse | TypeUniverse | RegionUniverse -> false)
  | TypeUniverse ->
     (match arg with
      | FreeUniverse | LinearUniverse | TypeUniverse -> true
      | RegionUniverse -> false)
  | RegionUniverse ->
     (match arg with
      | RegionUniverse -> true
      | FreeUniverse | LinearUniverse | TypeUniverse -> false)
