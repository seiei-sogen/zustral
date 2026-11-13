(*
   Part of the Austral project, under the Apache License v2.0 with LLVM Exceptions.
   See LICENSE file for details.

   SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
*)
open Id
open MonoType
open CodeGen
open CRepr
open ForeignType

let make_wrapper ((export_name, mono_id, { params; rt }): string * mono_id * mono_ty foreign_signature): c_decl =
  CFunctionDefinition (
      Desc "Wrapper function",
      (* The name the user asked for gets the reserved-word escape (issue #92). *)
      CIdent.external_escaped export_name,
      List.map (fun (name, ty) -> CValueParam (gen_ident name, gen_foreign_type ty)) params,
      gen_foreign_type rt,
      CBlock [
          CReturn (CFuncall (gen_mono_id mono_id, List.map (fun (n, _) -> CVar (gen_ident n)) params))
      ]
    )

let all_wrappers (exported: (string * mono_id * mono_ty foreign_signature) list): c_decl list Outcome.t =
  Outcome.catch (fun () -> List.map make_wrapper exported)
