(*
   Part of the Austral project, under the Apache License v2.0 with LLVM Exceptions.
   See LICENSE file for details.

   SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
*)
open Identifier
open Span
open SourceContext
open ErrorText

(* Types *)

type error =
  | Parse of ParseErrors.t
  | Cli of CliErrors.t
  | Type of TypeErrors.t
  | Linearity of LinearityErrors.t
  | Declaration of DeclarationErrors.t
  | Entrypoint of EntrypointErrors.t
  | Generic of GenericErrors.t
  | Internal of string

type place =
  | AtSpan of span
  | NoSpan

type location =
  | At of { module_name: module_name option; span: span }
  | InModule of module_name
  | Nowhere

type austral_error = AustralError of {
      location: location;
      error: error;
      source_ctx: source_ctx option;
    }

exception Austral_error of austral_error

let error_text (error: error): err_text =
  match error with
  | Parse e -> ParseErrors.render e
  | Cli e -> CliErrors.render e
  | Type e -> TypeErrors.render e
  | Linearity e -> LinearityErrors.render e
  | Declaration e -> DeclarationErrors.render e
  | Entrypoint e -> EntrypointErrors.render e
  | Generic e -> GenericErrors.render e
  | Internal message ->
     [
       Text "Internal compiler error: ";
       Text message;
       Break;
       Text "This is a bug in the compiler, please open an issue here: ";
       Code "https://github.com/austral/austral/issues"
     ]

(* Locations *)

let locate (module_name: module_name option) (place: place): location =
  match (place, module_name) with
  | (AtSpan span, _) -> At { module_name; span }
  | (NoSpan, Some module_name) -> InModule module_name
  | (NoSpan, None) -> Nowhere

let location_module_name (location: location): module_name option =
  match location with
  | At { module_name; _ } -> module_name
  | InModule module_name -> Some module_name
  | Nowhere -> None

let location_span (location: location): span option =
  match location with
  | At { span; _ } -> Some span
  | InModule _ | Nowhere -> None

(* Raising. Every function takes the place first: the code that raises
   says where the error is. *)

let raise_error (place: place) (error: error): 'a =
  raise (Austral_error (AustralError { location = locate None place; error; source_ctx = None }))

let parse_error (place: place) (e: ParseErrors.t): 'a = raise_error place (Parse e)

let cli_error (place: place) (e: CliErrors.t): 'a = raise_error place (Cli e)

let type_error (place: place) (e: TypeErrors.t): 'a = raise_error place (Type e)

let linearity_error (place: place) (e: LinearityErrors.t): 'a = raise_error place (Linearity e)

let declaration_error (place: place) (e: DeclarationErrors.t): 'a = raise_error place (Declaration e)

let entrypoint_error (place: place) (e: EntrypointErrors.t): 'a = raise_error place (Entrypoint e)

let generic_error (place: place) (e: GenericErrors.t): 'a = raise_error place (Generic e)

let internal_err (place: place) (message: string): 'a = raise_error place (Internal message)

let ok_or_raise (place: place) (result: ('a, error) result): 'a =
  match result with
  | Ok value -> value
  | Error error -> raise_error place error

let place_inside (outer: place) (span: span): place =
  match outer with
  | AtSpan _ when span = empty_span -> outer
  | AtSpan _ | NoSpan -> AtSpan span

(* Error context-augmenting functions *)

let add_source_ctx (error: austral_error) (ctx: source_ctx): austral_error =
  let (AustralError { location; error = cause; source_ctx }) = error in
  match source_ctx with
  | Some _ ->
     error
  | None ->
     AustralError { location; error = cause; source_ctx = Some ctx; }

let add_module_name (error: austral_error) (new_module_name: module_name): austral_error =
  let (AustralError { location; error = cause; source_ctx }) = error in
  match location with
  | At { module_name = Some _; _ } | InModule _ ->
     error
  | At { module_name = None; span } ->
     AustralError { location = locate (Some new_module_name) (AtSpan span); error = cause; source_ctx }
  | Nowhere ->
     AustralError { location = locate (Some new_module_name) NoSpan; error = cause; source_ctx }

(* Error rendering to plain text *)

let error_title (error: error): string =
  match error with
  | Generic _ -> "Generic Error"
  | Parse _ -> "Parse Error"
  | Cli _ -> "Command Line Arguments Error"
  | Type _ -> "Type Error"
  | Linearity _ -> "Linearity Error"
  | Declaration _ -> "Declaration Error"
  | Entrypoint _ -> "Entrypoint Definition Error"
  | Internal _ -> "Internal Error"

let indent_text (text: string) (indent: int): string =
  let lines = String.split_on_char '\n' text in
  let lines =
    List.map
      (fun line ->
        let pad = String.make indent ' '
        in
        pad ^ line)
      lines
  in
  String.concat "\n" lines

let render_error_to_plain (error: austral_error): string =
  let (AustralError { location; error = cause; source_ctx }) = error in
  let mn_text: string =
    match location_module_name location with
    | Some mn ->
       "  Module:\n"
       ^ "    " ^ (mod_name_string mn) ^ "\n"
    | None ->
       "  Module:\n"
       ^ "    [unknown]\n"
  in
  let title: string = error_title cause in
  let span_text =
    match location_span location with
    | Some (Span { filename; startp; endp }) ->
       "  Location:\n"
       ^ "    Filename: '" ^ filename ^ "'\n"
       ^ "    From: " ^ (position_to_string startp) ^ "\n"
       ^ "    To: " ^ (position_to_string endp) ^ "\n"
    | None ->
       "  Location:\n"
       ^ "    [no span available]\n"
  and code_text =
    (match source_ctx with
     | Some ctx ->
        "  Code:\n"
        ^ (indent_text (source_ctx_to_plain_text ctx) 4)
     | None ->
        "  Code:\n"
        ^ "    [no span available]\n")
  in
  "Error:\n"
  ^ "  Title: " ^ title ^ "\n"
  ^ mn_text
  ^ span_text
  ^ "  Description:\n"
  ^ (indent_text (error_text_to_plain (error_text cause)) 4) ^ "\n"
  ^ code_text
  ^ "\n"

let pos_json (p: position): Yojson.Basic.t =
  let (Position { line; column; }) = p in
  `Assoc [
      ("line", `Int line);
      ("column", `Int column)
    ]

let span_json (span: span): Yojson.Basic.t =
  let (Span { filename; startp; endp; }) = span in
  `Assoc [
      ("filename", `String filename);
      ("startp", pos_json startp);
      ("end", pos_json endp)
    ]

let source_ctx_json (ctx: source_ctx): Yojson.Basic.t =
  let (SourceContext lines) = ctx in
  let line_json ((n, l): (int * string)): Yojson.Basic.t =
    `List [`Int n; `String l]
  in
  `List (List.map line_json lines)

let render_error_to_json (error: austral_error): Yojson.Basic.t =
  (* Break up the error. *)
  let (AustralError { location; error = cause; source_ctx }) = error in
  (* Render components to JSON. *)
  let module_name: Yojson.Basic.t =
    match location_module_name location with
    | Some mn -> `String (mod_name_string mn)
    | None -> `Null
  in
  let span: Yojson.Basic.t =
    match location_span location with
    | Some span -> span_json span
    | None -> `Null
  in
  let source_ctx: Yojson.Basic.t =
    match source_ctx with
    | Some source_ctx -> source_ctx_json source_ctx
    | None -> `Null
  in
  (* Put it together. *)
  `Assoc [
      ("module", module_name);
      ("kind", `String (error_title cause));
      ("text", error_text_to_json (error_text cause));
      ("span", span);
      ("context", source_ctx)
    ]
