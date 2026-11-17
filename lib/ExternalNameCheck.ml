(*
   Part of the Austral project, under the Apache License v2.0 with LLVM Exceptions.
   Modified in the Zustral project, a fork of Austral.
   See LICENSE file for details.

   SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
*)
open Error

let external_name_must_be_a_c_identifier (span: Span.span) (name: string): unit =
  let c_identifier: Str.regexp = Str.regexp "[A-Za-z_][A-Za-z0-9_]*" in
  if not (Str.string_match c_identifier name 0 && (Str.match_end () = String.length name)) then
    declaration_error (AtSpan span) (ExternalNameIsNotACIdentifier name)
  else if CIdent.is_c_reserved_word name then
    declaration_error (AtSpan span) (ExternalNameIsACReservedWord name)
  else
    ()
