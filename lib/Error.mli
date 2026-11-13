(*
   Part of the Austral project, under the Apache License v2.0 with LLVM Exceptions.
   See LICENSE file for details.

   SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
 *)

(** Error reporting infrastructure. *)
open Identifier
open Span
open SourceContext
open ErrorText

(** What went wrong. Each category has its own type, with one constructor per
    error; the wording is in the [render] function of the category's module. *)
type error =
  | Parse of ParseErrors.t
  (** A parse error. *)
  | Cli of CliErrors.t
  (** An error with the user's command line arguments. *)
  | Type of TypeErrors.t
  (** A type error. *)
  | Linearity of LinearityErrors.t
  (** Signalled when code breaks the linearity rules. *)
  | Declaration of DeclarationErrors.t
  (** An error in the structure of a declaration. *)
  | Entrypoint of EntrypointErrors.t
  (** An error with the definition of the program entrypoint. *)
  | Generic of GenericErrors.t
  (** A generic error. This should be gradually phased out. *)
  | Internal of string
  (** A state the compiler believes cannot happen. The string says which. *)

(** Where in the source text an error is, as the code that raises it knows
    it. That code does not know the module: the parser raises before the
    module has a name, and the driver knows which module a stage works on. *)
type place =
  | AtSpan of span
  | NoSpan
  (** The error is reported with no span. Each use is a place where a span
      could be passed and is not (issue #87). *)

(** Where a finished error is. *)
type location =
  | At of { module_name: module_name option; span: span }
  (** A syntax error has a span before the module has a name. *)
  | InModule of module_name
  (** An error with no span, in a module. *)
  | Nowhere
  (** An error of the command line, or one with no span raised before the
      module has a name. *)

(** An Austral compiler error. The code that raises it gives the place, the
    driver adds the module name ({!add_module_name}), and the command line
    adds the source context. *)
type austral_error = AustralError of {
      location: location;
      (** Where the error occurred. *)
      error: error;
      (** What went wrong. *)
      source_ctx: source_ctx option;
      (** The source code where the error happened, if available. *)
    }

(** The exception that carries Austral errors. *)
exception Austral_error of austral_error

(** The wording of an error. *)
val error_text : error -> err_text

(** The module of a location, if it has one. *)
val location_module_name : location -> module_name option

(** The span of a location, if it has one. *)
val location_span : location -> span option

(** Raise a parse error. *)
val parse_error : ParseErrors.t -> 'a

(** Raise a parse error at a place in the source text. The lexer and the
    parser know the place where they stop. *)
val parse_error_at : span -> ParseErrors.t -> 'a

(** Raise an error with the user's command line arguments. *)
val cli_error : CliErrors.t -> 'a

(** Raise a type error. *)
val type_error : TypeErrors.t -> 'a

(** Raise a linearity error. *)
val linearity_error : LinearityErrors.t -> 'a

(** Raise a declaration error. *)
val declaration_error : DeclarationErrors.t -> 'a

(** Raise an error with the definition of the program entrypoint. *)
val entrypoint_error : EntrypointErrors.t -> 'a

(** Raise a generic error. *)
val generic_error : GenericErrors.t -> 'a

(** Raise an internal error. *)
val internal_err : string -> 'a

(** The value of a successful result. Raises the error of a failed one. A
    caller that cannot go on after the failure uses this on a function that
    returns its failure as a value. *)
val ok_or_raise : ('a, error) result -> 'a

(** The raising functions, with the place of the error as the first argument.
    They replace the ones above, file by file (issue #107): a file that opens
    this module after [Error] cannot raise without saying where. *)
module Placed : sig
  (** Raise a parse error. The lexer and the parser know the place where they
      stop. *)
  val parse_error : place -> ParseErrors.t -> 'a

  (** Raise an error with the user's command line arguments. *)
  val cli_error : place -> CliErrors.t -> 'a

  (** Raise a type error. *)
  val type_error : place -> TypeErrors.t -> 'a

  (** Raise a linearity error. *)
  val linearity_error : place -> LinearityErrors.t -> 'a

  (** Raise a declaration error. *)
  val declaration_error : place -> DeclarationErrors.t -> 'a

  (** Raise an error with the definition of the program entrypoint. *)
  val entrypoint_error : place -> EntrypointErrors.t -> 'a

  (** Raise a generic error. *)
  val generic_error : place -> GenericErrors.t -> 'a

  (** Raise an internal error. *)
  val internal_err : place -> string -> 'a

  (** The value of a successful result. Raises the error of a failed one, at
      the given place. A caller that cannot go on after the failure uses this
      on a function that returns its failure as a value. *)
  val ok_or_raise : place -> ('a, error) result -> 'a
end

(** Add a source context to an error if it doesn't have one. *)
val add_source_ctx : austral_error -> source_ctx -> austral_error

(** Put the module name in an error if it doesn't have one. *)
val add_module_name : austral_error -> module_name -> austral_error

(** Run the callback, and if it throws an error that doesn't have a span, put
    the given span in the error and rethrow it. *)
val adorn_error_with_span : span -> (unit -> 'a) -> 'a

(** The title of an error: the name of its category. *)
val error_title : error -> string

(** Render an error to plain text, for display to the terminal. *)
val render_error_to_plain : austral_error -> string

(** Render an error to JSON, for tools or the end to end tests. *)
val render_error_to_json : austral_error -> Yojson.Basic.t
