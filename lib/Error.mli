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
    adds the source context. The type is private: outside this module an error
    is read by its fields, and made only by {!make_error} or the raising
    functions, which all take a place. *)
type austral_error = private AustralError of {
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

(** The error that raising [error] at [place] raises, as a value. *)
val make_error : place -> error -> austral_error

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

(** The place of a part of the program whose span is [span], inside a part
    whose place is [outer]. A statement the compiler made up has the empty
    span ({!Span.empty_span}): an error in it is reported at the place around
    it, when that place has a span. *)
val place_inside : place -> span -> place

(** Add a source context to an error if it doesn't have one. *)
val add_source_ctx : austral_error -> source_ctx -> austral_error

(** Put the module name in an error if it doesn't have one. *)
val add_module_name : austral_error -> module_name -> austral_error

(** The title of an error: the name of its category. *)
val error_title : error -> string

(** Render an error to plain text, for display to the terminal. *)
val render_error_to_plain : austral_error -> string

(** Render an error to JSON, for tools or the end to end tests. *)
val render_error_to_json : austral_error -> Yojson.Basic.t
