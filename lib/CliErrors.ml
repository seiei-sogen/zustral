(*
   Part of the Austral project, under the Apache License v2.0 with LLVM Exceptions.
   Modified in the Zustral project, a fork of Austral.
   See LICENSE file for details.

   SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
*)
(** The errors with the user's command line arguments, and their wording. *)
open ErrorText

type t =
  | UnknownCommand of string
  | UnknownOption of string
  | InvalidEntrypoint of string
  | InvalidModuleSource of string
  | MissingEntrypoint
  | MissingModule
  | MissingOutput
  | NoEntrypointWrongTarget
  | UnknownTarget of string
  | UnknownErrorReportingMode of string
  | DuplicateFlags of string
  | ExpectedBoolFlag of string
  | ExpectedValueFlag of string
  | UnknownFlagType of string
  | WriteError of { path: string; reason: string }
  | ReadError of { path: string; reason: string }

let render (error: t): err_text =
  match error with
  | UnknownCommand command ->
     [
       Text "Unknown command ";
       Code command;
       Text ". Run ";
       Code "zustral --help";
       Text " to see the commands."
     ]
  | UnknownOption option ->
     [
       Text "Unknown option ";
       Code option;
       Text ". Run ";
       Code "zustral --help";
       Text " to see the options."
     ]
  | InvalidEntrypoint entry ->
     [
       Text "Invalid entrypoint format ";
       Code entry;
       Break;
       Text "The entrypoint must be supplied in the form ";
       Code "Module:name"
     ]
  | InvalidModuleSource source ->
     [
       Text "Invalid module source format ";
       Code source;
       Break;
       Text "Sources must be supplied as either ";
       Code "interface.zui,body.zum";
       Text " or ";
       Code "body.zum"
     ]
  | MissingEntrypoint ->
     [
       Code "--entrypoint";
       Text " argument not provided."
     ]
  | MissingModule ->
     [
       Text "The ";
       Code "compile";
       Text " command must specify at least one module."
     ]
  | MissingOutput ->
     [
       Code "--output";
       Text " argument not provided."
     ]
  | NoEntrypointWrongTarget ->
     [
       Code "--no-entrypoint";
       Text " requires ";
       Code "--target-type=c";
       Text ", because otherwise the compiler will try to build the generated C code, and will fail because there is no entrypoint function."
     ]
  | UnknownTarget target ->
     [
       Text "Unknown target type ";
       Code target
     ]
  | UnknownErrorReportingMode mode ->
     [
       Text "Unknown error reporting mode: ";
       Code mode;
       Text ". Valid values are ";
       Code "plain";
       Text " and ";
       Code "json";
       Text "."
     ]
  | DuplicateFlags name ->
     [
       Text "The argument ";
       Code name;
       Text " was given multiple times"
     ]
  | ExpectedBoolFlag name ->
     [
       Text "Expected ";
       Code name;
       Text " to be a boolean flag, but it was given as a value flag."
     ]
  | ExpectedValueFlag name ->
     [
       Text "Expected ";
       Code name;
       Text " to be a value flag, but it was given as a boolean flag."
     ]
  | UnknownFlagType flag ->
     [
       Text "Invalid flag argument ";
       Code flag
     ]
  | WriteError { path; reason } ->
     [
       Text "Failed to write to file ";
       Code path;
       Text ": ";
       Break;
       Text reason
     ]
  | ReadError { path; reason } ->
     [
       Text "Failed to read file ";
       Code path;
       Text ":";
       Break;
       Text reason
     ]
