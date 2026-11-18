/*
   Part of the Zustral project, a fork of Austral.
   See LICENSE file for details.

   SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
*/

/* The operating system's number of a signal, from OCaml's number of it.
   OCaml numbers the signals it knows with negative constants (Sys.sigkill
   is -7), and the system's numbers differ between systems (SIGBUS is 7 on
   Linux and 10 on macOS). The runtime has the table, but OCaml 4.14 has no
   function that returns it. */

#include <caml/mlvalues.h>

/* Exported by the runtime (OCaml 4.14 and 5), declared in caml/signals.h
   only for the runtime's own code (CAML_INTERNALS). */
CAMLextern int caml_convert_signal_number(int);

value zustral_system_signal_number(value ocaml_signal_number)
{
  return Val_int(caml_convert_signal_number(Int_val(ocaml_signal_number)));
}
