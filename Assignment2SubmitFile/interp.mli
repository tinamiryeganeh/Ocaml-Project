(* $Id: interp.mli,v 1.1 2021-10-03 11:39:15-07 - - $ *)

(*
* Interpreter for Mini Basic
*)

val source_filename : string ref

val interpret_program : Absyn.program -> unit

