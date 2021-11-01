(* $Id: etc.mli,v 1.1 2021-10-03 11:39:15-07 - - $ *)

(*
* Main program and system access.
*)

val warn : string list -> unit

val die : string list -> unit

val syntax_error : Lexing.position -> string list -> unit

val parse_failed : bool ref

val usage_exit : unit -> unit

val read_number : unit -> float

val int_of_round_float : float -> int

