(* debug.mli created by: ocamlopt -g -i debug.ml *)
(* date: Sun Oct 31 00:42:07 PDT 2021 *)
val debug : bool ref
val quote : string -> string
val join : string -> string -> string -> string list -> string
val string_of_option : ('a -> string) -> 'a option -> string
val string_of_ctor : string -> string list -> string
val string_of_list : ('a -> string) -> 'a list -> string
val string_of_printable : Absyn.printable -> string
val string_of_memref : Absyn.memref -> string
val string_of_expr : Absyn.expr -> string
val string_of_relexpr : Absyn.relexpr -> string
val string_of_stmt : Absyn.stmt -> string
val debug_progline : int * string option * Absyn.stmt option -> unit
val debug_program : Absyn.program -> unit
