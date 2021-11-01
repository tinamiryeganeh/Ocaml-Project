(* tables.mli created by: ocamlopt -g -i tables.ml *)
(* date: Sun Oct 31 00:42:07 PDT 2021 *)
type unary_fn_table_t = (string, float -> float) Hashtbl.t
type binary_fn_table_t = (string, float -> float -> float) Hashtbl.t
type bool_fn_table_t = (string, float -> float -> bool) Hashtbl.t
type variable_table_t = (string, float) Hashtbl.t
type array_table_t = (string, float array) Hashtbl.t
type label_table_t = (string, Absyn.program) Hashtbl.t
val unary_fn_table : unary_fn_table_t
val binary_fn_table : binary_fn_table_t
val bool_fn_table : bool_fn_table_t
val variable_table : variable_table_t
val array_table : array_table_t
val label_table : label_table_t
val init_label_table : Absyn.program -> unit
val debug_label_table : unit -> unit
