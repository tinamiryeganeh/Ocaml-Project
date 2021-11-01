(* $Id: interp.ml,v 1.3 2021-10-22 18:32:37-07 - - $ *)

open Absyn
open Printf
open Debug

let source_filename = ref ""

type binfn = float -> float -> float;;

let rec eval_expr (expr : Absyn.expr) : float = match expr with
    | Number number -> number
    | Memref memref -> eval_memref memref
    | Unary (oper, expr) -> eval_unary oper expr
    | Binary (oper, expr1, expr2) -> eval_binary oper expr1 expr2

and eval_memref (memref : Absyn.memref) : float = match memref with
    | Arrayref (ident, expr) -> eval_arrayref ident expr
    | Variable ident -> try Hashtbl.find Tables.variable_table ident
                        with Not_found -> 0.0

and eval_unary  (oper : Absyn.oper) (expr : Absyn.expr): float = 
	try let uniop = Hashtbl.find Tables.unary_fn_table  oper
		in uniop (eval_expr expr)
        with Not_found -> nan
 

and eval_binary  (oper : Absyn.oper) (expr1 : Absyn.expr) (expr2 : Absyn.expr) : float  = 
	try let binop = Hashtbl.find Tables.binary_fn_table oper
            in binop (eval_expr expr1) (eval_expr expr2)
        with Not_found -> nan


and eval_arrayref (ident : Absyn.ident) (expr : Absyn.expr): float =
    (*let arrid = arrid(0)*)
   (* for i = 0 to Array.length arrid - 1 do*)		
    	try let arrid = Hashtbl.find Tables.array_table ident
        	in arrid.(int_of_float(eval_expr expr)) 
        with Not_found -> nan

and eval_bool  (oper : Absyn.oper) (expr1 : Absyn.expr) (expr2 : Absyn.expr) : bool =
	try let boolid = Hashtbl.find Tables.bool_fn_table oper
        	in boolid (eval_expr expr1) (eval_expr expr2)
	with Not_found -> false

let rec interpret (program : Absyn.program) = match program with
    | [] -> ()
    | firstline::continuation -> match firstline with
       | _, _, None -> interpret continuation
       | _, _, Some stmt -> (interp_stmt stmt continuation)

and interp_stmt (stmt : Absyn.stmt) (continuation : Absyn.program) =
    match stmt with
    | Dim (ident, expr) -> interp_dim (ident, expr) continuation
    | Let (memref, expr) -> interp_let (memref, expr) continuation
    | Goto label -> interp_goto label continuation
    | If (expr, label) -> interp_if (expr, label) continuation
    | Print print_list -> interp_print print_list continuation
    | Input memref_list -> interp_input memref_list continuation

and interp_dim  (ident, expr : Absyn.ident * Absyn.expr)
                (continuation : Absyn.program) =
   (if !Debug.debug then
       printf "interp_dim: (%s, %s)\n%!" ident (string_of_expr expr);
   (* match ident with
    | Arrayref (ident, expr) -> Hashtbl.add Tables.array_table ident (eval_expr expr);
   *)
   (*Hashtbl.add Tables.array_table ident (eval_expr expr);*)
   interpret continuation)

and interp_let (memref, expr : Absyn.memref * Absyn.expr)
               (continuation : Absyn.program) =  
    (if !Debug.debug then
       printf "interp_let: (%s, %s)\n%!"
              (string_of_memref memref) (string_of_expr expr);
    match memref with  
    | Arrayref (ident, expr') -> interpret continuation
    | Variable ident -> Hashtbl.add Tables.variable_table ident (eval_expr expr);
   interpret continuation)
(*	
   (if !Debug.debug then
       printf "interp_let: (%s, %s)\n%!"
              (string_of_memref memref) (string_of_expr expr);
    match memref with
    | Arrayref (ident, expr') -> interpret continuation
    | Variable ident -> Hashtbl.add Tables.variable_table ident (eval_expr expr);
     interpret continuation)
*)

and interp_goto (label : Absyn.label) (continuation : Absyn.program) =
(*   interpret (Hashtbl.find Tables.label_table label)*)
  (if !Debug.debug then
       printf "interp_goto: %s\n%!" label;
  interpret (Hashtbl.find Tables.label_table label))
  (* interpret continuation)*)

and interp_if ((relexpr, label) : Absyn.relexpr * Absyn.label)
              (continuation : Absyn.program) =
   (if !Debug.debug then
       printf "interp_if: (%s, %s)\n%!"
              (string_of_relexpr relexpr) label;
    let Relexpr (oper, expr1, expr2) = relexpr in
  	 (if eval_bool oper expr1 expr2 then 
    interpret (Hashtbl.find Tables.label_table label)))
    (*interpret continuation)*)

and interp_print (print_list : Absyn.printable list)
                 (continuation : Absyn.program) =
   (if !Debug.debug then
        printf "interp_print: %s\n%!"
               (string_of_list string_of_printable print_list);
    let print_item item = match item with
        | String string ->
          let regex = Str.regexp "\"\\(.*\\)\""
          in print_string (Str.replace_first regex "\\1" string)
        | Printexpr expr ->
          print_string " "; print_float (eval_expr expr)
    in (List.iter print_item print_list; print_newline ());
    interpret continuation)


(*                 (continuation : Absyn.program) =
   (if !Debug.debug then
        printf "interp_print: %s\n%!"
               (string_of_list string_of_printable print_list);
    let print_item item = match item with
        | String string ->
          let regex = Str.regexp "\"\\(.*\\)\""
          in print_string (Str.replace_first regex "\\1" string)
        | Printexpr expr ->
          print_string " "; print_float (eval_expr expr)
    in (List.iter print_item print_list; print_newline ());
    interpret continuation)
*)
and interp_input (memref_list : Absyn.memref list)
                 (continuation : Absyn.program)  =
   (if !Debug.debug then
        printf "interp_input: %s\n%!"
               (string_of_list string_of_memref memref_list);
    let input_number memref =
        try  let number = Etc.read_number ()
             in (print_float number; print_newline ())
        with End_of_file -> 
             (print_string "End_of_file"; print_newline ())
    in List.iter input_number memref_list;
    interpret continuation)

let interpret_program program =
    (Tables.init_label_table program; 
     if !Debug.debug then Tables.debug_label_table ();
     if !Debug.debug then Debug.debug_program program;
     interpret program;
     if !Debug.debug then Tables.debug_label_table ())

