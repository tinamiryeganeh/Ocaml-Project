(* $Id: tables.ml,v 1.2 2021-10-09 14:25:52-07 - - $ *)

open Printf;;

type unary_fn_table_t = (string, float -> float) Hashtbl.t
type binary_fn_table_t = (string, float -> float -> float) Hashtbl.t
type bool_fn_table_t = (string, float -> float -> bool) Hashtbl.t
type variable_table_t = (string, float) Hashtbl.t
type array_table_t = (string, float array) Hashtbl.t
type label_table_t = (string, Absyn.program) Hashtbl.t

let unary_fn_table : unary_fn_table_t = Hashtbl.create 16
let _ = List.iter (fun (label, value) ->
                   Hashtbl.replace unary_fn_table label value)
                 ["+"    , (~+.);
                  "-"    , (~-.);
                  "abs"  , abs_float;
                  "acos" , acos;
                  "asin" , asin;
                  "atan" , atan;
                  "ceil" , ceil;
                  "cos"  , cos;
                  "exp"  , exp;
                  "floor", floor;
                  "log"  , log;
                  "log10", log10;
                  "log2" , (fun x -> log x /. log 2.0);
                  "round", Float.round;
                  "sin"  , sin;
                  "sqrt" , sqrt;
                  "tan"  , tan;
                  "trunc", (fun x -> snd (modf x));
                 ]

let binary_fn_table : binary_fn_table_t = Hashtbl.create 16
let _ = List.iter (fun (label, value) ->
                  Hashtbl.replace binary_fn_table label value)
                 ["+", (+.);
                  "-", (-.);
                  "*", ( *.);
                  "/", (/.);
                  "%", mod_float;
                  "^", ( ** );
                 ]

let bool_fn_table : bool_fn_table_t = Hashtbl.create 16
let _ = List.iter (fun (label, value) ->
                   Hashtbl.replace bool_fn_table label value)
		   ["=", (=);
		    "!=", (!=);
		    "<=", (<=);
		    ">=", (>=);
                    ">", (>);
                    "<", (<);
		   ]

let variable_table : variable_table_t = Hashtbl.create 16
let _ = List.iter (fun (label, value) ->
                   Hashtbl.replace variable_table label value)
                 ["e"  , exp 1.0;
                  "eof", 0.0;
                  "pi" , acos (~-. 1.0);
                  "nan", nan;
                 ]

let array_table : array_table_t = Hashtbl.create 16

let label_table : label_table_t = Hashtbl.create 16

let rec init_label_table program =
    let rec init program = match program with
        | [] -> ()
        | (_, Some label, _)::rest ->
              (Hashtbl.replace label_table label program; init rest)
        | (_, None, _)::rest -> init rest
    in (Hashtbl.reset label_table; init program)

let debug_label_table () =
    let debug key value = match value with
        | [] -> ()
        | (line, _, _)::_ ->
           fprintf stderr "label_table: \"%s\" -> line %d\n%!" key line
    in Hashtbl.iter debug label_table
