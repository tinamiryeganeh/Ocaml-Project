(* $Id: etc.ml,v 1.1 2021-10-03 11:39:15-07 - - $ *)

open Printf;;

let execname = Filename.basename Sys.argv.(0)

let exit_status_ref : int ref = ref 0
let parse_failed : bool ref = ref false

let quit () =
    if !Sys.interactive
    then printf "quit (): exit %d\n%!" !exit_status_ref
    else exit !exit_status_ref

let eprint_list message =
    (exit_status_ref := 1;
     flush_all ();
     List.iter prerr_string message;
     prerr_newline ();
     flush_all ())

let warn message = eprint_list (execname :: ": " :: message)

let die message = (warn message; quit ())

let syntax_error position message =
    (warn (position.Lexing.pos_fname :: ": "
           :: string_of_int position.Lexing.pos_lnum :: ": "
           :: message);
     parse_failed := true)

let usage_exit message =
    (eprint_list ["Usage: "; execname; " [-d] [filename.mb]"];
     quit ())

let buffer : string list ref = ref []

let rec read_number () = match !buffer with
    | head::tail -> (buffer := tail;
                     try float_of_string head
                     with Failure _ -> nan)
    | [] -> let line = input_line stdin
            in (buffer := Str.split (Str.regexp "[ \\t]+") line;
                read_number ())

let int_of_round_float number =
    int_of_float (Float.round number)

