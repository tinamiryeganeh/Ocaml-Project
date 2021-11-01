(* $Id: main.ml,v 1.1 2021-10-03 11:39:15-07 - - $ *)

(*
* Main program reads a file and prints to stdout.
*)

open Printf;;

let interpret_source filename =
   (if !Debug.debug then
        (printf "DEBUG:";
         Array.iter (printf " %s") Sys.argv;
         printf "\n%!");
    try (Interp.source_filename := filename;
         let sourcefile =
             if filename = "-" then stdin
                               else open_in filename
         in let lexbuf = Lexing.from_channel sourcefile
         in let abstract_syntax = Parser.program Scanner.token lexbuf
         in if !Etc.parse_failed
            then Etc.die ["parse failed"]
            else Interp.interpret_program abstract_syntax)
    with Sys_error (string) -> Etc.die [string]);;

let _ = if !Sys.interactive
        then ()
        else match Array.length Sys.argv with
             | 1 -> interpret_source "-"
             | 2 -> if Sys.argv.(1) = "-d"
                    then (Debug.debug := true;
                          interpret_source "-")
                    else interpret_source Sys.argv.(1)
             | 3 -> if Sys.argv.(1) = "-d"
                    then (Debug.debug := true;
                          interpret_source Sys.argv.(2))
                    else Etc.usage_exit ()
             | _ -> Etc.usage_exit ()

