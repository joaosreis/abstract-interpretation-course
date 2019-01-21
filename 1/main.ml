(*
  Cours "Semantics and applications to verification"

  Antoine Miné 2014
  Marc Chevalier 2018
  Ecole normale supérieure, Paris, France / CNRS / INRIA
*)

(*
  Simple driver: parses the file given as argument and prints it back.

  You should modify this file to call your functions instead!
*)


(* parse and print filename *)
let doit filename =
  let prog = File_parser.parse_file filename in
  (* let () = Abstract_syntax_printer.print_prog Format.std_formatter prog in *)
  let (body, _) = prog in
  let open N_interpreter in
  let res = List.fold_left (fun acc ->
      function Abstract_syntax_tree.AST_stat s ->
        eval_stat acc s) (create_empty_env_err_set ()) body in
  Env_err_set.exists (function Interpreter.E_error -> exit 1 | _ -> exit 0) res

(* parses arguments to get filename *)
let main () =
  match Array.length Sys.argv with
  | 2 -> doit Sys.argv.(1)
  | 0 -> Printf.printf "Usage: executable file\n"; exit 1
  | _ -> Printf.printf "Usage: %s file\n" Sys.argv.(0); exit 1

let _ = main ()
