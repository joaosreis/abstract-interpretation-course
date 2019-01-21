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
  (* Abstract_syntax_printer.print_prog Format.std_formatter prog *)
  let open Type_analysis in
  try match List.fold_left (fun env ->
      function Abstract_syntax_tree.AST_stat s ->
        eval_stat env s) (Env Type_analysis.Env.empty) (fst prog)
    with
      E_top -> exit 1
    | _ -> exit 0
  with _ -> exit 1

(* parses arguments to get filename *)
let main () =
  match Array.length Sys.argv with
  | 2 -> doit Sys.argv.(1)
  | 0 -> Printf.printf "Usage: executable file\n"; exit 1
  | _ -> Printf.printf "Usage: %s file\n" Sys.argv.(0); exit 1

let _ = main ()
