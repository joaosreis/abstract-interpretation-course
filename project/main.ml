(*
  Cours "Sémantique et Application à la Vérification de programmes"

  Antoine Miné 2015
  Marc Chevalier 2018
  Ecole normale supérieure, Paris, France / CNRS / INRIA
*)

(*
  Simple driver: parses the file given as argument and prints it back.

  You should modify this file to call your functions instead!
*)

module Interval_abstraction = Domain.NonRelational(Interval)
module Constant_abstraction = Domain.NonRelational(Constant)
module Constant_iterator = Iterator.Make(Constant_abstraction)
module Interval_iterator = Iterator.Make(Interval_abstraction)


(* parse filename *)
let doit filename =
  let prog = File_parser.parse_file filename in
  let cfg = Tree_to_cfg.prog prog in
  Printf.printf "%a" Cfg_printer.print_cfg cfg;
  Cfg_printer.output_dot "cfg.dot" cfg;
  (* let d = Interval_iterator.process_cfg cfg in *)
  (* let open Cfg in *)
  (* let main = List.find (fun f -> f.func_name = "main") cfg.cfg_funcs in *)
  (* let env = NodeMap.find main.func_exit d in *)
  let env = Interval_iterator.init cfg in
  Interval_abstraction.print stdout env

(* parses arguments to get filename *)
let main () =
  match Array.to_list Sys.argv with
  | _::filename::_ -> doit filename
  | _ -> invalid_arg "no source file specified"

let _ = main ()
