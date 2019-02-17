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

open Cmdliner

module Interval_abstraction = Domain.NonRelational(Interval)
module Constant_abstraction = Domain.NonRelational(Constant)
module Constant_iterator = Iterator.Make(Constant_abstraction)
module Interval_iterator = Iterator.Make(Interval_abstraction)

(* parse filename *)
let doit cfg_file interval constant filename =
  let prog = File_parser.parse_file filename in
  let cfg = Tree_to_cfg.prog prog in
  Printf.printf "%a" Cfg_printer.print_cfg cfg;
  (match cfg_file with
     Some f ->  Cfg_printer.output_dot f cfg
   | None -> ());
  if interval then
    let d = Interval_iterator.process_cfg cfg in
    let open Cfg in
    NodeMap.iter (fun node env ->
        Printf.printf "%d: " node.node_id;
        Interval_abstraction.print stdout env;
        print_newline ()) d;
    if constant then
      let d = Constant_iterator.process_cfg cfg in
      let open Cfg in
      NodeMap.iter (fun node env ->
          Printf.printf "%d: " node.node_id;
          Constant_abstraction.print stdout env;
          print_newline ()) d


(* parses arguments to get filename *)
let main cfg interval constant src =
  if Sys.is_directory src then
    `Error (false, src ^ " is a directory")
  else
    `Ok (doit cfg interval constant src)

let cfg =
  let doc = "Generate dot with CFG." in
  Arg.(value & opt (some string) None & info ["cfg-out"] ~doc)

let interval =
  let doc = "Use interval domain." in
  Arg.(value & flag & info ["interval"] ~doc)

let constant =
  let doc = "Use interval domain." in
  Arg.(value & flag & info ["constant"] ~doc)

let src =
  let doc = "Source file to analyse." in
  Arg.(required & pos 0 (some file) None & info [] ~docv:"FILE" ~doc)

let cmd =
  let doc = "perform static analysis" in
  let exits = Term.default_exits in
  Term.(ret (const main $ cfg $ interval $ constant $ src)),
  Term.info "main" ~doc ~exits

let () = Term.(exit @@ eval cmd)
