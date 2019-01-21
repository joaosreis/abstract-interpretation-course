(*
  Cours "Semantics and applications to verification"

  Antoine Miné 2014
  Marc Chevalier 2018
  Ecole normale supérieure, Paris, France / CNRS / INRIA
*)

(*
  Opens and parses a file given as argument.
*)

open Abstract_syntax_tree
open Abstract_syntax_printer
open Lexing

(* parsing, with *very* nice error messages *)

let parse_file (filename: string) : prog =
  let file = open_in filename in
  let lexbuf = from_channel file in
  lexbuf.lex_curr_p <- {lexbuf.lex_curr_p with pos_fname = filename; pos_lnum = 1};
  let module MI = Parser.MenhirInterpreter in
  let checkpoint = Parser.Incremental.file lexbuf.lex_curr_p
  and supplier = MI.lexer_lexbuf_to_supplier Lexer.token lexbuf
  and succeed = Abstract_syntax_utils.post_process_program
  and fail checkpoint =
    let position = string_of_position lexbuf.lex_start_p in
    let error_msg =
      match checkpoint with
      | MI.HandlingError env -> env |> MI.current_state_number |> Parser_messages.message
      | _ -> assert false (* This should not happen. *)
    in
    Printf.printf "%s: %s" position error_msg;
    exit 1
  in
  MI.loop_handle succeed fail supplier checkpoint
