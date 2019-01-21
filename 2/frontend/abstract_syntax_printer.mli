(*
  Cours "Semantics and applications to verification"

  Antoine Miné 2014
  Marc Chevalier 2018
  Ecole normale supérieure, Paris, France / CNRS / INRIA
*)

(*
  Pretty-printer for abstract syntax trees.
*)

open Format
open Abstract_syntax_tree

(* locations *)
val string_of_position: position -> string
val string_of_extent: extent -> string

(* printers *)
val print_var: formatter -> variable -> unit
val print_lvalue: formatter -> lvalue -> unit
val print_expr: formatter -> expr -> unit
val print_stat: string -> formatter -> stat -> unit
val print_block: string -> formatter -> stat ext list -> unit
val print_prog: formatter -> prog -> unit
