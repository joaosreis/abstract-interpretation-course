(*
  Cours "Semantics and applications to verification"

  Antoine Miné 2014
  Marc Chevalier 2018
  Ecole normale supérieure, Paris, France / CNRS / INRIA
*)

(*
  Definition of the abstract syntax trees output by the parser.
*)

(* position in the source file, we use ocamllex's default type *)
type position = Lexing.position
let position_unknown = Lexing.dummy_pos


(* extents are pairs of positions *)
type extent = position * position (* start/end *)
let extent_unknown = (position_unknown, position_unknown)


(* Many parts of the syntax are tagged with an extent indicating which
   part of the parser-file corresponds to the sub-tree.
   This is very useful for interesting error reporting!
*)
type 'a ext = 'a * extent

(* variable identifiers, just strings for now *)
(* local variables and scoping would require using UNIQUE IDENTIFIERS
   to handle the case where several variables have the same name
*)
type id = string

(* unary expression operators *)
type  unary_op =
  (* arithmetic operators *)
  | AST_UNARY_PLUS     (* +e *)
  | AST_UNARY_MINUS    (* -e *)
  (* logical operators *)
  | AST_NOT            (* !e logical negation *)


(* binary expression operators *)
type binary_op =
  (* arithmetic operators, work only for int *)
  | AST_PLUS          (* e + e *)
  | AST_MINUS         (* e - e *)
  | AST_MULTIPLY      (* e * e *)
  | AST_DIVIDE        (* e / e *)
  | AST_MODULO        (* e % e *)
  (* polymorphic comparison, should work for int and bool *)
  | AST_EQUAL         (* e == e *)
  | AST_NOT_EQUAL     (* e != e *)
  (* arithmetic comparisons, work only for int *)
  | AST_LESS          (* e < e *)
  | AST_LESS_EQUAL    (* e <= e *)
  | AST_GREATER       (* e > e *)
  | AST_GREATER_EQUAL (* e >= e *)
  (* boolean operators, work only for bool *)
  | AST_AND           (* e && e *)
  | AST_OR            (* e || e *)


(* expressions *)
type expr =
  (* unary operation *)
  | AST_unary of unary_op * (expr ext)
  (* binary operation *)
  | AST_binary of binary_op * (expr ext) * (expr ext)
  (* variable use *)
  | AST_variable of id ext
  (* constants (integers are still in their string representation) *)
  | AST_int_const of Z.t ext
  | AST_bool_const of bool
  (* non-deterministic choice between two integeres *)
  | AST_int_rand of (Z.t ext) (* lower bound *) *
                    (Z.t ext) (* upper bound *)


(* left part of assignments *)
type lvalue = id

(* statements *)
type stat =
  (* block of statements { ... } *)
  | AST_block of stat ext list
  (* assignment  lvalue = expr *)
  | AST_assign of (lvalue ext) * (expr ext)
  (* if-then-else; the else branch is optional *)
  | AST_if of (expr ext) (* condition *) *
              (stat ext) (* then branch *) *
              (stat ext option) (* optional else *)
  (* while loop *)
  | AST_while of (expr ext) (* condition *) *
                 (stat ext) (* body *)
  (* exits the program *)
  | AST_HALT
  (* assertion: fail if the boolean expression does not hold *)
  | AST_assert of expr ext
  (* prints the value of some variables *)
  | AST_print of (lvalue ext) list


(* top-level statements *)
type toplevel =
  (* statement to execute *)
  | AST_stat of stat ext

(* a program is a list of top-level statements *)
type prog = toplevel list ext
