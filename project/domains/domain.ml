(*
  Cours "Sémantique et Application à la Vérification de programmes"

  Antoine Miné 2015
  Marc Chevalier 2018
  Ecole normale supérieure, Paris, France / CNRS / INRIA
*)

(*
  Signature of abstract domains representing sets of envrionments
  (for instance: a map from variable to their bounds).
 *)

open Abstract_syntax_tree
open! Cfg

module type DOMAIN =
sig

  (* type of abstract elements *)
  (* an element of type t abstracts a set of mappings from variables
     to integers
  *)
  type t

  (* initial environment, with all variables initialized to 0 *)
  val init: var list -> t

  (* empty set of environments *)
  val bottom: t

  (* assign an integer expression to a variable *)
  val assign: t -> var -> int_expr -> t

  (* filter environments to keep only those satisfying the boolean expression *)
  val guard: t -> bool_expr -> t

  (* abstract join *)
  val join: t -> t -> t

  (* widening *)
  val widen: t -> t -> t

  (* whether an abstract element is included in another one *)
  val subset: t -> t -> bool

  (* whether the abstract element represents the empty set *)
  val is_bottom: t -> bool

  (* prints *)
  val print: out_channel -> t -> unit

end

module NonRelational(V : Value_domain.VALUE_DOMAIN) = struct
  module Map = Mapext.Make(struct
      type t = id
      let compare = compare
    end)

  type env = V.t Map.t

  type t = Env of env | Bot

  let init l =
    Env (List.fold_left (fun acc id -> Map.add id V.bottom acc) Map.empty l)

  let bottom = Bot

  let strict f = function
      Bot -> Bot
    | Env x -> f x

  let rec eval_int env = function
      AST_int_identifier (v, _) -> Map.find v env
    | AST_int_const (x, _) -> V.const (Z.of_string x)
    | AST_int_rand ((a, _), (b, _)) -> V.rand (Z.of_string a) (Z.of_string b)
    | AST_int_unary (op, (e, _)) -> V.unary (eval_int env e) op
    | AST_int_binary (op, (e_1, _), (e_2, _)) ->
      let eval_e1 = eval_int env e_1 in
      let eval_e2 = eval_int env e_2 in
      V.binary eval_e1 eval_e2 op
    | AST_expr_call ((_, _), _) -> assert false (* TODO: complete *)

  let assign t id expr =
    strict (fun env ->
        let eval_expr = eval_int env expr in
        if V.is_bottom eval_expr then
          Bot
        else
          Env (Map.add id.var_name eval_expr env)) t

  let rec preprocess = function
      AST_bool_unary (AST_NOT, (c, _)) -> (match c with
          AST_bool_binary (AST_OR, (c_1, e_1), (c_2, e_2)) ->
          let c_1' = AST_bool_unary (AST_NOT, (c_1, e_1)) in
          let c_2' = AST_bool_unary (AST_NOT, (c_2, e_2)) in
          AST_bool_binary (AST_AND, (c_1', e_1), (c_2', e_2))
        | AST_bool_binary (AST_AND, (c_1, e_1), (c_2, e_2)) ->
          let c_1' = AST_bool_unary (AST_NOT, (c_1, e_1)) in
          let c_2' = AST_bool_unary (AST_NOT, (c_2, e_2)) in
          AST_bool_binary (AST_OR, (c_1', e_1), (c_2', e_2))
        | AST_compare (AST_LESS_EQUAL, e_1, e_2) ->
          AST_compare (AST_LESS_EQUAL, e_1, e_2)
        | _ -> c)
    | AST_compare (AST_LESS, e_1, (e_2_expr, e_2_ext))
    | AST_compare (AST_GREATER, (e_2_expr, e_2_ext), e_1) ->
      let e_2_expr' =
        AST_int_binary (AST_MINUS, (e_2_expr, e_2_ext),
                        (AST_int_const ("1", extent_unknown), extent_unknown)) in
      AST_compare (AST_LESS_EQUAL, e_1, (e_2_expr', e_2_ext))
    | AST_compare (AST_GREATER_EQUAL, e_1, e_2) ->
      AST_compare (AST_LESS_EQUAL, e_2, e_1)
    | AST_compare (AST_EQUAL, e_1, e_2) ->
      let e_1' = AST_compare (AST_LESS_EQUAL, e_1, e_2), extent_unknown in
      let e_2' = AST_compare (AST_LESS_EQUAL, e_2, e_1), extent_unknown in
      AST_bool_binary (AST_AND, e_1', e_2')
    | AST_compare (AST_NOT_EQUAL, e_1, e_2) ->
      let e_1' =
        AST_compare (AST_LESS_EQUAL, e_1,
                     (AST_int_binary (AST_MINUS,
                                      (AST_int_const ("1", extent_unknown), extent_unknown),
                                      e_2)), extent_unknown), extent_unknown in
      let e_2' = AST_compare (AST_LESS_EQUAL, e_2, e_1), extent_unknown in


      let rec eval_bool env = function
          AST_bool_const x -> x
        | AST_bool_rand -> assert false (* TODO: complete *)
        | AST_bool_unary (AST_NOT, (c, _)) -> (match c with
              AST_bool_binary (AST_OR, (c_1, e_1), (c_2, e_2)) ->
              AST_bool_binary (AST_AND, (AST_bool_unary (c), e_1), (, e_2))
          )
        | AST_compare (op, (e, _)) -> V.unary (eval_int env e) op
        | AST_compare (op, (e_1, _), (e_2, _)) ->
          let eval_e1 = eval_int env e_1 in
          let eval_e2 = eval_int env e_2 in
          V.compare eval_e1 eval_e2 op
  let eval_e1 = eval_int env e_1 in
  let eval_e2 = eval_int env e_2 in
  V.binary eval_e1 eval_e2 op
| AST_expr_call ((_, _), _) -> assert false (* TODO: complete *)

  (* filter environments to keep only those satisfying the boolean expression
     val guard: t -> bool_expr -> t*)
  let guard t expr =


end
