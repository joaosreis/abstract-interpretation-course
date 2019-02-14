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

module NonRelational(V : Value_domain.VALUE_DOMAIN) = (struct
  module Map = Mapext.Make(struct
      type t = id
      let compare = compare
    end)

  type env = V.t Map.t

  type t = Env of env | Bot

  let init l =
    Env (List.fold_left (fun acc var -> Map.add var.var_name V.bottom acc) Map.empty l)

  let bottom = Bot

  let is_bottom = function Bot -> true | _ -> false

  let strict f = function
      Bot -> Bot
    | Env x -> f x

  let rec eval_int env =
    function
      AST_int_identifier (v, _) -> Map.find v env
    | AST_int_const (x, _) -> V.const (Z.of_string x)
    | AST_int_rand ((a, _), (b, _)) -> V.rand (Z.of_string a) (Z.of_string b)
    | AST_int_unary (op, (e, _)) -> V.unary (eval_int env e) op
    | AST_int_binary (op, (e_1, _), (e_2, _)) ->
      let eval_e1 = eval_int env e_1 in
      let eval_e2 = eval_int env e_2 in
      V.binary eval_e1 eval_e2 op
    | AST_expr_call ((_, _), _) -> assert false (* TODO: complete *)

  let rec eval_int_cfg env =
    function
      CFG_int_var v -> Map.find v.var_name env
    | CFG_int_const x -> V.const x
    | CFG_int_rand (a, b) -> V.rand a b
    | CFG_int_unary (op, e) -> V.unary (eval_int_cfg env e) op
    | CFG_int_binary (op, e_1, e_2) ->
      let eval_e1 = eval_int_cfg env e_1 in
      let eval_e2 = eval_int_cfg env e_2 in
      V.binary eval_e1 eval_e2 op

  let assign t var expr =
    strict (fun env ->
        let eval_expr = eval_int_cfg env expr in
        if V.is_bottom eval_expr then
          Bot
        else
          Env (Map.add var.var_name eval_expr env)) t

  let rec preprocess = function
      CFG_bool_unary (AST_NOT, c) -> (match c with
          CFG_bool_binary (AST_OR, c_1, c_2) ->
          let c_1' = preprocess c_1 in
          let c_2' = preprocess c_2 in
          let a = CFG_bool_unary (AST_NOT, c_1') in
          let b = CFG_bool_unary (AST_NOT, c_2') in          
          CFG_bool_binary (AST_AND, a, b)
        | CFG_bool_binary (AST_AND, c_1, c_2) ->
          let c_1' = preprocess c_1 in
          let c_2' = preprocess c_2 in
          let a = CFG_bool_unary (AST_NOT, c_1') in
          let b = CFG_bool_unary (AST_NOT, c_2') in
          CFG_bool_binary (AST_OR, a, b)
        | CFG_compare (AST_LESS_EQUAL, e_1, e_2) ->
          CFG_compare (AST_LESS_EQUAL, e_1, e_2)
        | _ -> preprocess c)
    | CFG_compare (AST_LESS, e_1, e_2)
    | CFG_compare (AST_GREATER, e_2, e_1) ->
      let e_2' =
        CFG_int_binary (AST_MINUS, e_2, (CFG_int_const Z.one)) in
      CFG_compare (AST_LESS_EQUAL, e_1, e_2')
    | CFG_compare (AST_GREATER_EQUAL, e_1, e_2) ->
      CFG_compare (AST_LESS_EQUAL, e_2, e_1)
    | CFG_compare (AST_EQUAL, e_1, e_2) ->
      let e_1' = CFG_compare (AST_LESS_EQUAL, e_1, e_2) in
      let e_2' = CFG_compare (AST_LESS_EQUAL, e_2, e_1) in
      CFG_bool_binary (AST_AND, e_1', e_2')
    | CFG_compare (AST_NOT_EQUAL, e_1, e_2) ->
      let a = CFG_int_binary (AST_MINUS, e_2, (CFG_int_const Z.one)) in
      let a' = CFG_compare (AST_LESS_EQUAL, e_1, a) in
      let b = CFG_int_binary (AST_MINUS, e_1, (CFG_int_const Z.one)) in
      let b' = CFG_compare (AST_LESS_EQUAL, e_2, b) in
      CFG_bool_binary (AST_OR, a', b')
    | c -> c

  let join a b = match a, b with
      Bot, x | x, Bot -> x
    | Env m, Env n -> Env (Map.map2z (fun _ x y -> V.join x y) m n)

  let meet a b = match a, b with
      Bot, x | x, Bot -> x
    | Env m, Env n -> Env (Map.map2z (fun _ x y -> V.meet x y) m n)

  (* FIXME: not sure if correct *)
  let widen a b = match a, b with
      Bot, x | x, Bot -> x
    | Env m, Env n ->
      Env (Map.map2z (fun _ x y -> V.widen x y) m n)

  (* FIXME: not sure if correct *)
  let subset a b = match a, b with
      Bot, _ -> true | _, Bot -> false
    | Env m, Env n ->
      Map.fold2z (fun _ x y acc -> acc && V.subset x y) m n true

  type tree = Unary of int_unary_op * g_tree
            | Binary of int_binary_op * g_tree * g_tree
            | Value
            | Var of id

  and g_tree = V.t * tree

  let rec annotated_tree env expr =
    eval_int_cfg env expr, match expr with
      CFG_int_unary (op, e) ->
      Unary (op, annotated_tree env e)
    | CFG_int_binary (op, e_1, e_2) ->
      Binary (op, annotated_tree env e_1, annotated_tree env e_2)
    | CFG_int_var var -> Var (var.var_name)
    | CFG_int_const _ | CFG_int_rand _ -> Value

  let rec refine (v, t) = v, match t with
      Unary (op, (v_1, t_1)) ->
      let v_1' = V.bwd_unary v_1 op v in
      let t_1' = refine (v_1', t_1) in
      Unary (op, t_1')
    | Binary (op, (v_1, t_1), (v_2, t_2)) ->
      let v_1', v_2' = V.bwd_binary v_1 v_2 op v in
      let t_1' = refine (v_1', t_1) in
      let t_2' = refine (v_2', t_2) in
      Binary (op, t_1', t_2')
    | Value | Var _ -> t

  let rec parse_tree env = function
      _, Unary (_, t) -> parse_tree env t
    | _, Binary (_, t_1, t_2) -> parse_tree (parse_tree env t_1) t_2
    | _, Value -> env
    | v, Var id -> Map.add id v env

  let guard t expr = strict (fun env ->
      let rec aux e = match preprocess e with
          CFG_bool_const false -> Bot
        | CFG_bool_const true -> t
        | CFG_bool_binary (AST_OR, c_1, c_2) ->
          join (aux c_1) (aux c_2)
        | CFG_bool_binary (AST_AND, c_1, c_2) ->
          meet (aux c_1) (aux c_2)
        | CFG_compare (AST_LESS_EQUAL, e_1, e_2) ->
          (match e_1, e_2 with
             CFG_int_var var, CFG_int_const c ->
             let v, _ = V.compare (Map.find var.var_name env) (V.const c)
                 AST_LESS_EQUAL in
             Env (Map.add var.var_name v env)
           | CFG_int_var var_1, CFG_int_var var_2 ->
             let v, w = V.compare (Map.find var_1.var_name env)
                 (Map.find var_2.var_name env) AST_LESS_EQUAL in
             Env (Map.add var_1.var_name v (Map.add var_2.var_name w env))
           | _ ->
             let eval_e1 = eval_int_cfg env e_1 in
             let eval_e2 = eval_int_cfg env e_2 in
             let r_1, r_2 = V.compare eval_e1 eval_e2 AST_LESS_EQUAL in
             let root_1 = V.meet eval_e1 r_1 in
             let root_2 = V.meet eval_e2 r_2 in
             let _, tree_1 = annotated_tree env e_1 in
             let _, tree_2 = annotated_tree env e_2 in
             let refined_1 = refine (root_1, tree_1) in
             let refined_2 = refine (root_2, tree_2) in
             let env_1 = parse_tree env refined_1 in
             let env_2 = parse_tree env refined_2 in
             meet (Env env_1) (Env env_2))
        | CFG_bool_rand -> assert false (* TODO: complete *)
        | CFG_bool_unary _ | CFG_compare _ -> assert false
      in aux expr) t

  let print out =
    let open Printf in
    function
      Bot -> fprintf out "bot"
    | Env env ->
      fprintf out "{ ";
      Map.iter (fun id v ->
          fprintf out "%s: " id;
          V.print out v;
          fprintf out "; ") env;
      fprintf out "}"
end(* : DOMAIN*))
