type aval = Top | I | B | Bot | All

let print_aval fmt =
  let f = Format.pp_print_string fmt in
  function
    Top -> f "type error"
  | I -> f "int"
  | B -> f "bool"
  | Bot -> f "not initialized"
  | All -> f "no information"

module Env = Mapext.Make(struct
    type t = Abstract_syntax_tree.variable
    let compare x y =
      compare x.Abstract_syntax_tree.var_uid y.Abstract_syntax_tree.var_uid
  end)

type env = aval Env.t

type env_lattice = E_bot | E_top | Env of env

exception Type_error

exception Type_error_ext of Abstract_syntax_tree.extent * aval * aval

let error_message = function
    Type_error_ext ((s, e), expected, actual) ->
    let open Lexing in
    Format.printf "On lines %d-%d, characters %d-%d, type error: this \
                   expression has type %a but an expression was expected of \
                   type %a.\n" s.pos_lnum e.pos_lnum s.pos_cnum e.pos_cnum
      print_aval expected print_aval actual
  | _ -> Format.printf "Error.\n"

let aval_join x y =
  match x, y with
    I, I -> I
  | B, B -> B
  | I, B | B, I -> All
  | Bot, x | x, Bot -> x
  | _ -> Top

let env_join x y = match x, y with
    Env e_1, Env e_2 -> Env (Env.map2 (fun _ -> aval_join) e_1 e_2)
  | E_top, _ | _, E_top -> E_top
  | E_bot, e | e, E_bot -> e

let env_equal x y = match x, y with
    E_bot, E_bot | E_top, E_top -> true
  | Env e_1, Env e_2 when (Env.equal (=) e_1 e_2) -> true
  | _ -> false

let eval_u_op o e_t =
  let open Abstract_syntax_tree in
  match o, e_t with
    AST_UNARY_PLUS, I
  | AST_UNARY_MINUS, I -> I
  | AST_NOT, B -> B
  | _, Bot -> Bot
  | _ -> raise Type_error

let eval_b_op o e1_t e2_t =
  let open Abstract_syntax_tree in
  match o, e1_t, e2_t with
    AST_PLUS, I, I
  | AST_MINUS, I, I
  | AST_MULTIPLY, I, I
  | AST_DIVIDE, I, I
  | AST_MODULO, I, I -> I
  | AST_EQUAL, I, I
  | AST_EQUAL, B, B
  | AST_NOT_EQUAL, I, I
  | AST_NOT_EQUAL, B, B -> B
  | AST_LESS, I, I
  | AST_LESS_EQUAL, I, I
  | AST_GREATER, I, I
  | AST_GREATER_EQUAL, I, I -> B
  | AST_AND, B, B
  | AST_OR, B, B -> B
  | _, Bot, _ | _, _, Bot -> Bot
  | _ -> Top


let rec eval_expr env (expr, _) =
  let open Abstract_syntax_tree in
  match expr with
    AST_unary (u_op, e) -> eval_u_op u_op (eval_expr env e)
  | AST_binary (b_op, e_1, e_2) ->
    eval_b_op b_op (eval_expr env e_1) (eval_expr env e_2)
  | AST_variable (id, _) -> Env.find id env
  | AST_int_const _ -> I
  | AST_bool_const _ -> B
  | AST_int_rand _ -> I

let rec eval_stat env_l (stmt, pos) =
  let open Abstract_syntax_tree in
  match env_l with
    E_top -> E_top
  | E_bot -> E_bot
  | Env env -> match stmt with
      AST_block b -> List.fold_left eval_stat env_l b
    | AST_assign ((lv, _), rv) -> (match eval_expr env rv with
          Top -> E_top
        | Bot -> E_bot
        | _ as t -> Env (Env.add lv t env))
    | AST_if (cond, ib, Some eb) -> (match eval_expr env cond with
          B -> env_join (eval_stat env_l ib) (eval_stat env_l eb)
        | Bot -> E_bot
        | _ -> E_top)
    | AST_if (cond, body, None) -> (match eval_expr env cond with
          B -> env_join env_l (eval_stat env_l body)
        | Bot -> E_bot
        | _ -> E_top)
    | AST_while (cond, body) ->
      let rec fix f x =
        let res = f x in
        if env_equal res x then res
        else fix f res in
      let f x = env_join env_l (eval_stat x body) in
      let inv = fix f E_bot in
      (match inv with
         E_top -> E_top
       | E_bot -> E_bot
       | Env env -> match eval_expr env cond with
           Bot -> E_bot
         | B -> inv
         | _ -> E_top)
    | AST_HALT -> env_l
    | AST_assert cond -> (match eval_expr env cond with
          B -> env_l
        | Bot -> E_bot
        | _ -> E_top)
    | AST_print ids ->
      List.iter (fun id -> let _ = eval_expr env (AST_variable id, pos) in ()) ids;
      env_l
    | AST_local inits ->
      Env (List.fold_left
             (fun acc ((var, _), init) -> match init with
                  Some e -> Env.add var (eval_expr acc e) acc
                | None -> Env.add var Bot acc) env inits)
