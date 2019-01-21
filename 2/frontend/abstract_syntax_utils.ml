(*
  Cours "Semantics and applications to verification"

  Marc Chevalier 2018
  Ecole normale supérieure, Paris, France / CNRS / INRIA
*)

open Abstract_syntax_tree

module StringMap = Map.Make(struct type t = string let compare = compare end)

type env =
  {
    env_var: variable StringMap.t;
  }

let empty_env =
  {
    env_var = StringMap.empty;
  }

let make_dummy_variable name =
  {
    var_uid = -1;
    var_name = name;
  }

(* All this post processing compute the scoping of variables. *)
let rec post_process_lvalue (lvalue: lvalue) (env: env) : lvalue*env =
  StringMap.find lvalue.var_name env.env_var, env

and post_process_lvalue_ext ((lvalue, ext): lvalue ext) (env: env) : lvalue ext*env =
  let lvalue, env = post_process_lvalue lvalue env in
  (lvalue, ext), env

and post_process_expr (expr: expr) (env: env) : expr*env =
  match expr with
  | AST_unary(op, expr) -> let expr, env = post_process_expr_ext expr env in AST_unary(op, expr), env
  | AST_binary(op, lhs, rhs) ->
    let lhs, env = post_process_expr_ext lhs env in
    let rhs, env = post_process_expr_ext rhs env in
    AST_binary(op, lhs, rhs), env
  | AST_variable (var, ext) -> AST_variable (StringMap.find var.var_name env.env_var, ext), env
  | AST_int_const n -> AST_int_const n, env
  | AST_bool_const b -> AST_bool_const b, env
  | AST_int_rand(l, h) -> AST_int_rand(l, h), env

and post_process_expr_ext ((expr, ext): expr ext) (env: env) : expr ext*env =
  let expr, env = post_process_expr expr env in
  (expr, ext), env

and post_process_var_init (((var, ext), init): var_init) (env: env) : var_init*env =
  let init, env =
    match Option.map (Fn.flip post_process_expr_ext env) init with
    | None -> None, env
    | Some(init, env) -> Some init, env
  in
  let var =
    {
      var_uid = UniqueId.get ();
      var_name = var.var_name;
    }
  in
  let env = { env_var = StringMap.add var.var_name var env.env_var } in
  ((var, ext), init), env

(* Useful if there is more in env *)
and restore_env (old_env: env) (_new_env: env) : env =
  old_env

and post_process_stat (stat: stat) (env: env) : stat*env =
  match stat with
  | AST_block l ->
    let old_env = env in
    let l, new_env =
      List.fold_left (fun (acc, env) stat -> let stat, env = post_process_stat_ext stat env in (stat::acc, env)) ([], old_env) l
    in
    AST_block (List.rev l), restore_env old_env new_env
  | AST_assign(lvalue, expr) ->
    let lvalue, env = post_process_lvalue_ext lvalue env in
    let expr, env = post_process_expr_ext expr env in
    AST_assign(lvalue, expr), env
  | AST_if(cond, body1, body2) ->
    let cond, env = post_process_expr_ext cond env in
    let body1, env = post_process_stat_ext body1 env in
    let body2, env = match Option.map (Fn.flip post_process_stat_ext env) body2 with
      | None -> body2, env
      | Some (body2, env) -> Some body2, env
    in
    AST_if(cond, body1, body2), env
  | AST_while(cond, body) ->
    let cond, env = post_process_expr_ext cond env in
    let body, env = post_process_stat_ext body env in
    AST_while(cond, body), env
  | AST_HALT -> AST_HALT, env
  | AST_assert(expr) -> let expr, env = post_process_expr_ext expr env in AST_assert expr, env
  | AST_print l ->
    let l, env =
      List.fold_left (fun (acc, env) stat -> let stat, env = post_process_lvalue_ext stat env in (stat::acc, env)) ([], env) l
    in
    AST_print (List.rev l), env
  | AST_local l ->
    let l, env =
      List.fold_left (fun (acc, env) stat -> let stat, env = post_process_var_init stat env in (stat::acc, env)) ([], env) l
    in
    AST_local (List.rev l), env

and post_process_stat_ext ((stat, ext): stat ext) (env: env) : stat ext*env =
  let stat, env = post_process_stat stat env in
  (stat, ext), env

and post_process_toplevel (AST_stat stat: toplevel) (env: env) : toplevel*env =
  let stat, env = post_process_stat_ext stat env in
  AST_stat stat, env

and post_process_prog ((prog, ext): prog) (env: env) : prog*env =
  let prog, env =
    List.fold_left (fun (acc, env) top -> let stat, env = post_process_toplevel top env in (stat::acc, env)) ([], env) prog
  in
  (List.rev prog, ext), env

let post_process_program =
  Fn.compose fst(Fn.flip post_process_prog empty_env)