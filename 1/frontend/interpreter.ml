type ival = I of Z.t | B of bool

type ival_err = T_correct of ival | T_error

module Environment = Map.Make(String)

type env = ival Environment.t

let env_find = Environment.find

let env_add = Environment.add

type env_err = E_correct of ival Environment.t | E_error

let compare_env_err x y = match x, y with
    Some (E_correct e_1), E_correct e_2 -> Environment.equal (=) e_1 e_2
  | Some (E_error), E_error -> true
  | _ -> false

let create_empty_env_err () = E_correct (Environment.empty)

type print_result = P_val of string | P_error

let print_ids env ids =
  let open Printf in
  let rec print_ids_rec acc l = match acc with
      P_error -> acc
    | P_val s -> match l with
        [] -> acc
      | (id, _) :: t ->
        let fmt = format_of_string (match t with
              [] -> "%s\"%s\": %s"
            |  _ ->  "%s\"%s\": %s,") in         
        try print_ids_rec (P_val (sprintf fmt s id (match Environment.find id env with
              I i -> Z.to_string i
            | B b -> string_of_bool b))) t
        with Not_found ->
          P_error in
  match print_ids_rec (P_val "") ids with
    P_val s -> P_val (sprintf "{%s}" s)
  | _ as r -> r

let rec eval_u_op env expr =
  let open Abstract_syntax_tree in
  match eval_expr env expr with
    T_error -> fun _ -> T_error
  | T_correct v -> function
      AST_UNARY_PLUS -> (match v with
          I _ -> T_correct v
        | B _ -> T_error)
    | AST_UNARY_MINUS -> (match v with
          I i -> T_correct (I (Z.neg i))
        | B _ -> T_error)
    | AST_NOT -> (match v with
          B b -> T_correct (B (not b))
        | I _ -> T_error)

and eval_b_op env expr_1 expr_2 =
  let open Abstract_syntax_tree in
  match eval_expr env expr_1, eval_expr env expr_2 with
    T_error, _ | _, T_error -> fun _ -> T_error
  | T_correct v_1, T_correct v_2 ->
    function
      AST_PLUS -> (match v_1, v_2 with
          I i1, I i2 -> T_correct (I (Z.add i1 i2))
        | B _, _
        | I _, B _ -> T_error)
    | AST_MINUS -> (match v_1, v_2 with
          I i1, I i2 -> T_correct (I (Z.sub i1 i2))
        | B _, _
        | I _, B _ -> T_error)
    | AST_MULTIPLY -> (match v_1, v_2 with
          I i1, I i2 -> T_correct (I (Z.mul i1 i2))
        | B _, _
        | I _, B _ -> T_error)
    | AST_DIVIDE -> (match v_1, v_2 with
          I _, I i2 when i2 = Z.zero -> T_error
        | I i1, I i2 -> T_correct (I (Z.div i1 i2))
        | B _, _
        | I _, B _ -> T_error)
    | AST_MODULO -> (match v_1, v_2 with
          I _, I i2 when i2 = Z.zero -> T_error
        | I i1, I i2 -> T_correct (I (Z.(mod) i1 i2))
        | B _, _
        | I _, B _ -> T_error)
    (* polymorphic comparison, should work for int and bool *)
    | AST_EQUAL -> (match v_1, v_2 with
          I i1, I i2 -> T_correct (B (Z.equal i1 i2))
        | B b1, B b2 -> T_correct (B (b1 = b2))
        | I _, B _
        | B _, I _ -> T_error)
    | AST_NOT_EQUAL -> (match v_1, v_2 with
          I i1, I i2 -> T_correct (B (not (Z.equal i1 i2)))
        | B b1, B b2 -> T_correct (B (not (b1 = b2)))
        | I _, B _
        | B _, I _ -> T_error)
    | AST_LESS -> (match v_1, v_2 with
          I i1, I i2 -> T_correct (B (Z.lt i1 i2))
        | B _, _
        | _, B _ -> T_error)
    | AST_LESS_EQUAL -> (match v_1, v_2 with
          I i1, I i2 -> T_correct (B (Z.leq i1 i2))
        | B _, _
        | _, B _ -> T_error)
    | AST_GREATER -> (match v_1, v_2 with
          I i1, I i2 -> T_correct (B (Z.gt i1 i2))
        | B _, _
        | _, B _ -> T_error)
    | AST_GREATER_EQUAL -> (match v_1, v_2 with
          I i1, I i2 -> T_correct (B (Z.geq i1 i2))
        | B _, _
        | _, B _ -> T_error)
    | AST_AND -> (match v_1, v_2 with
          B b1, B b2 -> T_correct (B (b1 && b2))
        | I _, _
        | _, I _ -> T_error)
    | AST_OR -> (match v_1, v_2 with
          B b1, B b2 -> T_correct (B (b1 || b2))
        | I _, _
        | _, I _ -> T_error)

and eval_expr env (expr, _) =
  let open Abstract_syntax_tree in
  match expr with
    AST_unary (u_op, expr') -> eval_u_op env expr' u_op
  | AST_binary (b_op, expr', expr'') -> eval_b_op env expr' expr'' b_op
  (* variable use *)
  | AST_variable (id, _) ->
    (try T_correct (Environment.find id env)
     with Not_found -> T_error)
  (* constants (integers are still in their string representation) *)
  | AST_int_const (i, _) -> T_correct (I i)
  | AST_bool_const b -> T_correct (B b)
  (* non-deterministic choice between two integeres *)
  | AST_int_rand _ -> assert false

let rec eval_stat env_err (stmt, pos) =
  match env_err with
    E_error -> env_err
  | E_correct env ->
    let open Abstract_syntax_tree in
    match stmt with
      AST_block stmts -> List.fold_left eval_stat env_err stmts
    | AST_assign ((lv, _), rv) ->
      (match eval_expr env rv with
         T_correct v -> E_correct (Environment.add lv v env)
       | T_error -> E_error)
    | AST_if (cond, t, e) ->
      (match eval_expr env cond, e with
         T_correct (B b), _ when b -> eval_stat env_err t
       | T_correct (B b), Some es when not b -> eval_stat env_err es
       | T_correct (B _), None -> env_err
       | T_correct _, _
       | T_error, _ -> E_error)
    | AST_while (cond, body) ->
      (match eval_expr env cond with
         T_correct (B b) -> if b then
           let (_, res) = List.fold_left (fun (prev, act) s ->
               if (compare_env_err prev act) then (prev, E_error)
               else (Some act, eval_stat act s)) (None, env_err) ([body;(stmt, pos)]) in
           res
         else env_err
       | T_correct _
       | T_error -> E_error)
    | AST_HALT -> E_error
    (* assertion: fail if the boolean expression does not hold *)
    | AST_assert cond -> (match eval_expr env cond with
          T_correct (B b) -> if b then env_err
          else E_error
        | T_correct _
        | T_error -> E_error)
    (* prints the value of some variables *)
    | AST_print ids -> match print_ids env ids with
        P_val s -> let () = print_endline s in env_err
      | P_error -> E_error
