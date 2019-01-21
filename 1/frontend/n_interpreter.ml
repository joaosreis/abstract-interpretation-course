open Interpreter

module Ival_err_set = Set.Make(struct
    type t = Interpreter.ival_err
    let compare = compare
  end)

type ival_err_set = Ival_err_set.t

module Env_err_set = Set.Make(struct
    type t = Interpreter.env_err
    let compare x y =
      match x, y with
        E_correct e_1, E_correct e_2 -> Environment.compare compare e_1 e_2
      | E_error, E_error -> 0
      | _ -> -1
  end)

type env_err_set = Env_err_set.t

let create_empty_env_err_set () =
  Env_err_set.singleton (E_correct Environment.empty)

let create_set l h =
  let rec create_set_rec acc x =
    if x > h then acc
    else
      create_set_rec (Ival_err_set.add (T_correct (I x)) acc) (Z.add x (Z.of_int 1)) in
  create_set_rec Ival_err_set.empty l

let rec eval_u_op env expr u_op =
  let open Abstract_syntax_tree in
  let v = eval_expr env expr in
  Ival_err_set.fold (fun x acc ->
      match x with
        T_error -> Ival_err_set.add T_error acc
      | T_correct v -> Ival_err_set.add (match u_op with
            AST_UNARY_PLUS -> (match v with
                I _ -> T_correct v
              | B _ -> T_error)
          | AST_UNARY_MINUS -> (match v with
                I i -> T_correct (I (Z.neg i))
              | B _ -> T_error)
          | AST_NOT -> (match v with
                B b -> T_correct (B (not b))
              | I _ -> T_error)) acc) v Ival_err_set.empty

and eval_b_op env expr_1 expr_2 b_op =
  let open Abstract_syntax_tree in
  let s_1 = eval_expr env expr_1 in
  let s_2 = eval_expr env expr_2 in
  let f = function
      T_error, _ | _, T_error -> T_error
    | T_correct v_1, T_correct v_2 -> match b_op with
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
            I i1, I i2 -> if i2 = Z.zero then T_error
            else T_correct (I (Z.div i1 i2))
          | B _, _
          | I _, B _ -> T_error)
      | AST_MODULO -> (match v_1, v_2 with
            I i1, I i2 -> if i2 = Z.zero then T_error
            else T_correct (I (Z.(mod) i1 i2))
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
      | AST_OR -> match v_1, v_2 with
          B b1, B b2 -> T_correct (B (b1 || b2))
        | I _, _
        | _, I _ -> T_error in
  Ival_err_set.fold (fun e_1 acc ->
      Ival_err_set.fold (fun e_2 acc ->
          Ival_err_set.add (f (e_1, e_2)) acc) s_2 acc) s_1 Ival_err_set.empty

and eval_expr env (expr, _) =
  let open Abstract_syntax_tree in
  match expr with
    AST_unary (u_op, expr') -> eval_u_op env expr' u_op
  | AST_binary (b_op, expr', expr'') -> eval_b_op env expr' expr'' b_op
  (* variable use *)
  | AST_variable (id, _) ->
    Ival_err_set.singleton (try T_correct (env_find id env)
                            with Not_found -> T_error)
  (* constants (integers are still in their string representation) *)
  | AST_int_const (i, _) -> Ival_err_set.singleton (T_correct (I i))
  | AST_bool_const b -> Ival_err_set.singleton (T_correct (B b))
  (* non-deterministic choice between two integeres *)
  | AST_int_rand ((l, _), (h, _)) -> create_set l h

let filter env_err_set (expr) c =
  Env_err_set.fold (fun env_err acc -> match env_err with
        E_error -> Env_err_set.add env_err acc
      | E_correct env ->
        Ival_err_set.fold (fun v acc -> match v with
              T_error -> Env_err_set.add E_error acc
            | T_correct (B b) -> if b = c then Env_err_set.add env_err acc
              else acc
            | _ -> Env_err_set.add E_error acc)
          (eval_expr env expr) acc) env_err_set Env_err_set.empty

let rec fix f equals e =
  let res = f e in
  if equals res e then res
  else fix f equals res

let eval_assign env_err_set lv rv =
  Env_err_set.fold (fun env_err acc -> match env_err with
        E_error -> Env_err_set.add env_err acc
      | E_correct env ->
        Ival_err_set.fold (fun e acc' -> match e with
              T_error -> Env_err_set.add E_error acc'
            | T_correct v -> Env_err_set.add (E_correct (env_add lv v env)) acc')
          (eval_expr env rv) acc) env_err_set Env_err_set.empty

let print_val fmt = function
    I i -> Z.pp_print fmt i
  | B b -> Format.pp_print_bool fmt b

let print_json env to_string l =
  let open Format in
  let rec aux = function
      [] -> ()
    | (id, _) :: xs ->
      let () = printf "\"%s\":" id in
      let () = try printf "%a" print_val (env_find id env)
        with Not_found -> printf "unknown" in
      match xs with
        [] -> ()
      | _ -> let () = printf "," in aux xs in
  let () = printf "{" in
  let () = aux l in
  printf "}"

let print_json_list to_string ids l =
  let open Format in
  let rec aux = function
      [E_correct x] -> print_json x to_string ids
    | E_correct x :: xs ->
      let () = print_json x to_string ids in
      (match xs with
         [] -> ()
       | _ -> let () = printf ", " in
         aux xs)
    | _ :: xs -> aux xs
    | [] -> () in
  let () = printf "[" in
  let () = aux (List.of_seq (Env_err_set.to_seq l)) in
  printf "]"

let rec eval_stat env_err_set (stmt, pos) =
  let open Abstract_syntax_tree in
  match stmt with
    AST_block stmts -> List.fold_left eval_stat env_err_set stmts
  | AST_assign ((lv, _), rv) -> eval_assign env_err_set lv rv
  | AST_if (cond, t, Some e) ->
    let t_env_err_set = eval_stat (filter env_err_set cond true) t in
    let f_env_err_set = eval_stat (filter env_err_set cond false) e in
    Env_err_set.union t_env_err_set f_env_err_set
  | AST_if (cond, t, None) ->
    eval_stat env_err_set (AST_if (cond, t, Some (AST_block [], pos)), pos)
  | AST_while (cond, body) ->
    let f x = Env_err_set.union env_err_set
        (eval_stat (filter x cond true) body) in
    let inv = fix f Env_err_set.equal Env_err_set.empty in
    let f_inv = filter inv cond false in
    if not (Env_err_set.is_empty f_inv) then
      Env_err_set.add E_error f_inv
    else f_inv
  | AST_assert cond ->
    let t_env_err_set = filter env_err_set cond true in
    let f_env_err_set = filter env_err_set cond false in
    if Env_err_set.exists (function E_correct _ -> true | _ -> false) f_env_err_set then
      Env_err_set.add E_error t_env_err_set
    else t_env_err_set
  | AST_print ids ->
    let open Format in
    let () = print_json_list print_val ids env_err_set in
    env_err_set
  | AST_HALT ->
    let error_env_err_set =
      Env_err_set.filter (function E_error -> true | _ -> false) env_err_set in
    Env_err_set.add E_error error_env_err_set
