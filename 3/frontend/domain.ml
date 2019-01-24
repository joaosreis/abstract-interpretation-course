open Abstract_syntax_tree

module type VALUE_DOMAIN = sig
  type t
  (* constructors *)
  val top: t
  val bottom: t
  val cost: Z.t -> t
  val rand: Z.t -> Z.t -> t
  (* order *)
  val subset: t -> t -> bool
  (* set-theoretic operations *)
  val join: t -> t -> t
  val meet: t -> t -> t
  val widen: t -> t -> t
  (* arithmetic operations *)
  val neg: t -> t
  val add: t -> t -> t
  val sub: t -> t -> t
  val mul: t -> t -> t
  val div: t -> t -> t
  (* boolean test *)
  val leq: t -> t -> t * t
end

module Interval = (struct
  type bound = N_infinity | P_infinity | Int of Z.t

  type t = Bot | Itv of bound * bound

  (* constructors *)
  let top = Itv (N_infinity, P_infinity)

  let bottom = Bot

  let cost x = Itv (Int x, Int x)

  let rand l h = Itv (Int l, Int h)

  (* utilities *)
  type sign = Neg | Pos

  let sign = function
      N_infinity -> Neg
    | P_infinity -> Pos
    | Int x ->
      if x >= Z.zero then Pos
      else Neg

  let bound_cmp a b = match a, b with
      N_infinity, N_infinity | P_infinity, P_infinity -> 0
    | N_infinity, _ | _, P_infinity -> -1
    | P_infinity, _ | _, N_infinity  -> 1
    | Int x, Int y -> Z.compare x y

  let bound_neg = function
      N_infinity -> P_infinity
    | P_infinity -> N_infinity
    | Int x -> Int (Z.neg x)

  let bound_add a b = match a, b with
      P_infinity, _ | _, P_infinity -> P_infinity
    | N_infinity, _ | _, N_infinity -> N_infinity
    | Int x, Int y -> Int (Z.add x y)

  let bound_sub a b = match a, b with
      P_infinity, _ | _, N_infinity -> P_infinity
    | N_infinity, _ | _, P_infinity -> N_infinity
    | Int x, Int y -> Int (Z.add x y)

  let bound_mul a b = match a, b, sign a, sign b with
      P_infinity, _, _, Neg | _, P_infinity, Neg, _ -> N_infinity
    | P_infinity, _, _, Pos | _, P_infinity, Pos, _ -> P_infinity
    | N_infinity, _, _, Neg | _, N_infinity, Neg, _ -> P_infinity
    | N_infinity, _, _, Pos | _, N_infinity, Pos, _ -> N_infinity
    | Int x, Int y, _, _ -> Int (Z.mul x y)

  let bound_div a b = match a, b, sign a, sign b with
      P_infinity, _, _, Neg | _, P_infinity, Neg, _ -> N_infinity
    | P_infinity, _, _, Pos | _, P_infinity, Pos, _ -> P_infinity
    | N_infinity, _, _, Neg | _, N_infinity, Neg, _ -> P_infinity
    | N_infinity, _, _, Pos | _, N_infinity, Pos, _ -> N_infinity
    | Int x, Int y, _, _ -> Int (Z.div x y)

  let bound_min a b = match a, b with
      N_infinity, _ | _, N_infinity -> N_infinity
    | P_infinity, x | x, P_infinity -> x
    | Int x, Int y -> Int (Z.min x y)

  let bound_max a b = match a, b with
      N_infinity, x | x, N_infinity -> x
    | P_infinity, _ | _, P_infinity -> P_infinity
    | Int x, Int y -> Int (Z.max x y)

  let strict f = function
      Bot -> Bot
    | Itv (x, y) -> f x y

  (* domain implementation *)

  (* order *)
  let subset a b = match a, b with
      Bot, _-> true | _, Bot -> false
    | Itv (a, b), Itv (c, d) ->
      bound_cmp a c >= 0 && bound_cmp b d >= 0

  (* set-theoretic operations *)
  let join x y = match x, y with
      Bot, l | l, Bot -> l
    | Itv (a, b), Itv (c, d) -> Itv (bound_min a c, bound_max b d)

  let meet x y = match x, y with
      Bot, _ | _, Bot -> Bot
    | Itv (a, b), Itv (c, d) ->
      if bound_max a c <= bound_min b d then Itv (bound_max a c, bound_min b d)
      else
        Bot

  let widen a b = match a, b with
      x, Bot | Bot, x -> x
    | Itv (a, b), Itv (c, d) ->
      let x =
        if bound_cmp a c <= 0 then a
        else N_infinity in
      let y =
        if bound_cmp b d >= 0 then b
        else P_infinity in
      Itv (x, y)

  (* arithmetic operations *)
  let neg = strict
      (fun a b -> Itv (bound_neg a, bound_neg b))

  let add x y = match x, y with
      Bot, _ | _, Bot -> Bot
    | Itv (a, b), Itv (c, d) -> Itv (bound_add a c, bound_add b d)

  let sub x y = match x, y with
      Bot, _ | _, Bot -> Bot
    | Itv (a, b), Itv (c, d) -> Itv (bound_sub a c, bound_sub b d)

  let mul x y = match x, y with
      Bot, _ | _, Bot -> Bot
    | Itv (a, b), Itv (c, d) ->
      let r1 = bound_mul a c in
      let r2 = bound_mul a d in
      let r3 = bound_mul b c in
      let r4 = bound_mul b d in
      Itv (bound_min r1 (bound_min r2 (bound_min r3 r4)), bound_max r1 (bound_max r2 (bound_max r3 r4)))

  let div x y = match x, y with
      Bot, _ | _, Bot -> Bot
    | Itv (a, b), Itv (c, d) ->
      let ac = bound_div a c in
      let ad = bound_div a d in
      let bc = bound_div b c in
      let bd = bound_div b d in
      if bound_cmp c (Int Z.one) >= 0 then
        Itv (min ac ad, max bc bd)
      else if bound_cmp d (Int (Z.neg Z.one)) <= 0 then
        Itv (min bc bd, max ac ad)
      else
        Bot

  (* boolean test *)
  let leq a b = match a, b with
      Bot, _ | _, Bot -> Bot, Bot
    | Itv (a, b), Itv (c, d) ->
      Itv (a, bound_min b d), Itv (bound_max a c, d)
end : VALUE_DOMAIN)

module type ENVIRONMENT_DOMAIN = sig
  type t
  (* constructors *)
  val init: id list -> t
  (* abstract operators *)
  val assign: t -> id -> expr -> t
  val compare: t -> expr -> expr -> t
  (* set-theoretic operations *)
  val join: t -> t -> t
  val meet: t -> t -> t
  val widen: t -> t -> t
  val subset: t -> t -> bool
end

module NonRelational(V : VALUE_DOMAIN) = (struct
  module Map = Mapext.Make(struct
      type t = id let compare = compare
    end)

  type env = V.t Map.t

  type t = Env of env | Bot

  let init = List.fold_left (fun acc x -> Map.add x V.bottom acc) Map.empty

  let assign env id e = 

    let compare a b = match a, b with
        Bot, _ -> -1 | _, Bot -> 1
      | Env m, Env n -> Map.fold2 (fun _ x y acc -> if acc <> 0 then acc else compare x y) m n 0

  (* utilities *)
  let rec eval env = function
      AST_variable (v, _) -> Map.find v.var_name env
    | AST_int_const x -> V.cost x
    | AST_int_rand ((a, _), (b, _)) -> V.rand a b
    | AST_unary (AST_UNARY_MINUS, (e, _)) -> V.neg (eval env e)
    | AST_binary (o, (e_1, _), (e_2, _)) ->
      let eval_e1 = eval env e_1 in
      let eval_e2 = eval env e_2 in
      (match o with
         AST_PLUS -> V.add eval_e1 eval_e2
       | AST_MINUS -> V.sub eval_e1 eval_e2
       | AST_MULTIPLY -> V.mul eval_e1 eval_e2
       | AST_DIVIDE -> V.div eval_e1 eval_e2
       | _ -> failwith "unsupported operation")
    | _ -> failwith "unsupported construct"

  let is_bot = Map.is_empty

  let strict f = function
      Bot -> Bot
    | Env env -> f env

  (* operators *)
  let join a b = match a, b with
      Bot, x | x, Bot -> x
    | Env m, Env n -> Env (Map.map2z (fun _ x y -> V.join x y) m n)

  let meet a b = match a, b with
      Bot, x | x, Bot -> x
    | Env m, Env n -> Env (Map.map2z (fun _ x y -> V.meet x y) m n)

  let widen a b = match a, b with
      Bot, x | x, Bot -> x
    | Env m, Env n -> Env (Map.map2z (fun _ x y -> V.widen x y) m n)

  let subset a b = match a, b with
      Bot, _ -> true | _, Bot -> false
    | Env m, Env n -> Map.for_all2z (fun _ x y -> V.subset x y) m n

end : ENVIRONMENT_DOMAIN)
