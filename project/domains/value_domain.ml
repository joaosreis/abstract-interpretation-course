(*
  Cours "Sémantique et Application à la Vérification de programmes"

  Antoine Miné 2015
  Marc Chevalier 2018
  Ecole normale supérieure, Paris, France / CNRS / INRIA
*)

(*
  Signature of abstract domains representing sets of integers
  (for instance: constants or intervals).
 *)

open Abstract_syntax_tree

module type VALUE_DOMAIN =
sig

  (* type of abstract elements *)
  (* an element of type t abstracts a set of integers *)
  type t

  (* unrestricted value: [-oo,+oo] *)
  val top: t

  (* bottom value: empty set *)
  val bottom: t

  (* constant: {c} *)
  val const: Z.t -> t

  (* interval: [a,b] *)
  val rand: Z.t -> Z.t -> t


  (* unary operation *)
  val unary: t -> int_unary_op -> t

  (* binary operation *)
  val binary: t -> t -> int_binary_op -> t


  (* comparison *)
  (* [compare x y op] returns (x',y') where
     - x' abstracts the set of v  in x such that v op v' is true for some v' in y
     - y' abstracts the set of v' in y such that v op v' is true for some v  in x
       i.e., we filter the abstract values x and y knowing that the test is true

     a safe, but not precise implementation, would be:
     compare x y op = (x,y)
  *)
  val compare: t -> t -> compare_op -> (t * t)


  (* backards unary operation *)
  (* [bwd_unary x op r] return x':
     - x' abstracts the set of v in x such as op v is in r
       i.e., we fiter the abstract values x knowing the result r of applying
       the operation on x
  *)
  val bwd_unary: t -> int_unary_op -> t -> t

  (* backward binary operation *)
  (* [bwd_binary x y op r] returns (x',y') where
     - x' abstracts the set of v  in x such that v op v' is in r for some v' in y
     - y' abstracts the set of v' in y such that v op v' is in r for some v  in x
       i.e., we filter the abstract values x and y knowing that, after
       applying the operation op, the result is in r
  *)
  val bwd_binary: t -> t -> int_binary_op -> t -> (t * t)


  (* set-theoretic operations *)
  val join: t -> t -> t
  val meet: t -> t -> t

  (* widening *)
  val widen: t -> t -> t

  (* subset inclusion of concretizations *)
  val subset: t -> t -> bool

  (* check the emptiness of the concretization *)
  val is_bottom: t -> bool

  (* print abstract element *)
  val print: out_channel -> t -> unit

end

module Interval = (struct
  type bound = N_infinity | P_infinity | Int of Z.t

  type t = Bot | Itv of bound * bound

  (* constructors *)
  let top = Itv (N_infinity, P_infinity)

  let bottom = Bot

  let const x = Itv (Int x, Int x)

  let rand l h = Itv (Int l, Int h)

  let bound_neg = function
      N_infinity -> P_infinity
    | P_infinity -> N_infinity
    | Int x -> Int (Z.neg x)

  let unary itv op = match itv, op with
      Bot, _ -> Bot
    | _, AST_UNARY_PLUS -> itv
    | Itv (x, y), AST_UNARY_MINUS -> Itv (bound_neg x, bound_neg y)

  type sign = Neg | Pos

  let sign = function
      N_infinity -> Neg
    | P_infinity -> Pos
    | Int x ->
      if x >= Z.zero then Pos
      else Neg

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

  let bound_cmp a b = match a, b with
      N_infinity, N_infinity | P_infinity, P_infinity -> 0
    | N_infinity, _ | _, P_infinity -> -1
    | P_infinity, _ | _, N_infinity  -> 1
    | Int x, Int y -> Z.compare x y

  let bound_min a b = match a, b with
      N_infinity, _ | _, N_infinity -> N_infinity
    | P_infinity, x | x, P_infinity -> x
    | Int x, Int y -> Int (Z.min x y)

  let bound_max a b = match a, b with
      N_infinity, x | x, N_infinity -> x
    | P_infinity, _ | _, P_infinity -> P_infinity
    | Int x, Int y -> Int (Z.max x y)


  let binary x y op = match x, y with
      Bot, _ | _, Bot -> Bot
    | Itv (a, b), Itv (c, d) -> match op with
        AST_PLUS -> Itv (bound_add a c, bound_add b d)
      | AST_MINUS -> Itv (bound_sub a d, bound_sub b c)
      | AST_MULTIPLY ->
        let r1 = bound_mul a c in
        let r2 = bound_mul a d in
        let r3 = bound_mul b c in
        let r4 = bound_mul b d in
        Itv (bound_min r1 (bound_min r2 (bound_min r3 r4)), bound_max r1 (bound_max r2 (bound_max r3 r4)))
      | AST_DIVIDE ->
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
      | AST_MODULO -> assert false (* TODO: complete *)

  let join x y = match x, y with
      Bot, l | l, Bot -> l
    | Itv (a, b), Itv (c, d) -> Itv (bound_min a c, bound_max b d)

  let meet x y = match x, y with
      Bot, _ | _, Bot -> Bot
    | Itv (a, b), Itv (c, d) ->
      if bound_max a c <= bound_min b d then Itv (bound_max a c, bound_min b d)
      else
        Bot

  let bwd_unary x op r =
    match x, r with
      Bot, _ | _, Bot -> Bot
    | Itv (a, b), Itv (r, s) -> match op with
        AST_UNARY_PLUS -> x
      | AST_UNARY_MINUS -> meet x (Itv (bound_neg s, bound_neg r))

  let bwd_binary x y op r =
    match x, y, r with
      Bot, _, _ | _, Bot, _ | _, _, Bot -> Bot, Bot
    | Itv _, Itv _, Itv _ -> match op with
        AST_PLUS ->
        meet x (binary r y AST_MINUS),
        meet y (binary r x AST_MINUS)
      | AST_MINUS ->
        meet x (binary r y AST_PLUS),
        meet y (binary x r AST_MINUS)
      | AST_MULTIPLY ->
        meet x (binary r y AST_DIVIDE),
        meet y (binary r x AST_DIVIDE)
      | AST_DIVIDE ->
        let s = binary r (Itv (Int Z.minus_one, Int Z.one)) AST_PLUS in
        meet x (binary s y AST_MULTIPLY),
        meet y (join (binary x s AST_DIVIDE) (const Z.zero))
      | AST_MODULO -> assert false (* TODO: complete *)

  let root_refine r x = function
      AST_GREATER_EQUAL -> meet r (Itv (N_infinity, x))
    | AST_GREATER -> meet r (Itv (N_infinity, bound_sub x (Int Z.one)))
    | AST_LESS_EQUAL -> meet r (Itv (x, P_infinity))
    | AST_LESS -> meet r (Itv (bound_add x (Int Z.one), P_infinity))

  let compare a b op =
    let new_expr =
      AST_binary (AST_MINUS, (a, extent_unknown), (b, extent_unknown)) in
    let eval_expr = eval env new_expr in
    match op with
      AST_EQUAL -> 

      (* utilities *)

      let strict f = function
          Bot -> Bot
        | Itv (x, y) -> f x y

  (* domain implementation *)

  (* order *)
  let subset a b = match a, b with
      Bot, _-> true | _, Bot -> false
    | Itv (a, b), Itv (c, d) ->
      bound_cmp a c >= 0 && bound_cmp b d >= 0

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

  (* boolean test *)
  let leq a b = match a, b with
      Bot, _ | _, Bot -> Bot, Bot
    | Itv (a, b), Itv (c, d) ->
      Itv (a, bound_min b d), Itv (bound_max a c, d)

  let is_bottom = function Bot -> true | _ -> false

  let print out x =
    let bound_to_string = function
        N_infinity -> "-inf"
      | P_infinity -> "inf"
      | Int x -> Z.to_string x in
    Printf.fprintf out (match x with
          Bot -> "[]"
        | Itv (a, b) -> "[%s, %s]" a b
end : VALUE_DOMAIN)