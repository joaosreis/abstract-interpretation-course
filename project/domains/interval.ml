open Abstract_syntax_tree

type bound = N_infinity | P_infinity | Int of Z.t

type t = Bot | Itv of bound * bound

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
  | Itv (x, y), AST_UNARY_MINUS -> Itv (bound_neg y, bound_neg x)

let%test _ = unary (Itv (Int Z.minus_one, P_infinity)) AST_UNARY_MINUS =
             Itv (N_infinity, Int Z.one)

type sign = Neg | Pos

let sign = function
    N_infinity -> Neg
  | P_infinity -> Pos
  | Int x ->
    if Z.geq x Z.zero then Pos
    else Neg

let bound_add a b = match a, b with
    P_infinity, _ | _, P_infinity -> P_infinity
  | N_infinity, _ | _, N_infinity -> N_infinity
  | Int x, Int y -> Int (Z.add x y)

let bound_sub a b = match a, b with
    P_infinity, _ | _, N_infinity -> P_infinity
  | N_infinity, _ | _, P_infinity -> N_infinity
  | Int x, Int y -> Int (Z.sub x y)

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

(* Infix operators *)
module Infix = struct
  let (+&) = bound_add
  let (-&) = bound_sub
  let ( *&) = bound_mul
  let (/&) = bound_div
  (*let (=&) a b = bound_cmp a b = 0
    let (<>&) a b = bound_cmp a b <> 0*)
  let (<&) a b = bound_cmp a b < 0
  let (<=&) a b = bound_cmp a b <= 0
  (* let (>&) a b = bound_cmp a b > 0 *)
  let (>=&) a b = bound_cmp a b >= 0
end

open Infix

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
      AST_PLUS -> Itv (a +& c, b +& d)
    | AST_MINUS -> Itv (a -& d, b -& c)
    | AST_MULTIPLY ->
      let r1 = a *& c in
      let r2 = a *& d in
      let r3 = b *& c in
      let r4 = b *& d in
      Itv (bound_min r1 (bound_min r2 (bound_min r3 r4)), bound_max r1 (bound_max r2 (bound_max r3 r4)))
    | AST_DIVIDE ->
      let ac = a /& c in
      let ad = a /& d in
      let bc = b /& c in
      let bd = b /& d in
      if c >=& (Int Z.one) then
        Itv (bound_min ac ad, bound_max bc bd)
      else if d <=& (Int (Z.neg Z.one)) then
        Itv (bound_min bc bd, bound_max ac ad)
      else
        Bot
    | AST_MODULO -> assert false (* TODO: complete *)

let%test _ =
  binary (Itv (Int Z.zero, Int (Z.of_int 10)))
    (Itv (Int (Z.of_int 2), Int (Z.of_int 10))) AST_PLUS =
  Itv (Int (Z.of_int 2), Int (Z.of_int 20))

let%test _ =
  binary (Itv (Int (Z.of_int 2), Int (Z.of_int 20)))
    (Itv (Int (Z.of_int 3), Int (Z.of_int 5))) AST_MINUS =
  Itv (Int (Z.of_int (-3)), Int (Z.of_int 17))

let%test _ =
  binary (Itv (Int (Z.of_int 3), Int (Z.of_int 20)))
    (Itv (Int (Z.of_int (-2)), Int (Z.of_int 0))) AST_MULTIPLY =
  Itv (Int (Z.of_int (-40)), Int (Z.of_int 0))

let%test _ =
  binary (Itv (Int (Z.of_int 5), Int (Z.of_int 6)))
    (Itv (Int (Z.of_int 2), Int (Z.of_int 3))) AST_DIVIDE =
  Itv (Int (Z.of_int 1), Int (Z.of_int 3))

let%test _ =
  binary (Itv (Int (Z.of_int 5), Int (Z.of_int 6)))
    (Itv (Int (Z.of_int (-2)), Int (Z.of_int (-1)))) AST_DIVIDE =
  Itv (Int (Z.of_int (-6)), Int (Z.of_int (-2)))

let join x y = match x, y with
    Bot, l | l, Bot -> l
  | Itv (a, b), Itv (c, d) -> Itv (bound_min a c, bound_max b d)

let meet x y = match x, y with
    Bot, _ | _, Bot -> Bot
  | Itv (a, b), Itv (c, d) ->
    if bound_max a c <= bound_min b d then Itv (bound_max a c, bound_min b d)
    else
      Bot

let%test _ =
  meet (Itv (Int (Z.of_int (-2)), Int (Z.of_int 6)))
    (Itv (Int (Z.of_int 0), Int (Z.of_int 20))) =
  Itv (Int (Z.of_int 0), Int (Z.of_int 6))

let%test _ =
  join (Itv (Int (Z.of_int (-2)), Int (Z.of_int 6)))
    (Itv (Int (Z.of_int 0), Int (Z.of_int 20))) =
  Itv (Int (Z.of_int (-2)), Int (Z.of_int 20))

let bwd_unary x op r =
  match x, r with
    Bot, _ | _, Bot -> Bot
  | Itv _, Itv (r, s) -> match op with
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

let rec compare x y op =
  match op, x, y with
    AST_LESS_EQUAL, Itv (a, b), Itv (c, d) ->
    if a <=& d then
      Itv (a, bound_min b d), Itv (bound_max a c, d)
    else Bot, Bot
  | AST_GREATER_EQUAL, Itv _, Itv _ ->
    compare y x AST_GREATER_EQUAL
  | AST_GREATER, Itv (a, b), Itv (c, d) -> (* FIXME: not sure if it is correct *)
    if a <& d then
      let min_bd = bound_min b d in
      let max_ac = bound_max a c in
      let l = if min_bd = d then min_bd -& (Int Z.one) else min_bd in
      let r = if max_ac = a then max_ac +& (Int Z.one) else max_ac in
      Itv (a, l), Itv (r, d)
    else Bot, Bot
  | AST_LESS, Itv _, Itv _ ->
    compare y x AST_GREATER
  | AST_EQUAL, Itv _, Itv _ ->
    meet x y, meet y x
  | AST_NOT_EQUAL, _, _ (* FIXME: probably not correct *)
  | _, Bot, _ | _, _ , Bot -> Bot, Bot

let subset a b = match a, b with
    Bot, _-> true | _, Bot -> false
  | Itv (a, b), Itv (c, d) ->
    a >=& c && b >=& d

let widen a b = match a, b with
    x, Bot | Bot, x -> x
  | Itv (a, b), Itv (c, d) ->
    let x =
      if a <=& c then a
      else N_infinity in
    let y =
      if b >=& d then b
      else P_infinity in
    Itv (x, y)

let is_bottom = function Bot -> true | _ -> false

let print out x =
  let bound_to_string = function
      N_infinity -> "-inf"
    | P_infinity -> "inf"
    | Int x -> Z.to_string x in
  match x with
    Bot -> Printf.fprintf out "[]"
  | Itv (a, b) ->
    Printf.fprintf out "[%s, %s]" (bound_to_string a) (bound_to_string b)
