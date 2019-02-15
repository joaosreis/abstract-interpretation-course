open Abstract_syntax_tree

type t = Bot | Top | Constant of Z.t

let top = Top

let bottom = Bot

let const x = Constant x

let rand _ _ = Top

let unary cst op = match cst, op with
    Bot, _ -> Bot
  | Top, _ -> Top
  | _, AST_UNARY_PLUS -> cst
  | Constant x, AST_UNARY_MINUS -> Constant (Z.neg x)

let%test _ = unary (Constant (Z.minus_one)) AST_UNARY_MINUS =
             Constant (Z.one)

let binary x y op = match x, y with
    Bot, _ | _, Bot -> Bot
  | Top, _ | _, Top -> Top
  | Constant x, Constant y -> match op with
      AST_PLUS -> Constant Z.(x + y)
    | AST_MINUS -> Constant Z.(x - y)
    | AST_MULTIPLY -> Constant Z.(x * y)
    | AST_DIVIDE -> if y = Z.zero then Top else Constant Z.(x / y)
    | AST_MODULO -> if y = Z.zero then Top else Constant Z.(x mod y)

let%test _ =
  binary (Constant (Z.of_int 10))
    (Constant (Z.of_int 2)) AST_PLUS =
  Constant (Z.of_int 12)

let%test _ =
  binary (Constant (Z.of_int 20))
    (Constant (Z.of_int 5)) AST_MINUS =
  Constant (Z.of_int 15)

let%test _ =
  binary (Constant (Z.of_int 3))
    (Constant (Z.of_int (-2))) AST_MULTIPLY =
  Constant (Z.of_int (-6))

let%test _ =
  binary (Constant (Z.of_int 16))
    (Constant (Z.of_int 4)) AST_DIVIDE =
  Constant (Z.of_int 4)

let%test _ =
  binary (Constant (Z.of_int 5))
    (Constant (Z.zero)) AST_DIVIDE =
  Top

let join x y = match x, y with
    Bot, l | l, Bot -> l
  | Constant a, Constant b when a = b -> x
  | Constant _, Constant _ | Top, _ | _, Top -> Top

let meet x y = match x, y with
    Bot, _ | _, Bot -> Bot
  | Top, l | l, Top -> l
  | Constant a, Constant b when a = b -> x
  | Constant _, Constant _ -> Top

let%test _ =
  meet (Constant (Z.of_int (-2)))
    (Constant (Z.of_int 0)) =
  Top

let%test _ =
  meet (Constant (Z.of_int 0))
    (Constant (Z.of_int 0)) =
  Constant (Z.of_int 0)

let%test _ =
  join (Constant (Z.of_int (-2)))
    (Constant (Z.of_int 0)) =
  Top

let%test _ =
  join (Constant (Z.of_int 0))
    (Constant (Z.of_int 0)) =
  Constant (Z.of_int 0)

let bwd_unary x op r =
  match x, r with
    Bot, _ | _, Bot -> Bot
  | Top, _ | _, Top -> Top
  | Constant _, Constant r -> match op with
      AST_UNARY_PLUS -> x
    | AST_UNARY_MINUS -> meet x (Constant (Z.neg r))

let bwd_binary x y op r =
  match x, y, r with
    Bot, _, _ | _, Bot, _ | _, _, Bot -> Bot, Bot
  | Top, _, _ | _, Top, _ | _, _, Top -> Top, Top
  | Constant _, Constant _, Constant _ -> match op with
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
      meet x (binary r y AST_MULTIPLY),
      meet y (join (binary x r AST_DIVIDE) (const Z.zero))
    | AST_MODULO -> assert false (* TODO: complete *)

let rec compare x y op =
  match op, x, y with
    AST_GREATER_EQUAL, Constant a, Constant b ->
    if Z.(leq a b) then
      x, y
    else Bot, Bot
  | AST_LESS_EQUAL, Constant _, Constant _ ->
    compare y x AST_GREATER_EQUAL
  | AST_GREATER, Constant a, Constant b -> (* FIXME: not sure if it is correct *)
    if Z.(lt a b) then
      x, y
    else Bot, Bot
  | AST_LESS, Constant _, Constant _ ->
    compare y x AST_GREATER
  | AST_EQUAL, Constant _, Constant _ ->
    meet x y, meet y x
  | AST_NOT_EQUAL, _, _ (* FIXME: probably not correct *)
  | _, Bot, _ | _, _ , Bot -> Bot, Bot
  | _, Top, _ | _, _ , Top -> Top, Top

let subset x y = match x, y with
    Bot, _-> true | _, Top
  | _, Bot | Top, _-> false 
  | Constant a, Constant b -> Z.(equal a b)

let widen a b = match a, b with
    x, Bot | Bot, x -> x
  | Top, _ | _, Top | Constant _, Constant _ -> Top

let is_bottom = function Bot -> true | _ -> false

let print out = function
    Bot -> Printf.fprintf out "bot"
  | Top -> Printf.fprintf out "top"
  | Constant x ->
    Printf.fprintf out "%s" (Z.to_string x)
