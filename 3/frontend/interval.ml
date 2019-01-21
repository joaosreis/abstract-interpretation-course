type iint = N_infinity | P_infinity | Int of int

type t = Bot | Interval of iint * iint

let min l =
  let rec aux m = function
      [] -> m
    | x :: xs -> if x < m then aux x xs else aux m xs in
  aux (List.hd l) l

let max l =
  let rec aux m = function
      [] -> m
    | x :: xs -> if x > m then aux x xs else aux m xs in
  aux (List.hd l) l

let rel = function
    [] -> Bot
  | l -> Interval (Int (min l), Int (max l))
