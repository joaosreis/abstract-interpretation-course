(*
  Cours "Semantics and applications to verification"

  Marc Chevalier 2018
  Ecole normale supérieure, Paris, France / CNRS / INRIA
  Inspired by https://github.com/janestreet/base/blob/master/src/fn.ml
*)

(* The identity function *)
let id : type a. a -> a = fun x -> x

(* Reverse the order of arguments for a binary function *)
let flip : type a b c. (b -> a -> c) -> a -> b -> c = fun f x y -> f y x

(* 'compose f g x' is 'f (g x)' *)
let compose : type a b c. (b -> c) -> (a -> b) -> a -> c = fun f g x -> x |> g |> f

(* Constructor of constant functions *)
let const : type a b. a -> b -> a = fun x _ -> x

let rec apply_n_times : type a. n:int -> (a -> a) -> a -> a = fun ~n:n f x ->
  if n <= 0 then x else apply_n_times ~n:(n-1) f (f x)
