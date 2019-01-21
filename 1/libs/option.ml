(*
  Cours "Semantics and applications to verification"

  Marc Chevalier 2018
  Ecole normale supérieure, Paris, France / CNRS / INRIA
  Inspired by https://github.com/janestreet/base/blob/master/src/option.ml
*)

let default : type a. a -> a option -> a = fun def vo ->
  match vo with
  | Some v -> v
  | None   -> def

let fold : type a b. (a -> b -> b) -> a option -> b -> b = fun f vo acc ->
  match vo with
  | Some v -> f v acc
  | None   -> acc

let map : type a b. (a -> b) -> a option -> b option = fun f vo ->
  match vo with
  | Some v -> Some (f v)
  | None   -> None

let apply : type a. (a -> unit) -> a option -> unit = fun f vo ->
  ignore (map f vo)

let map_default : type a b. (a -> b) -> b -> a option -> b = fun f def vo ->
  match vo with
  | Some v -> f v
  | None   -> def

let is_some : type a. a option -> bool = fun vo -> match vo with
  | Some _ -> true
  | None -> false

let pp : type f a. (f -> a -> unit) -> f -> a option -> unit = fun p c vo ->
  apply (p c) vo

let map_option : type a b. (a -> b option) -> a list -> b list = fun f l ->
  List.rev @@ List.fold_left
    (fun res x ->
       match f x with
       | None -> res
       | Some y -> y::res)
    []
    l
