(*
  Cours "Semantics and applications to verification"

  Marc Chevalier 2018
  Ecole normale supérieure, Paris, France / CNRS / INRIA
  Inspired by https://github.com/janestreet/base/blob/master/src/option.ml
*)

let is_none : type a. a option -> bool = function None -> true | _ -> false

let is_some : type a. a option -> bool = function Some _ -> true | _ -> false

let value_map : type a b. a option -> default:b -> f:(a -> b) -> b = fun o ~default ~f ->
  match o with
  | Some x -> f x
  | None   -> default

let map : type a b. (a -> b) -> a option -> b option = fun f vo ->
  match vo with
  | Some v -> Some (f v)
  | None   -> None

let iter : type a. a option -> f:(a -> unit) -> unit = fun o ~f ->
  match o with
  | None -> ()
  | Some a -> f a

let invariant : type a. (a -> unit) -> a option -> unit = fun f t -> iter t ~f

let map2 : type a b c. a option -> b option -> f:(a -> b -> c) -> c option  = fun o1 o2 ~f ->
  match o1, o2 with
  | Some a1, Some a2 -> Some (f a1 a2)
  | _ -> None

let call : type a. a -> f:((a -> unit) option) -> unit = fun x ~f ->
  match f with
  | None -> ()
  | Some f -> f x

let value : type a. a option -> default:a -> a = fun t ~default ->
  match t with
  | None -> default
  | Some x -> x

let to_array : type a. a option -> a array = fun t ->
  match t with
  | None -> [||]
  | Some x -> [|x|]

let to_list : type a. a option -> a list = fun t ->
  match t with
  | None -> []
  | Some x -> [x]

let for_all : type a. a option -> f:(a -> bool) -> bool = fun t ~f ->
  match t with
  | None -> true
  | Some x -> f x

let exists : type a. a option -> f:(a -> bool) -> bool = fun t ~f ->
  match t with
  | None -> false
  | Some x -> f x

let mem : type a b. a option -> b -> equal:(b -> a -> bool) -> bool = fun t a ~equal ->
  match t with
  | None -> false
  | Some a' -> equal a a'

let length : type a. a option -> int = fun t ->
  match t with
  | None -> 0
  | Some _ -> 1

let is_empty : type a. a option -> bool = is_none

let fold : type a b. a option -> init:b -> f:(b -> a -> b) -> b= fun t ~init ~f ->
  match t with
  | None -> init
  | Some x -> f init x

let count : type a. a option -> f:(a -> bool) -> int = fun t ~f ->
  match t with
  | None -> 0
  | Some a -> if f a then 1 else 0

let find : type a. a option -> f:(a -> bool) -> a option = fun t ~f ->
  match t with
  | None -> None
  | Some x -> if f x then Some x else None

let find_map : type a b. a option -> f:(a -> b option) -> b option = fun t ~f ->
  match t with
  | None -> None
  | Some a -> f a

let equal : type a b. (a -> b -> bool) -> a option -> b option -> bool = fun f t t' ->
  match t, t' with
  | None, None -> true
  | Some x, Some x' -> f x x'
  | _ -> false

let some : type a. a -> a option = fun x -> Some x

let both : type a b. a option -> b option -> (a*b) option = fun x y ->
  match x,y with
  | Some a, Some b -> Some (a,b)
  | _ -> None

let first_some : type a. a option -> a option -> a option = fun x y ->
  match x with
  | Some _ -> x
  | None -> y

let some_if : type a. bool -> a -> a option = fun cond x -> if cond then Some x else None

let merge : type a. a option -> a option -> f:(a -> a -> a) -> a option = fun a b ~f ->
  match a, b with
  | None, x | x, None -> x
  | Some a, Some b -> Some (f a b)

let filter : type a. a option -> f:(a -> bool) -> a option = fun t ~f ->
  match t with
  | Some v as o when f v -> o
  | _ -> None

let try_with : type a. (unit -> a) -> a option = fun f ->
  try Some (f ())
  with _ -> None

