include Type_analysis

module Type_set = Set.Make(struct
    type t = env
    let compare = compare
  end)

type env_set = Type_set.t

let env_set_join x y =

