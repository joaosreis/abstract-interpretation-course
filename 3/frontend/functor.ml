module Make(I : sig type t end) = struct
  module M = Map.Make(String)

  type t = I.t M.t
end
