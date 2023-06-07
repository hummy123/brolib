module Rope = Rope
module ArrayRope = ArrayRope

(* Provide a default Rope that should work well in common scenarios. *)
module Length1024 = struct
  let target_length = 1024
end

module Rope1024 = Rope.Make (Length1024)

module A1024 = struct
  type t = int

  let target_length = 1024
end

module Ar15 = ArrayRope.Make (A1024)
