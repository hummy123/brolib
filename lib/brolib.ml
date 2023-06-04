module Rope = Rope

(* Provide a couple of Ropes that may work well for common scenarios. *)
module Length512 = struct
  let target_length = 512
end

module Length1024 = struct
  let target_length = 1024
end

module Rope512 = Rope.Make (Length512)
module Rope1024 = Rope.Make (Length1024)
