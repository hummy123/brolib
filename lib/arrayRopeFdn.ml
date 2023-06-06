module type ElementType = sig
  type t
end

module ArrayFdn (El : ElementType) = struct
  type elt = El.t array
  type metadata = int

  type t =
    | N0 of elt
    | N1 of t
    | N2 of t * metadata * metadata * t
    | L2 of elt * elt
    | N3 of t * t * t

  let length = Array.length
  let concat = Array.concat
  let is_empty arr = Array.length arr = 0
  let combine = ( + )
end
