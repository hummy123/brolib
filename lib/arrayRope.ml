module type S = sig
  type elt
  (** The type of the individual elements in the ArrayRope. *)

  type t
  (** The type of the ArrayRope. *)

  val empty : t
  (** The empty ArrayRope. *)

  val of_array : elt array -> t
  (** An ArrayRope constructored from an array. *)

  val singleton : elt -> t
  (** An ArrayRope constructed from a single element. *)

  val insert : int -> elt array -> t -> t
  (** Inserts an array into an ArrayRope. *)

  val insert_one : int -> elt -> t -> t
  (** Inserts a single element into an ArrayRope. *)

  val sub : int -> int -> t -> elt array
  (** Returns an array from the specified range in the ArrayRope. *)

  val delete : int -> int -> t -> t
  (** Returns an ArrayRope with elements at the specified range removed. *)

  val to_array : t -> elt array
  (** Returns an array from an ArrayRope. *)

  val fold_left : ('b -> elt array -> 'b) -> 'b -> t -> 'b
  (** Applies a function on each internal array in the ArrayRope in order, threading an accumulator argument. *)

  val fold_elements_left : ('a -> elt -> 'a) -> 'a -> t -> 'a
  (** Like fold_left, but applies a function to each element instead of the arrays that contain them. *)

  val fold_right : ('a -> elt array -> 'a) -> 'a -> t -> 'a
  (** Applies a function on each internal array in the ArrayRope in reverse order, threading an accumulator argument. *)

  val fold_elements_right : (elt -> 'a -> 'a) -> 'a -> t -> 'a
  (** Like fold_right, but applies a function on each element instead of the arrays that contain them. *)
end

module type ArrayConfig = sig
  type t
  val target_length : int
end

module Make (Config : ArrayConfig) : S = struct
  module ArrayType = ArrayRopeFdn.ArrayFdn (Config)
  module ArrayBase = RopeBase.Make (ArrayType)
  open ArrayType
  open ArrayBase

  type elt = Config.t
  type t = ArrayBase.t

  let empty = N0 [||]
  let of_array array = N0 array
  let singleton element = N0 [| element |]
  let insert index array rope = root (ins index array rope)
  let insert_one index element rope = root (ins index [| element |] rope)

  let sub start length rope =
    sub_internal start (start + length) [] rope |> Array.concat

  let delete start length rope = del_internal start (start + length) rope
  let fold_left = fold_left
  let fold_elements_left = fold_elements_left
  let fold_right = fold_right
  let fold_elements_right = fold_elements_right

  let to_array rope =
    fold_right (fun lst arr -> arr :: lst) [] rope |> Array.concat
end
