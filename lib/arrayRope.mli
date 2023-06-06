open TargetLength

module type S = sig
  type 'a t
  (** The type of the ArrayRope. *)

  val empty : 'a t
  (** The empty ArrayRope. *)

  val of_array : 'a array -> 'a t
  (** An ArrayRope constructored from an array. *)

  val singleton : 'a -> 'a t
  (** An ArrayRope constructed from a single element. *)

  val insert : int -> 'a array -> 'a t -> 'a t
  (** Inserts an array into an ArrayRope. *)

  val insert_one : int -> 'a -> 'a t -> 'a t
  (** Inserts a single element into an ArrayRope. *)

  val sub : int -> int -> 'a t -> 'a array
  (** Returns an array from the specified range in the ArrayRope. *)

  val delete : int -> int -> 'a t -> 'a t
  (** Returns an ArrayRope with elements at the specified range removed. *)

  val to_array : 'a t -> 'a array
  (** Returns an array from an ArrayRope. *)

  val fold_left : ('b -> 'a array -> 'b) -> 'b -> 'a t -> 'b
  (** Applies a function on each internal array in the ArrayRope in order, threading an accumulator argument. *)

  val fold_elements_left : ('a -> 'b -> 'a) -> 'a -> 'b t -> 'a
  (** Like fold_left, but applies a function to each element instead of the arrays that contain them. *)

  val fold_right : ('a -> 'b array -> 'a) -> 'a -> 'b t -> 'a
  (** Applies a function on each internal array in the ArrayRope in reverse order, threading an accumulator argument. *)

  val fold_elements_right : ('a -> 'b -> 'b) -> 'b -> 'a t -> 'b
  (** Like fold_right, but applies a function on each element instead of the arrays that contain them. *)
end

module Make (_: TargetLength) : S
(** Functor building an implementation of the ArrayRope structure. *)
