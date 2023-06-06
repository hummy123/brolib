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
  open ArrayRopeFdn
  open ArrayBase

  type elt = Config.t
  type t = ArrayBase.t

  let empty = N0 [||]
  let of_array array = N0 array
  let singleton element = N0 [| element |]

  let rec ins cur_index ins_arr = function
    | N0 old_arr ->
        if cur_index <= 0 then
          if Array.length old_arr + Array.length ins_arr <= Config.target_length
          then N0 (Array.append ins_arr old_arr)
          else L2 (ins_arr, old_arr)
        else if cur_index >= Array.length old_arr then
          if Array.length old_arr + Array.length ins_arr <= Config.target_length
          then N0 (Array.append old_arr ins_arr)
          else L2 (old_arr, ins_arr)
        else
          let sub1 = Array.sub old_arr 0 cur_index in
          let sub2 =
            Array.sub old_arr cur_index (Array.length old_arr - cur_index)
          in
          if Array.length old_arr + Array.length ins_arr <= Config.target_length
          then N0 (Array.concat [ sub1; ins_arr; sub2 ])
          else if
            Array.length sub1 + Array.length ins_arr <= Config.target_length
          then L2 (Array.append sub1 ins_arr, sub2)
          else if
            Array.length sub2 + Array.length ins_arr <= Config.target_length
          then L2 (sub1, Array.append ins_arr sub2)
          else
            (* Array must be split into 3 different parts. *)
            N3 (N0 sub1, N0 ins_arr, N0 sub2)
    | N1 t -> n1 (ins cur_index ins_arr t)
    | N2 (l, lm, _, r) ->
        if cur_index < lm then ins_n2_left (ins cur_index ins_arr l) r
        else ins_n2_right l (ins (cur_index - lm) ins_arr r)
    | N3 _ -> failwith "unexpected Brope.ins: N3"
    | L2 _ -> failwith "unexpected Brope.ins: L2"

  let insert index array rope = root (ins index array rope)
  let insert_one index element rope = root (ins index [| element |] rope)

  let rec sub_internal start_idx end_idx acc = function
    | N0 arr ->
        if start_idx <= 0 && end_idx >= Array.length arr then
          (* In range. *)
          arr :: acc
        else if start_idx >= 0 && end_idx <= Array.length arr then
          (* In middle of this node. *)
          let arr = Array.sub arr start_idx (end_idx - start_idx) in
          arr :: acc
        else if start_idx >= 0 && end_idx >= Array.length arr then
          (* Starts at this node. *)
          let arr = Array.sub arr start_idx (Array.length arr - start_idx) in
          arr :: acc
        else
          (* Ends at this node. *)
          let ar = Array.sub arr 0 end_idx in
          ar :: acc
    | N1 t -> sub_internal start_idx end_idx acc t
    | N2 (l, lm, _, r) ->
        (* Cases we need to consider.
           1. start_idx and end_idx are in same directions (both less than or both greater than weight).
           2. start_idx and end_idx are in different direction (start_idx is less than weight while end_idx is less than weight.)
        *)
        if lm > start_idx && lm > end_idx then
          sub_internal start_idx end_idx acc l
        else if lm < start_idx && lm < end_idx then
          sub_internal (start_idx - lm) (end_idx - lm) acc r
        else
          let acc = sub_internal (start_idx - lm) (end_idx - lm) acc r in
          sub_internal start_idx end_idx acc l
    | _ -> failwith ""

  let sub start length rope =
    sub_internal start (start + length) [] rope |> Array.concat

  (* Deletion involves deleting strings within nodes rather deleting the nodes themselves,
     as this helps better maintain balancing.
  *)
  let rec del_internal start_idx end_idx = function
    | N0 arr ->
        if start_idx <= 0 && end_idx >= Array.length arr then
          (* In range. *)
          N0 [||]
        else if start_idx >= 0 && end_idx <= Array.length arr then
          (* In middle of this node. *)
          let sub1 = Array.sub arr 0 start_idx in
          let sub2 = Array.sub arr end_idx (Array.length arr - end_idx) in
          N0 (Array.append sub1 sub2)
        else if start_idx >= 0 && end_idx >= Array.length arr then
          (* Starts at this node. *)
          let arr = Array.sub arr 0 start_idx in
          N0 arr
        else
          (* Ends at this node. *)
          let arr = Array.sub arr end_idx (Array.length arr - end_idx) in
          N0 arr
    | N1 t -> del_internal start_idx end_idx t
    | N2 (l, lm, rm, r) ->
        if lm > start_idx && lm > end_idx then
          let l = del_internal start_idx end_idx l in
          N2 (l, size l, rm, r)
        else if lm < start_idx && lm < end_idx then
          let r = del_internal (start_idx - lm) (end_idx - lm) r in
          N2 (l, lm, size r, r)
        else
          let r = del_internal (start_idx - lm) (end_idx - lm) r in
          let l = del_internal start_idx end_idx l in
          N2 (l, size l, size r, r)
    | _ -> failwith ""

  let delete start length rope = del_internal start (start + length) rope
  let fold_left = fold_left
  let fold_right = fold_right

  let rec fold_elements_left f state = function
    | N0 [||] -> state
    | N0 arr -> Array.fold_left f state arr
    | N1 t -> fold_elements_left f state t
    | N2 (l, _, _, r) ->
        let state = fold_elements_left f state l in
        fold_elements_left f state r
    | N3 _ -> failwith "unexpected Rope.fold: N3"
    | L2 _ -> failwith "unexpected Rope.fold: L2"

  let rec fold_elements_right f state = function
    | N0 [||] -> state
    | N0 arr -> Array.fold_right f arr state
    | N1 t -> fold_elements_right f state t
    | N2 (l, _, _, r) ->
        let state = fold_elements_right f state r in
        fold_elements_right f state l
    | N3 _ -> failwith "unexpected Rope.fold_back: N3"
    | L2 _ -> failwith "unexpected Rope.fold_back: L2"

  let to_array rope =
    fold_right (fun lst arr -> arr :: lst) [] rope |> Array.concat
end
