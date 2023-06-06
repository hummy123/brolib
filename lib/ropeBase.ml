(* Module contains base rope functions used in both normal (string) Rope and ArrayRope. *)
module type Foundation = sig
  type elt
  type metadata

  type t =
    | N0 of elt
    | N1 of t
    | N2 of t * metadata * metadata * t
    (* Aux constructors. *)
    | L2 of elt * elt
    | N3 of t * t * t

  val length : elt -> metadata
  (** Returns the metadata (array size, etc.) of the subtree this is called on. *)

  val concat : elt list -> elt
  (** Returns an element concatenation. *)

  val is_empty : elt -> bool
  (** Returns a bool indicating whether the element is empty. *)

  val combine : metadata -> metadata -> metadata
  (** Sums two metadata types together to produce a new one. *)

  val less_than_weight : int -> metadata -> bool
  (** Whether the index is less than the left metadata. *)

  val greater_than_weight : int -> metadata -> bool

  val subtract_weight : int -> metadata -> int
  (** Subtracts the weight from the current integer index, as needed for tree traversal. *)

  val insert_leaf : int -> elt -> elt -> t
  (** Function specifying how to insert into a leaf, taking index and (old and new) elts into account. *)

  val sub_leaf : int -> int -> elt -> elt list -> elt list
  (** Specifies how to extract a sub from a leaf node, given start and end index, element at this node and accumulator. *)

  val del_leaf : int -> int -> elt -> t
  (** Specifies how to delete a leaf, whether delete means delete all or delete start/middle/end. *)
end

module Make (Fnd : Foundation) = struct
  type t = Fnd.t

  open Fnd

  let rec size = function
    | N0 s -> Fnd.length s
    | N1 t -> size t
    | N2 (_, lm, rm, _) -> Fnd.combine lm rm
    | N3 (t1, t2, t3) ->
        Fnd.combine (size t1) (size t2) |> Fnd.combine (size t3)
    | L2 _ -> failwith "unexpected RopeBase.size: L2"

  let root = function
    | L2 (s1, s2) -> N2 (N0 s1, Fnd.length s1, Fnd.length s2, N0 s2)
    | N3 (t1, t2, t3) ->
        let t1_size = size t1 in
        let t2_size = size t2 in
        let left = N2 (t1, t1_size, t2_size, t2) in
        N2 (left, Fnd.combine t1_size t2_size, size t3, N1 t3)
    | t -> t

  let n1 = function
    | L2 (s1, s2) -> N2 (N0 s1, Fnd.length s1, Fnd.length s2, N0 s2)
    | N3 (t1, t2, t3) ->
        let t1_size = size t1 in
        let t2_size = size t2 in
        let left = N2 (t1, t1_size, t2_size, t2) in
        N2 (left, Fnd.combine t1_size t2_size, size t3, N1 t3)
    | t -> N1 t

  let ins_n2_left left right =
    match (left, right) with
    | L2 (s1, s2), t3 -> N3 (N0 s1, N0 s2, t3)
    | N3 (t1, t2, t3), N1 t4 ->
        let t1_size = size t1 in
        let t2_size = size t2 in
        let left = N2 (t1, t1_size, t2_size, t2) in
        let t3_size = size t3 in
        let t4_size = size t4 in
        let right = N2 (t3, t3_size, t4_size, t4) in
        N2
          (left, Fnd.combine t1_size t2_size, Fnd.combine t3_size t4_size, right)
    | N3 (t1, t2, t3), (N2 _ as t4) ->
        N3 (N2 (t1, size t1, size t2, t2), N1 t3, t4)
    | N3 (t1, t2, t3), t4 ->
        let t1_size = size t1 in
        let t2_size = size t2 in
        let left = N2 (t1, t1_size, t2_size, t2) in
        let t3_size = size t3 in
        let t4_size = size t4 in
        let right = N2 (t3, t3_size, t4_size, t4) in
        N2
          (left, Fnd.combine t1_size t2_size, Fnd.combine t3_size t4_size, right)
    | l, r -> N2 (l, size l, size r, r)

  let ins_n2_right left right =
    match (left, right) with
    | t1, L2 (s1, s2) -> N3 (t1, N0 s1, N0 s2)
    | N1 t1, N3 (t2, t3, t4) ->
        let t1_size = size t1 in
        let t2_size = size t2 in
        let left = N2 (t1, t1_size, t2_size, t2) in
        let t3_size = size t3 in
        let t4_size = size t4 in
        let right = N2 (t3, t3_size, t4_size, t4) in
        N2
          (left, Fnd.combine t1_size t2_size, Fnd.combine t3_size t4_size, right)
    | (N2 _ as t1), N3 (t2, t3, t4) ->
        N3 (t1, N1 t2, N2 (t3, size t3, size t4, t4))
    | t1, N3 (t2, t3, t4) ->
        let t1_size = size t1 in
        let t2_size = size t2 in
        let left = N2 (t1, t1_size, t2_size, t2) in
        let t3_size = size t3 in
        let t4_size = size t4 in
        let right = N2 (t3, t3_size, t4_size, t4) in
        N2
          (left, Fnd.combine t1_size t2_size, Fnd.combine t3_size t4_size, right)
    | l, r -> N2 (l, size l, size r, r)

  let rec fold_left f state = function
    | N0 value -> if Fnd.is_empty value then state else f state value
    | N1 t -> fold_left f state t
    | N2 (l, _, _, r) ->
        let state = fold_left f state l in
        fold_left f state r
    | N3 _ -> failwith "unexpected Rope.fold: N3"
    | L2 _ -> failwith "unexpected Rope.fold: L2"

  let rec fold_right f state = function
    | N0 value -> if Fnd.is_empty value then state else f state value
    | N1 t -> fold_right f state t
    | N2 (l, _, _, r) ->
        let state = fold_right f state r in
        fold_right f state l
    | N3 _ -> failwith "unexpected Rope.fold_back: N3"
    | L2 _ -> failwith "unexpected Rope.fold_back: L2"

  let rec ins cur_index ins_val = function
    | N0 old_val -> Fnd.insert_leaf cur_index old_val ins_val
    | N1 t -> n1 (ins cur_index ins_val t)
    | N2 (l, lm, _, r) ->
        if Fnd.less_than_weight cur_index lm then
          ins_n2_left (ins cur_index ins_val l) r
        else ins_n2_right l (ins (Fnd.subtract_weight cur_index lm) ins_val r)
    | N3 _ -> failwith "unexpected Rope.ins: N3"
    | L2 _ -> failwith "unexpected Rope.ins: L2"

  let rec sub_internal start_idx end_idx acc = function
    | N0 elt -> Fnd.sub_leaf start_idx end_idx elt acc
    | N1 t -> sub_internal start_idx end_idx acc t
    | N2 (l, lm, _, r) ->
        (* Cases we need to consider.
           1. start_idx and end_idx are in same directions (both less than or both greater than weight).
              (if end index is less than weight, then start index must be too;
               if start index is greater than weight, then end index must be too.)
           2. start_idx and end_idx are in different direction (start_idx is less than weight while end_idx is less than weight.)
        *)
        if Fnd.less_than_weight end_idx lm then
          sub_internal start_idx end_idx acc l
        else if Fnd.greater_than_weight start_idx lm then
          sub_internal
            (Fnd.subtract_weight start_idx lm)
            (Fnd.subtract_weight end_idx lm)
            acc r
        else
          let acc =
            sub_internal
              (Fnd.subtract_weight start_idx lm)
              (Fnd.subtract_weight end_idx lm)
              acc r
          in
          sub_internal start_idx end_idx acc l
    | _ -> failwith "unexpected Rope.sub_internal"

  (* Deletion involves deleting strings within nodes rather deleting the nodes themselves,
     as this helps better maintain balancing.
  *)
  let rec del_internal start_idx end_idx = function
    | N0 elt -> del_leaf start_idx end_idx elt
    | N1 t -> del_internal start_idx end_idx t
    | N2 (l, lm, rm, r) ->
        if Fnd.less_than_weight end_idx lm then
          let l = del_internal start_idx end_idx l in
          N2 (l, size l, rm, r)
        else if Fnd.greater_than_weight start_idx lm then
          let r =
            del_internal
              (Fnd.subtract_weight start_idx lm)
              (Fnd.subtract_weight end_idx lm)
              r
          in
          N2 (l, lm, size r, r)
        else
          let r =
            del_internal
              (Fnd.subtract_weight start_idx lm)
              (Fnd.subtract_weight end_idx lm)
              r
          in
          let l = del_internal start_idx end_idx l in
          N2 (l, size l, size r, r)
    | _ -> failwith "unexpected Rope.del_internal"
end
