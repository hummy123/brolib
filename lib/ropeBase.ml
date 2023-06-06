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
  val concat : elt list -> elt
  val is_empty : elt -> bool
  val combine : metadata -> metadata -> metadata
end

module Make (Fnd : Foundation) = struct
  type t =
    | N0 of Fnd.elt
    | N1 of t
    | N2 of t * Fnd.metadata * Fnd.metadata * t
    (* Aux constructors. *)
    | L2 of Fnd.elt * Fnd.elt
    | N3 of t * t * t

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
end
