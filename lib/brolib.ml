module type OrderedType = sig
  type t

  val compare : t -> t -> int
end

(* original *)
module Make (Ord : OrderedType) = struct
  type key = Ord.t

  type set =
    | N0
    | N1 of set
    | N2 of set * key * set
    (* Aux constructors. *)
    | L2 of key
    | N3 of set * key * set * key * set
  (* Optimisation constructors. *)

  let rec member key set =
    match set with
    | N0 -> false
    | N1 t -> member key t
    | N2 (l, k, r) ->
        let c = Ord.compare key k in
        if c < 0 then member key l else if c > 0 then member key r else true
    | L2 _ | N3 _ -> failwith "encountered L2 or N3 in bro tree set."

  (* Insertion functions. *)
  let root = function
    | L2 a -> N2 (N0, a, N0)
    | N3 (t1, a1, t2, a2, t3) -> N2 (N2 (t1, a1, t2), a2, N1 t3)
    | t -> t

  let n1 = function
    | L2 a -> N2 (N0, a, N0)
    | N3 (t1, a1, t2, a2, t3) -> N2 (N2 (t1, a1, t2), a2, N1 t3)
    | t -> N1 t

  let n2_left left key right =
    match (left, key, right) with
    | L2 a1, a2, t1 -> N3 (N0, a1, N0, a2, t1)
    | N3 (t1, a1, t2, a2, t3), a3, N1 t4 ->
        N2 (N2 (t1, a1, t2), a2, N2 (t3, a3, t4))
    | N3 (t1, a1, t2, a2, t3), a3, (N2 _ as t4) ->
        N3 (N2 (t1, a1, t2), a2, N1 t3, a3, t4)
    | l, k, r -> N2 (l, k, r)

  let n2_right left key right =
    match (left, key, right) with
    | t1, a1, L2 a2 -> N3 (t1, a1, N0, a2, N0)
    | N1 t1, a1, N3 (t2, a2, t3, a3, t4) ->
        N2 (N2 (t1, a1, t2), a2, N2 (t3, a3, t4))
    | (N2 _ as t1), a1, N3 (t2, a2, t3, a3, t4) ->
        N3 (t1, a1, N1 t2, a2, N2 (t3, a3, t4))
    | l, k, r -> N2 (l, k, r)

  let rec ins key = function
    | N0 -> L2 key
    | N1 t -> n1 (ins key t)
    | N2 (l, k, r) as node ->
        let c = Ord.compare key k in
        if c > 0 then n2_right l k (ins key r)
        else if c < 0 then n2_left (ins key l) k r
        else node
    | _ -> failwith "unexpected broset ins"

  let insert key set = root (ins key set)
end

module BroInt = Make (Int)

let _ =
  BroInt.N2
    ( BroInt.N2 (BroInt.N2 (BroInt.N0, 1, BroInt.N0), 2, BroInt.N1 BroInt.N0),
      3,
      BroInt.N1 (BroInt.N2 (BroInt.N0, 4, BroInt.N0)) )
