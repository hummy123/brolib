open TargetLength

module Make (Length : TargetLength) = struct
  type 'a rope =
    | N0 of 'a array
    | N1 of 'a rope
    | N2 of 'a rope * int * int * 'a rope
    (* Aux constructors. *)
    | L2 of 'a array * 'a array
    | N3 of 'a rope * 'a rope * 'a rope

  let empty = N0 [||]
  let of_array array = N0 array

  let rec size = function
    | N0 arr -> Array.length arr
    | N1 t -> size t
    | N2 (_, lm, rm, _) -> lm + rm
    | N3 (t1, t2, t3) -> size t1 + size t2 + size t3
    | L2 _ -> failwith "unexpected Brope.size: L2"

  let root = function
    | L2 (a1, a2) -> N2 (N0 a1, Array.length a1, Array.length a2, N0 a2)
    | N3 (t1, t2, t3) ->
        let t1_size = size t1 in
        let t2_size = size t2 in
        let left = N2 (t1, t1_size, t2_size, t2) in
        N2 (left, t1_size + t2_size, size t3, N1 t3)
    | t -> t

  let n1 = function
    | L2 (a1, a2) -> N2 (N0 a1, Array.length a1, Array.length a2, N0 a2)
    | N3 (t1, t2, t3) ->
        let t1_size = size t1 in
        let t2_size = size t2 in
        let left = N2 (t1, t1_size, t2_size, t2) in
        N2 (left, t1_size + t2_size, size t3, N1 t3)
    | t -> N1 t

  let ins_n2_left left right =
    match (left, right) with
    | L2 (a1, a2), t3 -> N3 (N0 a1, N0 a2, t3)
    | N3 (t1, t2, t3), N1 t4 ->
        let t1_size = size t1 in
        let t2_size = size t2 in
        let left = N2 (t1, t1_size, t2_size, t2) in
        let t3_size = size t3 in
        let t4_size = size t4 in
        let right = N2 (t3, t3_size, t4_size, t4) in
        N2 (left, t1_size + t2_size, t3_size + t4_size, right)
    | N3 (t1, t2, t3), (N2 _ as t4) ->
        N3 (N2 (t1, size t1, size t2, t2), N1 t3, t4)
    | N3 (t1, t2, t3), t4 ->
        let t1_size = size t1 in
        let t2_size = size t2 in
        let left = N2 (t1, t1_size, t2_size, t2) in
        let t3_size = size t3 in
        let t4_size = size t4 in
        let right = N2 (t3, t3_size, t4_size, t4) in
        N2 (left, t1_size + t2_size, t3_size + t4_size, right)
    | l, r -> N2 (l, size l, size r, r)

  let ins_n2_right left right =
    match (left, right) with
    | t1, L2 (a1, a2) -> N3 (t1, N0 a1, N0 a2)
    | N1 t1, N3 (t2, t3, t4) ->
        let t1_size = size t1 in
        let t2_size = size t2 in
        let left = N2 (t1, t1_size, t2_size, t2) in
        let t3_size = size t3 in
        let t4_size = size t4 in
        let right = N2 (t3, t3_size, t4_size, t4) in
        N2 (left, t1_size + t2_size, t3_size + t4_size, right)
    | (N2 _ as t1), N3 (t2, t3, t4) ->
        N3 (t1, N1 t2, N2 (t3, size t3, size t4, t4))
    | t1, N3 (t2, t3, t4) ->
        let t1_size = size t1 in
        let t2_size = size t2 in
        let left = N2 (t1, t1_size, t2_size, t2) in
        let t3_size = size t3 in
        let t4_size = size t4 in
        let right = N2 (t3, t3_size, t4_size, t4) in
        N2 (left, t1_size + t2_size, t3_size + t4_size, right)
    | l, r -> N2 (l, size l, size r, r)

  let rec ins cur_index ins_arr = function
    | N0 old_arr ->
        if cur_index <= 0 then
          if Array.length old_arr + Array.length ins_arr <= Length.target_length
          then N0 (Array.append ins_arr old_arr)
          else L2 (ins_arr, old_arr)
        else if cur_index >= Array.length old_arr then
          if Array.length old_arr + Array.length ins_arr <= Length.target_length
          then N0 (Array.append old_arr ins_arr)
          else L2 (old_arr, ins_arr)
        else
          let sub1 = Array.sub old_arr 0 cur_index in
          let sub2 =
            Array.sub old_arr cur_index (Array.length old_arr - cur_index)
          in
          if Array.length old_arr + Array.length ins_arr <= Length.target_length
          then N0 (Array.concat [ sub1; ins_arr; sub2 ])
          else if
            Array.length sub1 + Array.length ins_arr <= Length.target_length
          then L2 (Array.append sub1 ins_arr, sub2)
          else if
            Array.length sub2 + Array.length ins_arr <= Length.target_length
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

  let rec fold_left f state = function
    | N0 [||] -> state
    | N0 arr -> f state arr
    | N1 t -> fold_left f state t
    | N2 (l, _, _, r) ->
        let state = fold_left f state l in
        fold_left f state r
    | N3 _ -> failwith "unexpected Brope.fold: N3"
    | L2 _ -> failwith "unexpected Brope.fold: L2"

  let rec fold_elements_left f state = function
    | N0 [||] -> state
    | N0 arr -> Array.fold_left f state arr
    | N1 t -> fold_elements_left f state t
    | N2 (l, _, _, r) ->
        let state = fold_elements_left f state l in
        fold_elements_left f state r
    | N3 _ -> failwith "unexpected Brope.fold: N3"
    | L2 _ -> failwith "unexpected Brope.fold: L2"

  let rec fold_right f state = function
    | N0 [||] -> state
    | N0 arr -> f state arr
    | N1 t -> fold_right f state t
    | N2 (l, _, _, r) ->
        let state = fold_right f state r in
        fold_right f state l
    | N3 _ -> failwith "unexpected Brope.fold_back: N3"
    | L2 _ -> failwith "unexpected Brope.fold_back: L2"

  let rec fold_elements_right f state = function
    | N0 [||] -> state
    | N0 arr -> Array.fold_right f arr state
    | N1 t -> fold_elements_right f state t
    | N2 (l, _, _, r) ->
        let state = fold_elements_right f state r in
        fold_elements_right f state l
    | N3 _ -> failwith "unexpected Brope.fold_back: N3"
    | L2 _ -> failwith "unexpected Brope.fold_back: L2"

  let to_array rope =
    fold_right (fun lst arr -> arr :: lst) [] rope |> Array.concat
end
