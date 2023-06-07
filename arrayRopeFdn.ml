module type ArrayConfig = sig
  type t

  val target_length : int
end

module ArrayFdn (Config : ArrayConfig) = struct
  type elt = Config.t array
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
  let less_than_weight = ( < )
  let greater_than_weight = ( > )
  let subtract_weight = ( - )

  let insert_leaf cur_index old_arr ins_arr =
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
      else if Array.length sub1 + Array.length ins_arr <= Config.target_length
      then L2 (Array.append sub1 ins_arr, sub2)
      else if Array.length sub2 + Array.length ins_arr <= Config.target_length
      then L2 (sub1, Array.append ins_arr sub2)
      else
        (* Array must be split into 3 different parts. *)
        N3 (N0 sub1, N0 ins_arr, N0 sub2)

  let sub_leaf start_idx end_idx arr acc =
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

  let del_leaf start_idx end_idx arr =
    if start_idx <= 0 && end_idx >= Array.length arr then (* In range. *)
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
end
