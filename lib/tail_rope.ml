type rope =
  | N0 of string
  | N1 of rope
  | N2 of rope * int * rope
  (* Aux constructors. *)
  | L2 of string * string
  | N3 of rope * rope * rope

type t = rope

(* Data for constructing rope, for tail recursion. *)
type bal_node =
  (* B1 parallels the N1 case, which is a signal to possibly call the n1 smart constructor. *)
  | B1
  (* B2Left contains left rope for N2 case, and possibly signals to call ins_n2_left or del_n2_left. *)
  | B2Left of rope
  (* Contains right rope for N2 case. Possibl xignal to call ins_n2_right or del_n2_left. *)
  | B2Right of rope

let target_length = 1024
let empty = N0 ""
let of_string string = N0 string

(* Tail recursive fold_right. *)
let rec fold_right lst f state = function
  | N0 s ->
      let state = f state s in
      fold_right_lst f state lst
  | N1 t -> fold_right lst f state t
  | N2 (l, _, r) -> fold_right (l :: lst) f state r
  | _ -> failwith "fold_right failure"

and fold_right_lst f state = function
  | [] -> state
  | x :: xs ->
      let state = fold_right xs f state x in
      fold_right_lst f state xs

let fold_right f state rope = fold_right [] f state rope

let to_string rope =
  let lst = fold_right (fun acc str -> str :: acc) [] rope in
  String.concat "" lst

let rec size acc = function
  | N0 s -> acc + String.length s
  | N1 t -> size acc t
  | N2 (_, lm, r) -> size (acc + lm) r
  | _ -> failwith "bal_rope failed in function size"

let size t = size 0 t

let ins_root = function
  | L2 (s1, s2) -> N2 (N0 s1, String.length s1, N0 s2)
  | N3 (t1, t2, t3) ->
      let t1_size = size t1 in
      let left = N2 (t1, t1_size, t2) in
      let t2_size = size t2 in
      N2 (left, t1_size + t2_size, N1 t3)
  | t -> t

let del_root = function N1 t -> t | t -> t

let ins_n1 = function
  | L2 (s1, s2) -> N2 (N0 s1, String.length s1, N0 s2)
  | N3 (t1, t2, t3) ->
      let t1_size = size t1 in
      let left = N2 (t1, t1_size, t2) in
      let t2_size = size t2 in
      N2 (left, t1_size + t2_size, N1 t3)
  | t -> N1 t

let ins_n2_left left right =
  match (left, right) with
  | L2 (s1, s2), t3 -> N3 (N0 s1, N0 s2, t3)
  | N3 (t1, t2, t3), N1 t4 ->
      let t1_size = size t1 in
      let left = N2 (t1, t1_size, t2) in
      let t3_size = size t3 in
      let right = N2 (t3, t3_size, t4) in
      let t2_size = size t2 in
      N2 (left, t1_size + t2_size, right)
  | N3 (t1, t2, t3), t4 ->
      let left = N2 (t1, size t1, t2) in
      N3 (left, N1 t3, t4)
  | l, r -> N2 (l, size l, r)

let del_n2_left left right =
  match (left, right) with
  | N1 t1, N1 t2 -> N1 (N2 (t1, size t1, t2))
  | N1 (N1 t1), N2 (N1 t2, _, (N2 _ as t3)) ->
      let left = N2 (t1, size t1, t2) in
      let left_size = size left in
      let inner = N2 (left, left_size, t3) in
      N1 inner
  | N1 (N1 t1), N2 (N2 (t2, _, t3), _, N1 t4) ->
      let left = N2 (t1, size t1, t2) in
      let right = N2 (t3, size t3, t4) in
      let inner = N2 (left, size left, right) in
      N1 inner
  | N1 (N1 _ as t1), N2 ((N2 _ as t2), _, (N2 _ as t3)) ->
      let left = N2 (t1, size t1, t2) in
      let right = N1 t3 in
      N2 (left, size left, right)
  | l, r -> N2 (l, size l, r)

let ins_n2_right left right =
  match (left, right) with
  | t1, L2 (s1, s2) -> N3 (t1, N0 s1, N0 s2)
  | N1 t1, N3 (t2, t3, t4) ->
      let t1_size = size t1 in
      let left = N2 (t1, t1_size, t2) in
      let t3_size = size t3 in
      let right = N2 (t3, t3_size, t4) in
      let t2_size = size t2 in
      N2 (left, t1_size + t2_size, right)
  | t1, N3 (t2, t3, t4) ->
      let right = N2 (t3, size t3, t4) in
      N3 (t1, N1 t2, right)
  | l, r -> N2 (l, size l, r)

let del_n2_right left right =
  match (left, right) with
  | N2 (N1 t1, _, N2 (t2, _, t3)), N1 (N1 t4) ->
      let left = N2 (t1, size t1, t2) in
      let right = N2 (t3, size t3, t4) in
      let inner = N2 (left, size left, right) in
      N1 inner
  | N2 ((N2 _ as t1), lm, N1 t2), N1 (N1 t3) ->
      let right = N2 (t2, size t2, t3) in
      let inner = N2 (t1, lm, right) in
      N1 inner
  | N2 ((N2 _ as t1), _, (N2 _ as t2)), N1 (N1 _ as t3) ->
      let left_size = size t1 in
      let left = N1 t1 in
      let right = N2 (t2, size t2, t3) in
      N2 (left, left_size, right)
  | l, r -> N2 (l, size l, r)

let rec ins_added_node rope lst =
  match lst with
  | x :: xs ->
      let rope =
        match x with
        | B1 -> ins_n1 rope
        | B2Left l -> ins_n2_right l rope
        | B2Right r -> ins_n2_left rope r
      in
      ins_added_node rope xs
  | [] -> ins_root rope

let rec ins_deleted_node rope lst =
  match lst with
  | x :: xs ->
      let rope =
        match x with
        | B1 -> N1 rope
        | B2Left l -> del_n2_right l rope
        | B2Right r -> del_n2_left rope r
      in
      ins_deleted_node rope xs
  | [] -> del_root rope

let rec ins_no_action rope lst =
  match lst with
  | x :: xs ->
      let rope =
        match x with
        | B1 -> N1 rope
        | B2Left l ->
            let lm = size l in
            N2 (l, lm, rope)
        | B2Right r -> N2 (rope, size rope, r)
      in
      ins_no_action rope xs
  | [] -> rope

let ins_no_action rope lst =
  match lst with
  | x :: xs -> (
      match x with
      | B2Left l -> (
          match (l, rope) with
          | N0 s1, N0 s2 ->
              if String.length s1 + String.length s2 <= target_length then
                let rope = N0 (s1 ^ s2) in
                ins_deleted_node rope xs
              else ins_no_action rope lst
          | _ -> ins_no_action rope lst)
      | B2Right r -> (
          match (rope, r) with
          | N0 s1, N0 s2 ->
              if String.length s1 + String.length s2 <= target_length then
                let rope = N0 (s1 ^ s2) in
                ins_deleted_node rope xs
              else ins_no_action rope lst
          | _ -> ins_no_action rope lst)
      | _ -> ins_no_action rope lst)
  | [] -> rope

let rec ins cur_index string lst = function
  | N0 str ->
      if cur_index <= 0 then
        if String.length str + String.length string <= target_length then
          let rope = N0 (string ^ str) in
          ins_no_action rope lst
        else
          let rope = L2 (string, str) in
          ins_added_node rope lst
      else if cur_index >= String.length str then
        if String.length str + String.length string <= target_length then
          let rope = N0 (str ^ string) in
          ins_no_action rope lst
        else
          let rope = L2 (str, string) in
          ins_added_node rope lst
      else if String.length str + String.length string <= target_length then
        let new_str = String_join.join_three str string cur_index in
        let rope = N0 new_str in
        ins_no_action rope lst
        (* All if-staments below are if concatenating into a single string exceeds target_length. *)
      else if
        (* If first half of old string + insert string does not exceed target_length. *)
        cur_index + String.length string <= target_length
      then
        let sub1 = String_join.join_start str string cur_index in
        let sub2 = String.sub str cur_index (String.length str - cur_index) in
        let rope = L2 (sub1, sub2) in
        ins_added_node rope lst
        (* If second half olf old + insert string does not exceed target length. *)
      else if
        String.length str - cur_index + String.length string <= target_length
      then
        (* Get first substring. *)
        let sub1 = String.sub str 0 cur_index in
        let sub2 = String_join.join_last str string cur_index in
        let rope = L2 (sub1, sub2) in
        ins_added_node rope lst
      else
        (* String must be split into 3 different parts. *)
        let sub1 = String.sub str 0 cur_index in
        let sub2 = String.sub str cur_index (String.length str - cur_index) in
        let left = N2 (N0 sub1, String.length sub1, N0 string) in
        let right = N0 sub2 in
        let rope = ins_n2_left left right in
        ins_added_node rope lst
  | N1 t ->
      let lst = B1 :: lst in
      ins cur_index string lst t
  | N2 (l, lm, r) ->
      if cur_index < lm then
        let lst = B2Right r :: lst in
        ins cur_index string lst l
      else
        let lst = B2Left l :: lst in
        ins (cur_index - lm) string lst r
  | _ -> failwith ""

let insert index string rope = ins index string [] rope

let rec del_internal start_idx end_idx = function
  | N0 str ->
      if start_idx <= 0 && end_idx >= String.length str then
        (* In range. *)
        (N0 "", false)
      else if start_idx >= 0 && end_idx <= String.length str then
        (* In middle of this node. *)
        if start_idx + (String.length str - end_idx) <= target_length then
          let str = String_join.del_middle start_idx end_idx str in
          (N0 str, false)
        else
          let sub1 = String.sub str 0 start_idx in
          let sub2 = String.sub str end_idx (String.length str - end_idx) in
          (L2 (sub1, sub2), true)
      else if start_idx >= 0 && end_idx >= String.length str then
        (* Starts at this node. *)
        let str = String.sub str 0 start_idx in
        (N0 str, false)
      else
        (* Ends at this node. *)
        let str = String.sub str end_idx (String.length str - end_idx) in
        (N0 str, false)
  | N1 t ->
      let t, did_ins = del_internal start_idx end_idx t in
      if did_ins then (ins_n1 t, true) else (N1 t, false)
  | N2 (l, lm, r) ->
      if lm > start_idx && lm > end_idx then
        let l, did_ins = del_internal start_idx end_idx l in
        match did_ins with
        | false -> (N2 (l, size l, r), false)
        | true -> (ins_n2_left l r, true)
      else if lm < start_idx && lm < end_idx then
        let r, did_ins = del_internal (start_idx - lm) (end_idx - lm) r in
        match did_ins with
        | false -> (N2 (l, lm, r), false)
        | true -> (ins_n2_right l r, true)
      else
        (* It is only possible for did_ins to be true for one side as it only happens when deleting at the middle of a node. *)
        let r, did_ins_r = del_internal (start_idx - lm) (end_idx - lm) r in
        let l, did_ins_l = del_internal start_idx end_idx l in
        if did_ins_l then (ins_n2_left l r, true)
        else if did_ins_r then (ins_n2_right l r, true)
        else (N2 (l, size l, r), false)
  | _ -> failwith ""

let delete start length rope =
  let rope, did_ins = del_internal start (start + length) rope in
  if did_ins then ins_root rope else rope

let rec height = function
  | N0 _ -> 0
  | N1 t -> height t + 1
  | N2 (l, _, r) -> Int.max (height l) (height r) + 1
  | _ -> failwith ""

let height rope =
  match rope with
  | N2 (l, _, r) ->
      let left = height l in
      let right = height r in
      let _ = Printf.printf "\ntry_height   l: %i; r: %i\n" left right in
      let diff = if left < right then right - left else left - right in
      let _ = Printf.printf "abs_height: %i\n\n" diff in
      height rope
  | _ -> height rope

let rec count_balance = function
  | N0 _ -> 0
  | N1 t -> count_balance t + 1
  | N2 (l, _, r) ->
      let l = count_balance l + 1 in
      let r = count_balance r + 1 in
      l + r
  | _ -> failwith ""

let count_balance rope =
  let num_nodes = count_balance rope in
  Printf.printf "num nodes: %i" num_nodes
