type rope =
  | N0 of string
  | N1 of rope
  | N2 of rope * int * rope
  (* Aux constructors. *)
  | L2 of string * string
  | N3 of rope * rope * rope

type t = rope

let target_length = 1024
let empty = N0 ""
let of_string string = N0 string

let rec fold_right f state = function
  | N0 s -> f state s
  | N1 t -> fold_right f state t
  | N2 (l, _, r) ->
      let state = fold_right f state r in
      fold_right f state l
  | _ -> failwith "mem_rope fold_right failure"

let to_string rope =
  let lst = fold_right (fun acc str -> str :: acc) [] rope in
  String.concat "" lst

let rec size acc = function
  | N0 s -> acc + String.length s
  | N1 t -> size acc t
  | N2 (_, lm, r) -> size (acc + lm) r
  | _ -> failwith "mem_rope failed in function size"

let size t = size 0 t

let root = function
  | L2 (s1, s2) -> N2 (N0 s1, String.length s1, N0 s2)
  | N3 (t1, t2, t3) ->
      let t1_size = size t1 in
      let left = N2 (t1, t1_size, t2) in
      let t2_size = size t2 in
      N2 (left, t1_size + t2_size, N1 t3)
  | t -> t

let n1 = function
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

let rec ins cur_index string = function
  | N0 str ->
      if cur_index <= 0 then
        if String.length str + String.length string <= target_length then
          N0 (string ^ str)
        else L2 (string, str)
      else if cur_index >= String.length str then
        if String.length str + String.length string <= target_length then
          N0 (str ^ string)
        else L2 (str, string)
      else if String.length str + String.length string <= target_length then
        let new_str = String_join.join_three str string cur_index in
        N0 new_str
        (* All if-staments below are if concatenating into a single string exceeds target_length. *)
      else if
        (* If first half of old string + insert string does not exceed target_length. *)
        cur_index + String.length string <= target_length
      then
        let sub1 = String_join.join_start str string cur_index in
        let sub2 = String.sub str cur_index (String.length str - cur_index) in
        L2 (sub1, sub2)
        (* If second half olf old + insert string does not exceed target length. *)
      else if
        String.length str - cur_index + String.length string <= target_length
      then
        (* Get first substring. *)
        let sub1 = String.sub str 0 cur_index in
        let sub2 = String_join.join_last str string cur_index in
        L2 (sub1, sub2)
      else
        (* String must be split into 3 different parts. *)
        let sub1 = String.sub str 0 cur_index in
        let sub2 = String.sub str cur_index (String.length str - cur_index) in
        N3 (N0 sub1, N0 string, N0 sub2)
  | N1 t -> n1 (ins cur_index string t)
  | N2 (l, lm, r) ->
      if cur_index < lm then ins_n2_left (ins cur_index string l) r
      else ins_n2_right l (ins (cur_index - lm) string r)
  | _ -> failwith ""

let insert index string rope = root (ins index string rope)

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
      if did_ins then (n1 t, true) else (N1 t, false)
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
  if did_ins then root rope else rope
