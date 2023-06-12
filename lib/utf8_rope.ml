type rope =
  | N0 of string
  | N1 of rope
  | N2 of { l : rope; lm : int; rm : int; r : rope }
  (* Aux constructors. *)
  | L2 of string * string
  | N3 of rope * rope * rope

type t = rope

(* We accept and don't modify strings with a length of more than 1024,
   but we don't build any strings longer than that ourselves.
   The target_length has performance implications and 1024 seems like a good size from benchmarks. *)
let target_length = 1024
let empty = N0 ""
let of_string string = N0 string

let rec size = function
  | N0 s -> String.length s
  | N1 t -> size t
  | N2 { lm; rm; _ } -> lm + rm
  | N3 (t1, t2, t3) -> size t1 + size t2 + size t3
  | _ -> failwith ""

let root = function
  | L2 (s1, s2) ->
      N2 { l = N0 s1; lm = String.length s1; rm = String.length s2; r = N0 s2 }
  | N3 (t1, t2, t3) ->
      let t1_size = size t1 in
      let t2_size = size t2 in
      let left = N2 { l = t1; lm = t1_size; rm = t2_size; r = t2 } in
      N2 { l = left; lm = t1_size + t2_size; rm = size t3; r = N1 t3 }
  | t -> t

let n1 = function
  | L2 (s1, s2) ->
      N2 { l = N0 s1; lm = String.length s1; rm = String.length s2; r = N0 s2 }
  | N3 (t1, t2, t3) ->
      let t1_size = size t1 in
      let t2_size = size t2 in
      let left = N2 { l = t1; lm = t1_size; rm = t2_size; r = t2 } in
      N2 { l = left; lm = t1_size + t2_size; rm = size t3; r = N1 t3 }
  | t -> N1 t

let ins_n2_left left right =
  match (left, right) with
  | L2 (s1, s2), t3 -> N3 (N0 s1, N0 s2, t3)
  | N3 (t1, t2, t3), N1 t4 ->
      let t1_size = size t1 in
      let t2_size = size t2 in
      let left = N2 { l = t1; lm = t1_size; rm = t2_size; r = t2 } in
      let t3_size = size t3 in
      let t4_size = size t4 in
      let right = N2 { l = t3; lm = t3_size; rm = t4_size; r = t4 } in
      N2 { l = left; lm = t1_size + t2_size; rm = t3_size + t4_size; r = right }
  | N3 (t1, t2, t3), (N2 _ as t4) ->
      N3 (N2 { l = t1; lm = size t1; rm = size t2; r = t2 }, N1 t3, t4)
  | N3 (t1, t2, t3), t4 ->
      let t1_size = size t1 in
      let t2_size = size t2 in
      let left = N2 { l = t1; lm = t1_size; rm = t2_size; r = t2 } in
      let t3_size = size t3 in
      let t4_size = size t4 in
      let right = N2 { l = t3; lm = t3_size; rm = t4_size; r = t4 } in
      N2 { l = left; lm = t1_size + t2_size; rm = t3_size + t4_size; r = right }
  | l, r -> N2 { l; lm = size l; rm = size r; r }

let ins_n2_right left right =
  match (left, right) with
  | t1, L2 (s1, s2) -> N3 (t1, N0 s1, N0 s2)
  | N1 t1, N3 (t2, t3, t4) ->
      let t1_size = size t1 in
      let t2_size = size t2 in
      let left = N2 { l = t1; lm = t1_size; rm = t2_size; r = t2 } in
      let t3_size = size t3 in
      let t4_size = size t4 in
      let right = N2 { l = t3; lm = t3_size; rm = t4_size; r = t4 } in
      N2 { l = left; lm = t1_size + t2_size; rm = t3_size + t4_size; r = right }
  | (N2 _ as t1), N3 (t2, t3, t4) ->
      N3 (t1, N1 t2, N2 { l = t3; lm = size t3; rm = size t4; r = t4 })
  | t1, N3 (t2, t3, t4) ->
      let t1_size = size t1 in
      let t2_size = size t2 in
      let left = N2 { l = t1; lm = t1_size; rm = t2_size; r = t2 } in
      let t3_size = size t3 in
      let t4_size = size t4 in
      let right = N2 { l = t3; lm = t3_size; rm = t4_size; r = t4 } in
      N2 { l = left; lm = t1_size + t2_size; rm = t3_size + t4_size; r = right }
  | l, r -> N2 { l; lm = size l; rm = size r; r }

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
      else
        let sub1 = String.sub str 0 cur_index in
        let sub2 = String.sub str cur_index (String.length str - cur_index) in
        if String.length str + String.length string <= target_length then
          N0 (sub1 ^ string ^ sub2)
        else if String.length sub1 + String.length string <= target_length then
          L2 (sub1 ^ string, sub2)
        else if String.length sub2 + String.length string <= target_length then
          L2 (sub1, string ^ sub2)
        else
          (* String must be split into 3 different parts. *)
          N3 (N0 sub1, N0 string, N0 sub2)
  | N1 t -> n1 (ins cur_index string t)
  | N2 { l; lm; r; _ } ->
      if cur_index < lm then ins_n2_left (ins cur_index string l) r
      else ins_n2_right l (ins (cur_index - lm) string r)
  | _ -> failwith ""

let insert index string rope = root (ins index string rope)

let rec sub_internal start_idx end_idx acc = function
  | N0 str ->
      if start_idx <= 0 && end_idx >= String.length str then
        (* In range. *)
        str :: acc
      else if start_idx >= 0 && end_idx <= String.length str then
        (* In middle of this node. *)
        let str = String.sub str start_idx (end_idx - start_idx) in
        str :: acc
      else if start_idx >= 0 && end_idx >= String.length str then
        (* Starts at this node. *)
        let str = String.sub str start_idx (String.length str - start_idx) in
        str :: acc
      else
        (* Ends at this node. *)
        let str = String.sub str 0 end_idx in
        str :: acc
  | N1 t -> sub_internal start_idx end_idx acc t
  | N2 { l; lm; r; _ } ->
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
  sub_internal start (start + length) [] rope |> String.concat ""

(*
    Deletion involves deleting strings within nodes rather deleting the nodes themselves,
    as this helps better maintain balancing.
    A deletion may actually insert a new node into the 1-2 Brother Tree in one case.
    This happens only when the two following conditions are true:
      (a) We are deleting the middle of a string that is longer than target_length.
      (b) Joining the two strings back together would result in a string still longer than target_length.
    We propagate a boolean indicating if this happened, and call the insert rebalancing operations if it did.
*)
let rec del_internal start_idx end_idx = function
  | N0 str ->
      if start_idx <= 0 && end_idx >= String.length str then
        (* In range. *)
        (N0 "", false)
      else if start_idx >= 0 && end_idx <= String.length str then
        (* In middle of this node. *)
        let sub1 = String.sub str 0 start_idx in
        let sub2 = String.sub str end_idx (String.length str - end_idx) in
        if String.length sub1 + String.length sub2 <= target_length then
          (N0 (sub1 ^ sub2), false)
        else (L2 (sub1, sub2), true)
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
  | N2 (l, lm, rm, r) ->
      if lm > start_idx && lm > end_idx then
        let l, did_ins = del_internal start_idx end_idx l in
        match did_ins with
        | false -> (N2 (l, size l, rm, r), false)
        | true -> (ins_n2_left l r, true)
      else if lm < start_idx && lm < end_idx then
        let r, did_ins = del_internal (start_idx - lm) (end_idx - lm) r in
        match did_ins with
        | false -> (N2 (l, lm, size r, r), false)
        | true -> (ins_n2_right l r, true)
      else
        (* It is only possible for did_ins to be true for one side as it only happens when deleting at the middle of a node. *)
        let r, did_ins_r = del_internal (start_idx - lm) (end_idx - lm) r in
        let l, did_ins_l = del_internal start_idx end_idx l in
        if did_ins_l then (ins_n2_left l r, true)
        else if did_ins_r then (ins_n2_right l r, true)
        else (N2 (l, size l, size r, r), false)
  | _ -> failwith ""

let delete start length rope =
  let rope, did_ins = del_internal start (start + length) rope in
  if did_ins then root rope else rope

let rec fold f state = function
  | N0 "" -> state
  | N0 str -> f state str
  | N1 t -> fold f state t
  | N2 { l; r; _ } ->
      let state = fold f state l in
      fold f state r
  | _ -> failwith ""

let rec fold_back f state = function
  | N0 "" -> state
  | N0 str -> f state str
  | N1 t -> fold_back f state t
  | N2 { l; r; _ } ->
      let state = fold_back f state r in
      fold_back f state l
  | _ -> failwith ""

let to_string rope =
  let lst = fold_back (fun lst str -> str :: lst) [] rope in
  String.concat "" lst
