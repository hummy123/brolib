type rope =
  | N0 of string
  | N1 of rope
  | N2 of rope * int * int * rope
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
  | N2 (_, lm, rm, _) -> lm + rm
  | N3 (t1, t2, t3) -> size t1 + size t2 + size t3
  | _ -> failwith ""

let root = function
  | L2 (s1, s2) -> N2 (N0 s1, String.length s1, String.length s2, N0 s2)
  | N3 (t1, t2, t3) ->
      let t1_size = size t1 in
      let t2_size = size t2 in
      let left = N2 (t1, t1_size, t2_size, t2) in
      N2 (left, t1_size + t2_size, size t3, N1 t3)
  | t -> t

let n1 = function
  | L2 (s1, s2) -> N2 (N0 s1, String.length s1, String.length s2, N0 s2)
  | N3 (t1, t2, t3) ->
      let t1_size = size t1 in
      let t2_size = size t2 in
      let left = N2 (t1, t1_size, t2_size, t2) in
      N2 (left, t1_size + t2_size, size t3, N1 t3)
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
  | t1, L2 (s1, s2) -> N3 (t1, N0 s1, N0 s2)
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
        (* Create mutable byte array containing length of both strings. *)
        let bytes = Bytes.create (String.length str + String.length string) in
        (* Copy the first half of the old string to the start of the Bytes. *)
        let _ = Bytes.unsafe_blit_string str 0 bytes 0 cur_index in
        (* Copy the second half of the old string to the end of the Bytes. *)
        let _ =
          Bytes.unsafe_blit_string str cur_index bytes
            (cur_index + String.length string)
            (String.length str - cur_index)
        in
        (* Copy the newly inserted string to the middle of the Bytes. *)
        let _ =
          Bytes.unsafe_blit_string string 0 bytes cur_index
            (String.length string)
        in
        (* Return the Bytes as a string. This is still outwardly immutable,
           because the mutable bytes were created in this function and returned as an immutable string
           by this function too. *)
        N0 (Bytes.unsafe_to_string bytes)
        (* All if-staments below are if concatenating into a single string exceeds target_length. *)
      else if
        (* If first half of old string + insert string does not exceed target_length. *)
        cur_index + String.length string <= target_length
      then
        let bytes = Bytes.create (cur_index + String.length string) in
        (* Add first half of old string to Bytes. *)
        let _ = Bytes.unsafe_blit_string str 0 bytes 0 cur_index in
        (* Add insert string to Bytes. *)
        let _ =
          Bytes.unsafe_blit_string string 0 bytes cur_index
            (String.length string)
        in
        let sub2 = String.sub str cur_index (String.length str - cur_index) in
        L2 (Bytes.unsafe_to_string bytes, sub2)
        (* If second half olf old + insert string does not exceed target length. *)
      else if
        String.length str - cur_index + String.length string <= target_length
      then
        (* Get first substring. *)
        let sub1 = String.sub str 0 cur_index in
        let bytes =
          Bytes.create (String.length string + (String.length str - cur_index))
        in
        let _ =
          Bytes.unsafe_blit_string string 0 bytes 0 (String.length string)
        in
        let _ =
          Bytes.unsafe_blit_string str cur_index bytes (String.length string)
            (String.length str - cur_index)
        in
        L2 (sub1, Bytes.unsafe_to_string bytes)
      else
        (* String must be split into 3 different parts. *)
        let sub1 = String.sub str 0 cur_index in
        let sub2 = String.sub str cur_index (String.length str - cur_index) in
        N3 (N0 sub1, N0 string, N0 sub2)
  | N1 t -> n1 (ins cur_index string t)
  | N2 (l, lm, _, r) ->
      if cur_index < lm then ins_n2_left (ins cur_index string l) r
      else ins_n2_right l (ins (cur_index - lm) string r)
  | _ -> failwith ""

let insert index string rope = root (ins index string rope)

let rec prepend_internal string = function
  | N0 str ->
      if String.length str + String.length string <= target_length then
        N0 (string ^ str)
      else L2 (string, str)
  | N1 t -> n1 (prepend_internal string t)
  | N2 (l, _, _, r) -> ins_n2_left (prepend_internal string l) r
  | _ -> failwith ""

let prepend string rope = root (prepend_internal string rope)

let rec append_internal string = function
  | N0 str ->
      if String.length str + String.length string <= target_length then
        N0 (str ^ string)
      else L2 (str, string)
  | N1 t -> n1 (append_internal string t)
  | N2 (l, _, _, r) -> ins_n2_right l (append_internal string r)
  | _ -> failwith ""

let append string rope = root (append_internal string rope)

let rec sub_string_internal start_idx end_idx acc = function
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
  | N1 t -> sub_string_internal start_idx end_idx acc t
  | N2 (l, lm, _, r) ->
      (* Cases we need to consider.
         1. start_idx and end_idx are in same directions (both less than or both greater than weight).
         2. start_idx and end_idx are in different direction (start_idx is less than weight while end_idx is less than weight.)
      *)
      if lm > start_idx && lm > end_idx then
        sub_string_internal start_idx end_idx acc l
      else if lm < start_idx && lm < end_idx then
        sub_string_internal (start_idx - lm) (end_idx - lm) acc r
      else
        let acc = sub_string_internal (start_idx - lm) (end_idx - lm) acc r in
        sub_string_internal start_idx end_idx acc l
  | _ -> failwith ""

let sub_string start length rope =
  sub_string_internal start (start + length) [] rope |> String.concat ""

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
        if start_idx + (String.length str - end_idx) <= target_length then
          let bytes =
            Bytes.create (start_idx + (String.length str - end_idx))
          in
          let _ = Bytes.unsafe_blit_string str 0 bytes 0 start_idx in
          let _ =
            Bytes.unsafe_blit_string str end_idx bytes start_idx
              (String.length str - end_idx)
          in
          (N0 (Bytes.unsafe_to_string bytes), false)
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
  | N2 (l, _, _, r) ->
      let state = fold f state l in
      fold f state r
  | _ -> failwith ""

let rec fold_back f state = function
  | N0 "" -> state
  | N0 str -> f state str
  | N1 t -> fold_back f state t
  | N2 (l, _, _, r) ->
      let state = fold_back f state r in
      fold_back f state l
  | _ -> failwith ""

(* Applies a function f to all non-empty strings in the rope before index.
   The traversal order is frmm right-to-left; the highest index first and then
   the previous indices after. *)
let rec fold_right_before_index f state index is_before_index = function
  | N0 "" -> state
  | N0 str as node ->
      if is_before_index then f state str
      else
        let sub = sub_string 0 index node in
        f state sub
  | N1 t -> fold_right_before_index f state index is_before_index t
  | N2 (l, lm, _, r) ->
      if index < lm then fold_right_before_index f state index is_before_index l
      else
        let state = fold_right_before_index f state (index - lm) false r in
        fold_right_before_index f state index true l
  | _ -> failwith ""

let fold_right_before_index f state index rope =
  fold_right_before_index f state index false rope

(* Applies a function f to all non-empty strings in the rope before index.
   The traversal order is frmm left-to-right; the lowest index first and then
   the higher indices after. *)
let rec fold_left_before_index f state index is_before_index = function
  | N0 "" -> state
  | N0 str as node ->
      if is_before_index then f state str
      else
        let sub = sub_string 0 index node in
        f state sub
  | N1 t -> fold_left_before_index f state index is_before_index t
  | N2 (l, lm, _, r) ->
      if index < lm then fold_left_before_index f state index is_before_index l
      else
        let state = fold_left_before_index f state index true l in
        fold_left_before_index f state (index - lm) is_before_index r
  | _ -> failwith ""

let fold_left_before_index f state index rope =
  fold_left_before_index f state index false rope

let rec fold_after_index f state index = function
  | N0 "" -> state
  | N0 str ->
      if index = -1 then f state str
      else
        let sub = String.sub str index (String.length str - index) in
        f state sub
  | N1 t -> fold_after_index f state index t
  | N2 (l, lm, _, r) ->
      if index < lm then
        let state = fold_after_index f state index l in
        fold_after_index f state (-1) r
      else fold_after_index f state (index - lm) r
  | _ -> failwith ""

let to_string rope =
  let bytes = Bytes.create (size rope) in
  let _ =
    fold
      (fun pos str ->
        Bytes.unsafe_blit_string str 0 bytes pos (String.length str);
        pos + String.length str)
      0 rope
  in
  Bytes.unsafe_to_string bytes

type rope_stats = { utf8_length : int }

let stats rope =
  let utf8_length = size rope in
  { utf8_length }

let flatten rope = fold (fun rope str -> append str rope) rope

(* Implementation of some functions from the String.module, but on Tiny_rope. *)
let starts_with ~prefix rope =
  let str = sub_string 0 (String.length prefix) rope in
  str = prefix

let ends_with ~suffix rope =
  let str =
    sub_string (size rope - String.length suffix) (String.length suffix) rope
  in
  str = suffix

let rindex_from_opt rope ~before_index chr =
  let result =
    fold_right_before_index
      (fun (idx, acc) str ->
        let start_idx = idx - String.length str in
        let acc =
          match acc with
          | Some _ as result -> result
          | None -> (
              match String.rindex_from_opt str (String.length str - 1) chr with
              | Some found_idx -> Some (start_idx + found_idx)
              | None -> None)
        in
        (start_idx, acc))
      (before_index, None) before_index rope
  in
  match result with _, Some idx -> Some idx | _ -> None
