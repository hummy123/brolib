(* Define Unicode character operations first. *)
let utf8_length chr =
  match chr with
  | '\x00' .. '\x7f' -> 1
  | '\xc0' .. '\xdf' -> 2
  | '\xe0' .. '\xef' -> 3
  | '\xf0' .. '\xf7' -> 4
  | _ -> failwith "invalid utf-8 start"

let utf16_length chr =
  match chr with
  | '\x00' .. '\xef' -> 1
  | '\xf0' .. '\xf7' -> 2
  | _ -> failwith "invalid utf-8 start"

let rec count_string_stats string u8_pos u16_ctr u32_ctr =
  if u8_pos = String.length string then (u8_pos, u16_ctr, u32_ctr)
  else
    let chr = String.unsafe_get string u8_pos in
    let u8_length = utf8_length chr in
    let u16_length = utf16_length chr in
    (* utf32_length is always 1 because we're going through the screen by code points. *)
    count_string_stats string (u8_pos + u8_length) (u16_ctr + u16_length)
      (u32_ctr + 1)

type rope =
  | N0 of string
  | N1 of rope
  (*
      Using { } for N2 constructor because labels are helpful when there are many fields.
      Another, cleaner option is to define separate metadata type and use that, but
      that can increase indirection and thus decrease performance which I don't want.
  *)
  | N2 of {
      l : rope;
      lm8 : int;
      lm16 : int;
      lm32 : int;
      rm8 : int;
      rm16 : int;
      rm32 : int;
      r : rope;
    }
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
  | N0 s -> count_string_stats s 0 0 0
  | N1 t -> size t
  | N2 { lm8; lm16; lm32; rm8; rm16; rm32; _ } ->
      (lm8 + rm8, lm16 + rm16, lm32 + rm32)
  | N3 (t1, t2, t3) ->
      let t1_8, t1_16, t1_32 = size t1 in
      let t2_8, t2_16, t2_32 = size t2 in
      let t3_8, t3_16, t3_32 = size t3 in
      (t1_8 + t2_8 + t3_8, t1_16 + t2_16 + t3_16, t1_32 + t2_32 + t3_32)
  | _ -> failwith ""

let root = function
  | L2 (s1, s2) ->
      let lm8, lm16, lm32 = count_string_stats s1 0 0 0 in
      let rm8, rm16, rm32 = count_string_stats s2 0 0 0 in
      N2 { l = N0 s1; lm8; lm16; lm32; rm8; rm16; rm32; r = N0 s2 }
  | N3 (t1, t2, t3) ->
      let lm8, lm16, lm32 = size t1 in
      let rm8, rm16, rm32 = size t2 in
      let left = N2 { l = t1; lm8; lm16; lm32; rm8; rm16; rm32; r = t2 } in
      (* lm variables below refer to size of "left" variable using N2 constructor in line above. *)
      let lm8 = lm8 + rm8 in
      let lm16 = lm16 + rm16 in
      let lm32 = lm32 + rm32 in
      let rm8, rm16, rm32 = size t3 in
      N2 { l = left; lm8; lm16; lm32; rm8; rm16; rm32; r = N1 t3 }
  | t -> t

let n1 = function
  | L2 (s1, s2) ->
      let lm8, lm16, lm32 = count_string_stats s1 0 0 0 in
      let rm8, rm16, rm32 = count_string_stats s2 0 0 0 in
      N2 { l = N0 s1; lm8; lm16; lm32; rm8; rm16; rm32; r = N0 s2 }
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
  | N2 { l; lm; rm; r } ->
      if lm > start_idx && lm > end_idx then
        let l, did_ins = del_internal start_idx end_idx l in
        match did_ins with
        | false -> (N2 { l; lm = size l; rm; r }, false)
        | true -> (ins_n2_left l r, true)
      else if lm < start_idx && lm < end_idx then
        let r, did_ins = del_internal (start_idx - lm) (end_idx - lm) r in
        match did_ins with
        | false -> (N2 { l; lm; rm = size r; r }, false)
        | true -> (ins_n2_right l r, true)
      else
        (* It is only possible for did_ins to be true for one side as it only happens when deleting at the middle of a node. *)
        let r, did_ins_r = del_internal (start_idx - lm) (end_idx - lm) r in
        let l, did_ins_l = del_internal start_idx end_idx l in
        if did_ins_l then (ins_n2_left l r, true)
        else if did_ins_r then (ins_n2_right l r, true)
        else (N2 { l; lm = size l; rm = size r; r }, false)
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
