(* Functions for manipulating line break arrays. *)
let rec count_line_breaks ?(u8_pos = 0) ?(acc = []) ?(prev_is_cr = false) str =
  if u8_pos = String.length str then List.rev acc |> Array.of_list
  else
    let chr = String.unsafe_get str u8_pos in
    if chr = '\r' || (chr = '\n' && not prev_is_cr) then
      count_line_breaks ~u8_pos:(u8_pos + 1) ~acc:(u8_pos :: acc)
        ~prev_is_cr:(chr = '\r') str
    else count_line_breaks ~u8_pos:(u8_pos + 1) ~acc ~prev_is_cr:false str

let rec split_lines (find_num : int) lines low high =
  if Array.length lines = 0 then 0
  else
    let mid = low + ((high - low) / 2) in
    let mid_val = Array.unsafe_get lines mid in
    if high >= low then
      if mid_val = find_num then mid
      else if mid_val < find_num then split_lines find_num lines (mid + 1) high
      else split_lines find_num lines low (mid - 1)
    else mid

let sub_before mid_point lines =
  if Array.length lines = 0 then [||] else Array.sub lines 0 mid_point

let sub_after mid_point lines =
  if Array.length lines = 0 then [||]
  else if mid_point >= Array.length lines then [||]
  else Array.sub lines mid_point (Array.length lines - mid_point)

let align_to_end_of_line str pos =
  let chr = String.unsafe_get str pos in
  if String.length str - 1 > pos then
    if chr = '\r' && String.unsafe_get str (pos + 1) = '\n' then pos + 1
    else pos
  else pos

let align_to_after_line str pos =
  let chr = String.unsafe_get str pos in
  match chr with
  | '\n' -> pos + 1
  | '\r' -> if String.unsafe_get str (pos + 1) = '\n' then pos + 2 else pos + 1
  | _ -> pos

(* Like Array.map, but mutable.
   To keep the structure's data immutable, we only use Array.map
   when an array iwas previously (like with Array.sub or Array.copy). *)
let rec map ?(pos = 0) (f : int -> int) lines =
  if pos = Array.length lines then lines
  else
    let num = Array.unsafe_get lines pos in
    Array.unsafe_set lines pos (f num);
    map f lines ~pos:(pos + 1)

type rope =
  | N0 of { str : string; lines : int array }
  | N1 of rope
  | N2 of {
      l : rope;
      lm : int;
      lm_lines : int;
      rm : int;
      rm_lines : int;
      r : rope;
    }
  (* Aux constructors. *)
  | L2 of {
      s1 : string;
      s1_lines : int array;
      s2 : string;
      s2_lines : int array;
    }
  | N3 of rope * rope * rope

type t = rope

(* We accept and don't modify strings with a length of more than 1024,
   but we don't build any strings longer than that ourselves.
   The target_length has performance implications and 1024 seems like a good size from benchmarks. *)
let string_length = 1024
let array_length = 128
let empty = N0 { str = ""; lines = [||] }
let of_string string = N0 { str = string; lines = count_line_breaks string }

let rec size = function
  | N0 s -> (String.length s.str, Array.length s.lines)
  | N1 t -> size t
  | N2 { lm; rm; lm_lines; rm_lines; _ } -> (lm + rm, lm_lines + rm_lines)
  | N3 (t1, t2, t3) ->
      let idx1, lines1 = size t1 in
      let idx2, lines2 = size t2 in
      let idx3, lines3 = size t3 in
      (idx1 + idx2 + idx3, lines1 + lines2 + lines3)
  | _ -> failwith ""

let root = function
  | L2 { s1; s1_lines; s2; s2_lines } ->
      N2
        {
          l = N0 { str = s1; lines = s1_lines };
          lm = String.length s1;
          lm_lines = Array.length s1_lines;
          rm = String.length s2;
          rm_lines = Array.length s2_lines;
          r = N0 { str = s2; lines = s2_lines };
        }
  | N3 (t1, t2, t3) ->
      let t1_idx, t1_lines = size t1 in
      let t2_idx, t2_lines = size t2 in
      let t3_idx, t3_lines = size t3 in
      let left =
        N2
          {
            l = t1;
            lm = t1_idx;
            lm_lines = t1_lines;
            rm = t2_idx;
            rm_lines = t2_lines;
            r = t2;
          }
      in
      N2
        {
          l = left;
          lm = t1_idx + t2_idx;
          lm_lines = t1_lines + t2_lines;
          rm = t3_idx;
          rm_lines = t3_lines;
          r = N1 t3;
        }
  | t -> t

let n1 = function
  | L2 { s1; s1_lines; s2; s2_lines } ->
      N2
        {
          l = N0 { str = s1; lines = s1_lines };
          lm = String.length s1;
          lm_lines = Array.length s1_lines;
          rm = String.length s2;
          rm_lines = Array.length s2_lines;
          r = N0 { str = s2; lines = s2_lines };
        }
  | N3 (t1, t2, t3) ->
      let t1_idx, t1_lines = size t1 in
      let t2_idx, t2_lines = size t2 in
      let t3_idx, t3_lines = size t3 in
      let left =
        N2
          {
            l = t1;
            lm = t1_idx;
            lm_lines = t1_lines;
            rm = t2_idx;
            rm_lines = t2_lines;
            r = t2;
          }
      in
      N2
        {
          l = left;
          lm = t1_idx + t2_idx;
          lm_lines = t1_lines + t2_lines;
          rm = t3_idx;
          rm_lines = t3_lines;
          r = N1 t3;
        }
  | t -> N1 t

let ins_n2_left left right =
  match (left, right) with
  | L2 { s1; s1_lines; s2; s2_lines }, t3 ->
      N3
        ( N0 { str = s1; lines = s1_lines },
          N0 { str = s2; lines = s2_lines },
          t3 )
  | N3 (t1, t2, t3), N1 t4 ->
      let t1_idx, t1_lines = size t1 in
      let t2_idx, t2_lines = size t2 in
      let left =
        N2
          {
            l = t1;
            lm = t1_idx;
            lm_lines = t1_lines;
            rm = t2_idx;
            rm_lines = t2_lines;
            r = t2;
          }
      in
      let t3_idx, t3_lines = size t3 in
      let t4_idx, t4_lines = size t4 in
      let right =
        N2
          {
            l = t3;
            lm = t3_idx;
            lm_lines = t3_lines;
            rm = t4_idx;
            rm_lines = t4_lines;
            r = t4;
          }
      in
      N2
        {
          l = left;
          lm = t1_idx + t2_idx;
          lm_lines = t1_lines + t2_lines;
          rm = t3_idx + t4_idx;
          rm_lines = t3_lines + t4_lines;
          r = right;
        }
  | N3 (t1, t2, t3), (N2 _ as t4) ->
      let lm, lm_lines = size t1 in
      let rm, rm_lines = size t2 in
      N3 (N2 { l = t1; lm; lm_lines; rm; rm_lines; r = t2 }, N1 t3, t4)
  | N3 (t1, t2, t3), t4 ->
      let t1_idx, t1_lines = size t1 in
      let t2_idx, t2_lines = size t2 in
      let left =
        N2
          {
            l = t1;
            lm = t1_idx;
            lm_lines = t1_lines;
            rm = t2_idx;
            rm_lines = t2_lines;
            r = t2;
          }
      in
      let t3_idx, t3_lines = size t3 in
      let t4_idx, t4_lines = size t4 in
      let right =
        N2
          {
            l = t3;
            lm = t3_idx;
            lm_lines = t3_lines;
            rm = t4_idx;
            rm_lines = t4_lines;
            r = t4;
          }
      in
      N2
        {
          l = left;
          lm = t1_idx + t2_idx;
          lm_lines = t1_lines + t2_lines;
          rm = t3_idx + t4_idx;
          rm_lines = t3_lines + t4_lines;
          r = right;
        }
  | l, r ->
      let lm, lm_lines = size l in
      let rm, rm_lines = size r in
      N2 { l; lm; lm_lines; rm; rm_lines; r }

let ins_n2_right left right =
  match (left, right) with
  | t1, L2 { s1; s1_lines; s2; s2_lines } ->
      N3
        ( t1,
          N0 { str = s1; lines = s1_lines },
          N0 { str = s2; lines = s2_lines } )
  | N1 t1, N3 (t2, t3, t4) ->
      let t1_idx, t1_lines = size t1 in
      let t2_idx, t2_lines = size t2 in
      let left =
        N2
          {
            l = t1;
            lm = t1_idx;
            lm_lines = t1_lines;
            rm = t2_idx;
            rm_lines = t2_lines;
            r = t2;
          }
      in
      let t3_idx, t3_lines = size t3 in
      let t4_idx, t4_lines = size t4 in
      let right =
        N2
          {
            l = t3;
            lm = t3_idx;
            lm_lines = t3_lines;
            rm = t4_idx;
            rm_lines = t4_lines;
            r = t4;
          }
      in
      N2
        {
          l = left;
          lm = t1_idx + t2_idx;
          lm_lines = t1_lines + t2_lines;
          rm = t3_idx + t4_idx;
          rm_lines = t3_lines + t4_lines;
          r = right;
        }
  | (N2 _ as t1), N3 (t2, t3, t4) ->
      let lm, lm_lines = size t3 in
      let rm, rm_lines = size t4 in
      N3 (t1, N1 t2, N2 { l = t3; lm; lm_lines; rm; rm_lines; r = t4 })
  | t1, N3 (t2, t3, t4) ->
      let t1_idx, t1_lines = size t1 in
      let t2_idx, t2_lines = size t2 in
      let left =
        N2
          {
            l = t1;
            lm = t1_idx;
            lm_lines = t1_lines;
            rm = t2_idx;
            rm_lines = t2_lines;
            r = t2;
          }
      in
      let t3_idx, t3_lines = size t3 in
      let t4_idx, t4_lines = size t4 in
      let right =
        N2
          {
            l = t3;
            lm = t3_idx;
            lm_lines = t3_lines;
            rm = t4_idx;
            rm_lines = t4_lines;
            r = t4;
          }
      in
      N2
        {
          l = left;
          lm = t1_idx + t2_idx;
          lm_lines = t1_lines + t2_lines;
          rm = t3_idx + t4_idx;
          rm_lines = t3_lines + t4_lines;
          r = right;
        }
  | l, r ->
      let lm, lm_lines = size l in
      let rm, rm_lines = size r in
      N2 { l; lm; lm_lines; rm; rm_lines; r }

(* Since insertion logic can be complex due to storing line array,
   have small functions for handling different N0 insertion cases. *)

(* New string is before old. *)
let ins_before ins_string old_string old_lines =
  let ins_lines = count_line_breaks ins_string in
  if
    String.length old_string + String.length ins_string <= string_length
    && Array.length old_lines + Array.length ins_lines <= array_length
  then
    let old_lines =
      Array.map (fun x -> x + String.length ins_string) old_lines
    in
    N0
      {
        str = ins_string ^ old_string;
        lines = Array.append ins_lines old_lines;
      }
  else
    L2
      {
        s1 = ins_string;
        s1_lines = ins_lines;
        s2 = old_string;
        s2_lines = old_lines;
      }

(* New string is after old. *)
let ins_after ins_string old_string old_lines =
  let ins_lines = count_line_breaks ins_string in
  if
    String.length old_string + String.length ins_string <= string_length
    && Array.length old_lines + Array.length ins_lines <= array_length
  then
    let _ = map (fun x -> x + String.length old_string) ins_lines in
    let lines = Array.append old_lines ins_lines in
    N0 { str = old_string ^ ins_string; lines }
  else
    L2
      {
        s1 = old_string;
        s1_lines = old_lines;
        s2 = ins_string;
        s2_lines = ins_lines;
      }

let ins_middle ins_string old_string old_lines cur_index =
  let sub1 = String.sub old_string 0 cur_index in
  (* Line variables are "raw" and unedited from original array; may need to be modified in below if-statement. *)
  let mid_point =
    split_lines (String.length sub1) old_lines 0 (Array.length old_lines - 1)
  in
  let sub1_lines = sub_before mid_point old_lines in
  let sub2_lines = sub_after mid_point old_lines in
  let sub2 =
    String.sub old_string cur_index (String.length old_string - cur_index)
  in
  let ins_lines = count_line_breaks ins_string in
  if
    String.length old_string + String.length ins_string <= string_length
    && Array.length old_lines + Array.length ins_lines <= array_length
  then
    let _ = map (fun x -> x + String.length ins_string) sub2_lines in
    let _ = map (fun x -> x + String.length sub1) ins_lines in
    let lines =
      let start = Array.append sub1_lines ins_lines in
      Array.append start sub2_lines
    in
    N0 { str = sub1 ^ ins_string ^ sub2; lines }
  else if
    String.length sub1 + String.length ins_string <= string_length
    && Array.length sub1_lines + Array.length ins_lines <= array_length
  then
    let _ = map (fun x -> x + String.length sub1) ins_lines in
    let s1_lines = Array.append sub1_lines ins_lines in
    let _ = map (fun x -> x - String.length sub1) sub2_lines in
    L2 { s1 = sub1 ^ ins_string; s1_lines; s2 = sub2; s2_lines = sub2_lines }
  else if
    String.length sub2 + String.length ins_string <= string_length
    && Array.length sub2_lines + Array.length ins_lines <= array_length
  then
    let _ =
      map
        (fun x -> x - String.length sub1 + String.length ins_string)
        sub2_lines
    in
    L2
      {
        s1 = sub1;
        s1_lines = sub1_lines;
        s2 = ins_string ^ sub2;
        s2_lines = Array.append ins_lines sub2_lines;
      }
  else
    (* String must be split into 3 different parts. *)
    N3
      ( N0 { str = sub1; lines = sub1_lines },
        N0 { str = ins_string; lines = count_line_breaks ins_string },
        N0
          {
            str = sub2;
            lines = map (fun x -> x - String.length sub1) sub2_lines;
          } )

let rec ins cur_index ins_string = function
  | N0 { str = old_string; lines } ->
      if cur_index <= 0 then ins_before ins_string old_string lines
      else if cur_index >= String.length old_string then
        ins_after ins_string old_string lines
      else ins_middle ins_string old_string lines cur_index
  | N1 t -> n1 (ins cur_index ins_string t)
  | N2 { l; lm; r; _ } ->
      if cur_index < lm then ins_n2_left (ins cur_index ins_string l) r
      else ins_n2_right l (ins (cur_index - lm) ins_string r)
  | _ -> failwith ""

let insert index string rope = root (ins index string rope)

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
  | N0 { str; lines } ->
      if start_idx <= 0 && end_idx >= String.length str then
        (* In range. *)
        (empty, false)
      else if start_idx >= 0 && end_idx <= String.length str then
        (* In middle of this node. *)
        let sub1 = String.sub str 0 start_idx in
        let sub2 = String.sub str end_idx (String.length str - end_idx) in
        (* Raw, unedited array; sub2 may need to be mapped below.*)
        let start_point =
          split_lines start_idx lines 0 (Array.length lines - 1)
        in
        let sub1_lines = sub_before start_point lines in
        let end_point = split_lines end_idx lines 0 (Array.length lines - 1) in
        let sub2_lines = sub_after end_point lines in
        let difference = end_idx - start_idx in
        if
          String.length sub1 + String.length sub2 <= string_length
          && Array.length sub1_lines + Array.length sub2_lines <= array_length
        then
          let sub2_lines = map (fun x -> x - difference) sub2_lines in
          ( N0 { str = sub1 ^ sub2; lines = Array.append sub1_lines sub2_lines },
            false )
        else
          let sub2_lines =
            map
              (fun x ->
                if x >= start_idx then x - (String.length sub1 + difference)
                else x)
              sub2_lines
          in
          ( L2
              {
                s1 = sub1;
                s1_lines = sub1_lines;
                s2 = sub2;
                s2_lines = sub2_lines;
              },
            true )
      else if start_idx >= 0 && end_idx >= String.length str then
        (* Starts at this node. *)
        let str = String.sub str 0 start_idx in
        let mid_point =
          split_lines start_idx lines 0 (Array.length lines - 1)
        in
        let lines = sub_before mid_point lines in
        (N0 { str; lines }, false)
      else
        (* Ends at this node. *)
        let str = String.sub str end_idx (String.length str - end_idx) in
        let mid_point = split_lines end_idx lines 0 (Array.length lines - 1) in
        let lines = sub_after mid_point lines in
        let _ = map (fun x -> x - end_idx) lines in
        (N0 { str; lines }, false)
  | N1 t ->
      let t, did_ins = del_internal start_idx end_idx t in
      if did_ins then (n1 t, true) else (N1 t, false)
  | N2 { l; lm; lm_lines; rm; rm_lines; r } ->
      if lm > start_idx && lm > end_idx then
        let l, did_ins = del_internal start_idx end_idx l in
        match did_ins with
        | false ->
            let lm, lm_lines = size l in
            (N2 { l; lm; lm_lines; rm; rm_lines; r }, false)
        | true -> (ins_n2_left l r, true)
      else if lm < start_idx && lm < end_idx then
        let r, did_ins = del_internal (start_idx - lm) (end_idx - lm) r in
        match did_ins with
        | false ->
            let rm, rm_lines = size r in
            (N2 { l; lm; lm_lines; rm; rm_lines; r }, false)
        | true -> (ins_n2_right l r, true)
      else
        (* It is only possible for did_ins to be true for one side as it only happens when deleting at the middle of a node. *)
        let r, did_ins_r = del_internal (start_idx - lm) (end_idx - lm) r in
        let l, did_ins_l = del_internal start_idx end_idx l in
        if did_ins_l then (ins_n2_left l r, true)
        else if did_ins_r then (ins_n2_right l r, true)
        else
          let lm, lm_lines = size l in
          let rm, rm_lines = size r in
          (N2 { l; lm; lm_lines; rm; rm_lines; r }, false)
  | _ -> failwith ""

let delete start length rope =
  let rope, did_ins = del_internal start (start + length) rope in
  if did_ins then root rope else rope

let rec sub_text_internal start_idx end_idx acc = function
  | N0 { str; _ } ->
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
  | N1 t -> sub_text_internal start_idx end_idx acc t
  | N2 { l; lm; r; _ } ->
      (* Cases we need to consider.
         1. start_idx and end_idx are in same directions (both less than or both greater than weight).
         2. start_idx and end_idx are in different direction (start_idx is less than weight while end_idx is less than weight.)
      *)
      if lm > start_idx && lm > end_idx then
        sub_text_internal start_idx end_idx acc l
      else if lm < start_idx && lm < end_idx then
        sub_text_internal (start_idx - lm) (end_idx - lm) acc r
      else
        let acc = sub_text_internal (start_idx - lm) (end_idx - lm) acc r in
        sub_text_internal start_idx end_idx acc l
  | _ -> failwith ""

let sub_text start length rope =
  sub_text_internal start (start + length) [] rope |> String.concat ""

let rec sub_lines_internal start_line end_line acc = function
  | N0 { str; lines } ->
      if start_line < 0 && end_line >= Array.length lines then
        (* In range. *)
        str :: acc
      else if start_line >= 0 && end_line < Array.length lines then
        (* In middle. *)
        let start =
          align_to_after_line str (Array.unsafe_get lines start_line)
        in
        let finish =
          align_to_end_of_line str (Array.unsafe_get lines end_line)
        in
        String.sub str start finish :: acc
      else if start_line >= 0 then
        (* Start of line at this node. *)
        let start =
          align_to_after_line str (Array.unsafe_get lines start_line)
        in
        String.sub str start (String.length str - start) :: acc
      else
        (* End of line at this node *)
        let finish =
          align_to_end_of_line str (Array.unsafe_get lines end_line)
        in
        String.sub str 0 finish :: acc
  | N1 t -> sub_lines_internal start_line end_line acc t
  | N2 { l; lm_lines; r; _ } ->
      if lm_lines > start_line && lm_lines > end_line then
        sub_lines_internal start_line end_line acc l
      else if lm_lines < start_line && lm_lines < end_line then
        sub_text_internal (start_line - lm_lines) (end_line - lm_lines) acc r
      else
        let acc =
          sub_text_internal (start_line - lm_lines) (end_line - lm_lines) acc r
        in
        sub_text_internal start_line end_line acc l
  | _ -> failwith ""

let sub_lines start_line num_of_lines rope =
  sub_lines_internal (start_line - 1) (start_line - 1 + num_of_lines) [] rope
  |> String.concat ""

let sub_line line rope =
  sub_lines_internal (line - 1) 1 [] rope |> String.concat ""

let rec fold f state = function
  | N0 { str; lines } -> if str = "" then state else f state str lines
  | N1 t -> fold f state t
  | N2 { l; r; _ } ->
      let state = fold f state l in
      fold f state r
  | _ -> failwith ""

let rec fold_back f state = function
  | N0 { str; lines } -> if str = "" then state else f state str lines
  | N1 t -> fold_back f state t
  | N2 { l; r; _ } ->
      let state = fold_back f state r in
      fold_back f state l
  | _ -> failwith ""

let to_string rope =
  let lst = fold_back (fun lst str _ -> str :: lst) [] rope in
  String.concat "" lst
