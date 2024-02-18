type rope =
  | N0 of { start : int; length : int; str : string }
  | N1 of rope
  | N2 of rope * int * rope
  (* Aux constructors. *)
  | L2 of {
      s1 : int;
      l1 : int;
      str1 : string;
      s2 : int;
      l2 : int;
      str2 : string;
    }
  | N3 of rope * rope * rope

type t = rope
type balance = AddedNode | DeletedNode | NoAction

let target_length = 1024
let empty = N0 { start = 0; length = 0; str = "" }
let mk string = N0 { start = 0; length = String.length string; str = string }
let of_string = mk

let get_str = function
  | N0 s ->
      if s.start = 0 && s.length = String.length s.str then s.str
      else String.sub s.str s.start s.length
  | _ -> failwith "get_str error"

let rec fold_right f state = function
  | N0 s -> f state s.str
  | N1 t -> fold_right f state t
  | N2 (l, _, r) ->
      let state = fold_right f state r in
      fold_right f state l
  | _ -> failwith "bal_rope fold_right failure"

let to_string rope =
  let lst = fold_right (fun acc str -> str :: acc) [] rope in
  String.concat "" lst

let rec size acc = function
  | N0 s -> acc + s.length
  | N1 t -> size acc t
  | N2 (_, lm, r) -> size (acc + lm) r
  | _ -> failwith "bal_rope failed in function size"

let size t = size 0 t

let ins_root = function
  | L2 { s1; l1; str1; s2; l2; str2 } ->
      N2
        ( N0 { start = s1; length = l1; str = str1 },
          l1,
          N0 { start = s2; length = l2; str = str2 } )
  | N3 (t1, t2, t3) ->
      let t1_size = size t1 in
      let left = N2 (t1, t1_size, t2) in
      let t2_size = size t2 in
      N2 (left, t1_size + t2_size, N1 t3)
  | t -> t

let del_root = function N1 t -> t | t -> t

let ins_n1 = function
  | L2 { s1; l1; str1; s2; l2; str2 } ->
      N2
        ( N0 { start = s1; length = l1; str = str1 },
          l1,
          N0 { start = s2; length = l2; str = str2 } )
  | N3 (t1, t2, t3) ->
      let t1_size = size t1 in
      let left = N2 (t1, t1_size, t2) in
      let t2_size = size t2 in
      N2 (left, t1_size + t2_size, N1 t3)
  | t -> N1 t

let ins_n2_left left right =
  match (left, right) with
  | L2 { s1; l1; str1; s2; l2; str2 }, t3 ->
      N3
        ( N0 { start = s1; length = l1; str = str1 },
          N0 { start = s2; length = l2; str = str2 },
          t3 )
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
  | t1, L2 { s1; l1; str1; s2; l2; str2 } ->
      N3
        ( t1,
          N0 { start = s1; length = l1; str = str1 },
          N0 { start = s2; length = l2; str = str2 } )
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

let rec ins cur_index new_str = function
  | N0 old_str as node ->
      if cur_index <= 0 then
        if old_str.length + String.length new_str <= target_length then
          let str = new_str ^ get_str node in
          (mk str, NoAction)
        else
          let n =
            L2
              {
                s1 = 0;
                l1 = String.length new_str;
                str1 = new_str;
                s2 = old_str.start;
                l2 = old_str.length;
                str2 = old_str.str;
              }
          in
          (n, AddedNode)
      else if cur_index >= old_str.length then
        if old_str.length + String.length new_str <= target_length then
          let n_str = get_str node ^ new_str in
          let n = N0 { start = 0; length = String.length n_str; str = n_str } in
          (n, NoAction)
        else
          let n =
            L2
              {
                s1 = old_str.start;
                l1 = old_str.length;
                str1 = old_str.str;
                s2 = 0;
                l2 = String.length new_str;
                str2 = new_str;
              }
          in
          (n, AddedNode)
      else if old_str.length + String.length new_str <= target_length then
        let new_str = String_join.join_three (get_str node) new_str cur_index in
        (mk new_str, NoAction)
        (* All if-staments below are if concatenating into a single string exceeds target_length. *)
      else if
        (* If first half of old string + insert string does not exceed target_length. *)
        cur_index + String.length new_str <= target_length
      then
        let sub1 = String_join.join_start (get_str node) new_str cur_index in
        let n =
          L2
            {
              s1 = 0;
              l1 = String.length sub1;
              str1 = sub1;
              s2 = old_str.start + cur_index;
              l2 = old_str.length - cur_index;
              str2 = old_str.str;
            }
        in
        (n, AddedNode)
        (* If second half olf old + insert string does not exceed target length. *)
      else if
        old_str.length - cur_index + String.length new_str <= target_length
      then
        (* Get first substring. *)
        let sub2 = String_join.join_last (get_str node) new_str cur_index in
        let n =
          L2
            {
              s1 = old_str.start;
              l1 = old_str.length - cur_index;
              str1 = old_str.str;
              s2 = 0;
              l2 = String.length sub2;
              str2 = sub2;
            }
        in
        (n, AddedNode)
      else
        (* String must be split into 3 different parts. *)
        let left =
          N2
            ( N0
                {
                  start = old_str.start;
                  length = old_str.length - cur_index;
                  str = old_str.str;
                },
              old_str.length,
              mk new_str )
        in
        let right =
          N0
            {
              start = old_str.start + cur_index;
              length = old_str.length - cur_index;
              str = old_str.str;
            }
        in
        (ins_n2_left left right, AddedNode)
  | N1 t -> (
      let t, action = ins cur_index new_str t in
      match action with AddedNode -> (ins_n1 t, action) | _ -> (N1 t, action))
  | N2 (l, lm, r) -> (
      if cur_index < lm then
        let l, action = ins cur_index new_str l in
        match action with
        | NoAction -> (
            match (l, r) with
            | N0 s1, N0 s2 when s1.length + s2.length <= target_length ->
                let str = get_str l ^ get_str r in
                (mk str, DeletedNode)
            | _ -> (N2 (l, lm + String.length new_str, r), action))
        | AddedNode -> (ins_n2_left l r, action)
        | DeletedNode -> (del_n2_left l r, action)
      else
        let r, action = ins (cur_index - lm) new_str r in
        match action with
        | NoAction -> (
            match (l, r) with
            | N0 s1, N0 s2 when s1.length + s2.length <= target_length ->
                let str = get_str l ^ get_str r in
                (mk str, DeletedNode)
            | _ -> (N2 (l, lm, r), action))
        | AddedNode -> (ins_n2_right l r, action)
        | DeletedNode -> (del_n2_right l r, action))
  | _ -> failwith ""

let insert index string rope =
  let rope, action = ins index string rope in
  match action with
  | NoAction -> rope
  | AddedNode -> ins_root rope
  | DeletedNode -> del_root rope

let rec del_internal start_idx end_idx = function
  | N0 str ->
      if start_idx <= 0 && end_idx >= str.length then
        (* In range. *)
        (empty, false)
      else if start_idx >= 0 && end_idx <= str.length then
        (* In middle of this node. *)
        let n =
          L2
            {
              s1 = str.start;
              l1 = start_idx;
              str1 = str.str;
              s2 = str.start + end_idx;
              l2 = str.length - end_idx;
              str2 = str.str;
            }
        in

        (n, true)
      else if start_idx >= 0 && end_idx >= str.length then
        (* Starts at this node. *)
        let n = N0 { start = str.start; length = start_idx; str = str.str } in
        (n, false)
      else
        (* Ends at this node. *)
        let n =
          N0
            {
              start = str.start + end_idx;
              length = str.length - end_idx;
              str = str.str;
            }
        in
        (n, false)
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
