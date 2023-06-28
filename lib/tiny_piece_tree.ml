type tree =
  | N0 of { start : int; length : int }
  | N1 of tree
  | N2 of { l : tree; lm : int; rm : int; r : tree }
  (* Aux constructors. *)
  | L2 of { s1 : int; l1 : int; s2 : int; l2 : int }
  | N3 of tree * tree * tree

type t = { tree : tree; rope : Tiny_rope.t }

let empty = { tree = N0 { start = 0; length = 0 }; rope = Tiny_rope.empty }

let rec size = function
  | N0 { length; _ } -> length
  | N1 t -> size t
  | N2 { lm; rm; _ } -> lm + rm
  | N3 (t1, t2, t3) -> size t1 + size t2 + size t3
  | _ -> failwith ""

let root = function
  | L2 { s1; l1; s2; l2 } ->
      N2
        {
          l = N0 { start = s1; length = l1 };
          lm = l1;
          rm = l2;
          r = N0 { start = s2; length = l2 };
        }
  | N3 (t1, t2, t3) ->
      let left = N2 { l = t1; lm = size t1; rm = size t2; r = t2 } in
      N2 { l = left; lm = size t1 + size t2; rm = size t3; r = N1 t3 }
  | t -> t

let n1 = function
  | L2 { s1; l1; s2; l2 } ->
      N2
        {
          l = N0 { start = s1; length = l1 };
          lm = l1;
          rm = l2;
          r = N0 { start = s2; length = l2 };
        }
  | N3 (t1, t2, t3) ->
      let left = N2 { l = t1; lm = size t1; rm = size t2; r = t2 } in
      N2 { l = left; lm = size t1 + size t2; rm = size t3; r = N1 t3 }
  | t -> N1 t

let ins_n2_left left right =
  match (left, right) with
  | L2 { s1; l1; s2; l2 }, t3 ->
      N3 (N0 { start = s1; length = l1 }, N0 { start = s2; length = l2 }, t3)
  | N3 (t1, t2, t3), N1 t4 ->
      let left = N2 { l = t1; lm = size t1; rm = size t2; r = t2 } in
      let right = N2 { l = t3; lm = size t3; rm = size t4; r = t4 } in
      N2 { l = left; lm = size left; rm = size right; r = right }
  | N3 (t1, t2, t3), (N2 _ as t4) ->
      N3 (N2 { l = t1; lm = size t1; rm = size t2; r = t2 }, N1 t3, t4)
  | N3 (t1, t2, t3), t4 ->
      let left = N2 { l = t1; lm = size t1; rm = size t2; r = t2 } in
      let right = N2 { l = t3; lm = size t3; rm = size t4; r = t4 } in
      N2 { l = left; lm = size left; rm = size right; r = right }
  | l, r -> N2 { l; lm = size l; rm = size r; r }

let ins_n2_right left right =
  match (left, right) with
  | t1, L2 { s1; l1; s2; l2 } ->
      N3 (t1, N0 { start = s1; length = l1 }, N0 { start = s2; length = l2 })
  | N1 t1, N3 (t2, t3, t4) ->
      let left = N2 { l = t1; lm = size t1; rm = size t2; r = t2 } in
      let right = N2 { l = t3; lm = size t3; rm = size t3; r = t4 } in
      N2 { l = left; lm = size left; rm = size right; r = right }
  | (N2 _ as t1), N3 (t2, t3, t4) ->
      N3 (t1, N1 t2, N2 { l = t3; lm = size t3; rm = size t4; r = t4 })
  | t1, N3 (t2, t3, t4) ->
      let left = N2 { l = t1; lm = size t1; rm = size t2; r = t2 } in
      let right = N2 { l = t3; lm = size t3; rm = size t3; r = t4 } in
      N2 { l = left; lm = size left; rm = size right; r = right }
  | l, r -> N2 { l; lm = size l; rm = size r; r }

let rec ins cur_index ins_start ins_length = function
  | N0 { start = old_start; length = old_length } ->
      if cur_index <= 0 then
        L2 { s1 = ins_start; l1 = ins_length; s2 = old_start; l2 = old_length }
      else if cur_index >= ins_length then
        if old_start + old_length = ins_start then
          N0 { start = old_start; length = old_length + ins_length }
        else
          L2
            { s1 = old_start; l1 = old_length; s2 = ins_start; l2 = ins_length }
      else
        (* Have to split old node in two, and insert current one in middle. *)
        let left = N0 { start = old_start; length = cur_index } in
        let right =
          N0 { start = old_start + cur_index; length = old_length - cur_index }
        in
        let middle = N0 { start = ins_start; length = ins_length } in
        N3 (left, middle, right)
  | N1 t -> n1 (ins cur_index ins_start ins_length t)
  | N2 { l; lm; r; _ } ->
      if cur_index < lm then
        ins_n2_left (ins cur_index ins_start ins_length l) r
      else ins_n2_right l (ins (cur_index - lm) ins_start ins_length r)
  | _ -> failwith ""

let insert index string piece_tree =
  let ins_start = Tiny_rope.size piece_tree.rope in
  let ins_length = String.length string in
  let tree = root (ins index ins_start ins_length piece_tree.tree) in
  let rope = Tiny_rope.append string piece_tree.rope in
  { tree; rope }
