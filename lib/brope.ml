type brope =
  | N0 of string
  | N1 of brope
  | N2 of brope * int * int * brope
  (* Aux constructors. *)
  | L2 of string * string
  | N3 of brope * brope * brope

let max_string_length = 512
let empty = N0 ""

let rec size = function
  | N0 s -> String.length s
  | N1 t -> size t
  | N2 (_, lm, rm, _) -> lm + rm
  | N3 (t1, t2, t3) -> size t1 + size t2 + size t3
  | L2 _ -> failwith "unexpected Brope.size: L2"

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

let n2_left left right =
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

let n2_right left right =
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
        if String.length str + String.length string <= max_string_length then
          N0 (string ^ str)
        else L2 (string, str)
      else if cur_index >= String.length str then
        if String.length str + String.length string <= max_string_length then
          N0 (str ^ string)
        else L2 (str, string)
      else
        let sub1 = String.sub str 0 cur_index in
        let sub2 = String.sub str cur_index (String.length str - cur_index) in
        if String.length str + String.length string <= max_string_length then
          N0 (sub1 ^ string ^ sub2)
        else if String.length sub1 + String.length string <= max_string_length
        then L2 (sub1 ^ string, sub2)
        else if String.length sub2 + String.length string <= max_string_length
        then L2 (sub1, string ^ sub2)
        else
          (* String must be split into 3 different parts. *)
          N3 (N0 sub1, N0 string, N0 sub2)
  | N1 t -> n1 (ins cur_index string t)
  | N2 (l, lm, _, r) ->
      if cur_index < lm then n2_left (ins cur_index string l) r
      else n2_right l (ins (cur_index - lm) string r)
  | N3 _ -> failwith "unexpected Brope.ins: N3"
  | L2 _ -> failwith "unexpected Brope.ins: L2"

let insert index string rope = root (ins (size rope - index) string rope)

let rec fold f state = function
  | N0 str -> f state str
  | N1 t -> fold f state t
  | N2 (l, _, _, r) ->
      let state = fold f state l in
      fold f state r
  | N3 _ -> failwith "unexpected Brope.fold: N3"
  | L2 _ -> failwith "unexpected Brope.fold: L2"

let rec fold_back f state = function
  | N0 str -> f state str
  | N1 t -> fold_back f state t
  | N2 (l, _, _, r) ->
      let state = fold_back f state l in
      fold_back f state r
  | N3 _ -> failwith "unexpected Brope.fold_back: N3"
  | L2 _ -> failwith "unexpected Brope.fold_back: L2"

let to_string rope =
  let lst = fold_back (fun lst str -> str :: lst) [] rope in
  String.concat "" lst
