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

let rec size acc = function
  | N0 s -> acc + String.length s
  | N1 t -> size acc t
  | N2 (l, lm, r) -> size (acc + lm) r
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
