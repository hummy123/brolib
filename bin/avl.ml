(* Just a standard, barebones AVL-Tree implementation copied from a formal proof
   - not using one in standard library because that one may have optimisations while B-Tree doesn't.
   Implementation ported from: https://isabelle.in.tum.de/library/HOL/HOL-Data_Structures/document.pdf . *)

type ('key, 'value) avl_tree =
  | E
  | T of int * ('key, 'value) avl_tree * 'key * 'value * ('key, 'value) avl_tree

let ht = function E -> 0 | T (h, _, _, _, _) -> h

let mk lt lb st rt =
  let lh = ht lt in
  let rh = ht rt in
  let h = (if lh > rh then lh else rh) + 1 in
  T (h, lt, lb, st, rt)

let bal_L ab xl xs c =
  if ht ab = ht c + 2 then
    match ab with
    | T (_, a, yl, ys, b) -> (
        if ht a >= ht b then mk a yl ys (mk b xl xs c)
        else
          match b with
          | T (_, b1, bxl, bxs, b2) ->
              mk (mk a yl ys b1) bxl bxs (mk b2 xl xs c)
          | x -> x)
    | x -> x
  else mk ab xl xs c

let bal_R a xl xs bc =
  if ht bc = ht a + 2 then
    match bc with
    | T (_, b, yl, ys, c) -> (
        if ht b <= ht c then mk (mk a xl xs b) yl ys c
        else
          match b with
          | T (_, b1, bxl, bxs, b2) ->
              mk (mk a xl xs b1) bxl bxs (mk b2 yl ys c)
          | x -> x)
    | x -> x
  else mk a xl xs bc

let rec add key value = function
  | E -> mk E key value E
  | T (_, l, k, v, r) ->
      if key < k then bal_L (add key value l) k v r
      else if key > k then bal_R l k v (add key value r)
      else (* Update node with newly given value. *)
        mk l key value r

let rec find_opt key = function
  | E -> None
  | T (_, l, k, v, r) ->
      if key < k then find_opt key l
      else if key > k then find_opt key r
      else Some v

let rec fold f state = function
  | E -> state
  | T (_, l, key, value, r) ->
      let state = fold f state l in
      let state = f key value state in
      fold f state r
