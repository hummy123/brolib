(* Just a standard, barebones AVL-Tree implementation copied from a formal proof
   - not using one in standard library because that one may have optimisations while B-Tree doesn't.
   Implementation ported from: https://isabelle.in.tum.de/library/HOL/HOL-Data_Structures/document.pdf . *)

type 'key avl_tree = E | T of int * 'key avl_tree * 'key * 'key avl_tree

let ht = function E -> 0 | T (h, _, _, _) -> h

let mk lt lb rt =
  let lh = ht lt in
  let rh = ht rt in
  let h = (if lh > rh then lh else rh) + 1 in
  T (h, lt, lb, rt)

let bal_L ab xl c =
  if ht ab = ht c + 2 then
    match ab with
    | T (_, a, yl, b) -> (
        if ht a >= ht b then mk a yl (mk b xl c)
        else
          match b with
          | T (_, b1, bxl, b2) -> mk (mk a yl b1) bxl (mk b2 xl c)
          | x -> x)
    | x -> x
  else mk ab xl c

let bal_R a xl bc =
  if ht bc = ht a + 2 then
    match bc with
    | T (_, b, yl, c) -> (
        if ht b <= ht c then mk (mk a xl b) yl c
        else
          match b with
          | T (_, b1, bxl, b2) -> mk (mk a xl b1) bxl (mk b2 yl c)
          | x -> x)
    | x -> x
  else mk a xl bc

let rec add key = function
  | E -> mk E key E
  | T (_, l, k, r) as node ->
      if key < k then bal_L (add key l) k r
      else if key > k then bal_R l k (add key r)
      else (* Update node with newly given value. *)
        node

let rec member key = function
  | E -> false
  | T (_, l, k, r) ->
      if key < k then member key l else if key > k then member key r else true

let rec fold f state = function
  | E -> state
  | T (_, l, key, r) ->
      let state = fold f state l in
      let state = f key state in
      fold f state r
