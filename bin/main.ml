open Brolib

let tiny_insert pos ins_str rope = Tiny_rope.insert pos ins_str rope
let tiny_delete start length rope = Tiny_rope.delete start length rope
let tiny_to_string rope = Tiny_rope.to_string rope
let utf8_insert pos ins_str rope = Utf8_rope.insert pos ins_str rope
let utf8_delete start length rope = Utf8_rope.delete start length rope
let utf8_to_string rope = Utf8_rope.to_string rope

let () =
  (* Utils.run_edit_traces "Tiny_rope" Tiny_rope.empty tiny_insert tiny_delete *)
  (*   tiny_to_string; *)
  (* Utils.run_edit_traces "Utf8_rope" Utf8_rope.empty utf8_insert utf8_delete *)
  (*   utf8_to_string *)
  let svelte = Utils.get_svelte Tiny_rope.empty tiny_insert tiny_delete in
  let str = Tiny_rope.sub_string 0 1000 svelte in
  Printf.printf "str:\n%s" str;
  Printf.printf "\nend;;\n\n";
  (* (match String.rindex_opt str  '/' with *)
  (* | Some x -> Printf.printf "\n%i" x; *)
  (* | None -> ()); *)
  match Tiny_rope.index_from_opt svelte ~after_index:500 'x' with
  | Some idx -> Printf.printf "\nfound at idx: %i\n" idx
  | None -> Printf.printf "\nnot found"
