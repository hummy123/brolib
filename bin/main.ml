open Brolib

let tiny_insert pos ins_str rope = Tiny_rope.insert pos ins_str rope
let tiny_delete start length rope = Tiny_rope.delete start length rope
let tiny_to_string rope = Tiny_rope.to_string rope
let utf8_insert pos ins_str rope = Utf8_rope.insert pos ins_str rope
let utf8_delete start length rope = Utf8_rope.delete start length rope
let utf8_to_string rope = Utf8_rope.to_string rope

let () =
  (* Print out the time it takes to run the four data sets. *)
  Utils.run_edit_traces "Tiny_rope" Tiny_rope.empty tiny_insert tiny_delete
    tiny_to_string;

  (* The Utf8_rope supports line queries using and managing array metadata. *)
  Utils.run_edit_traces "Utf8_rope" Utf8_rope.empty utf8_insert utf8_delete
    utf8_to_string;

  (* 
     Run the edit traces 1000 times, 
     cons the time difference between each individual run to a list,  
     and write the list to a CSV file.
     *)
  let svelte = Utils.run_txns_acc 0 Sveltecomponent.data [] in
  let _ = Utils.write_list "ocaml_svelte.csv" svelte in
  let rust = Utils.run_txns_acc 0 Rustcode.data [] in
  let _ = Utils.write_list "ocaml_rust.csv" rust in
  let seph = Utils.run_txns_acc 0 Sephblog.data [] in
  let _ = Utils.write_list "ocaml_seph.csv" seph in
  let automerge = Utils.run_txns_acc 0 Automerge.data [] in
  let _ = Utils.write_list "ocaml_automerge.csv" automerge in
  ()
