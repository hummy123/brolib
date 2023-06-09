open Brolib

let rope_insert pos ins_str rope = Text_rope.insert pos ins_str rope
let rope_delete start length rope = Text_rope.delete start length rope
let rope_to_string rope = Text_rope.to_string rope

let () =
  Printf.printf "\n-\t Rope edit traces \t-";
  let svelte =
    Utils.run_txns_time "Svelete Rope" Sveltecomponent.data Text_rope.empty
      rope_insert rope_delete
  in
  let rust =
    Utils.run_txns_time "Rustcode Rope" Rustcode.data Text_rope.empty
      rope_insert rope_delete
  in
  let seph =
    Utils.run_txns_time "Sephblog Rope" Sephblog.data Text_rope.empty
      rope_insert rope_delete
  in
  let automerge =
    Utils.run_txns_time "Automerge Rope" Automerge.data Text_rope.empty
      rope_insert rope_delete
  in
  Printf.printf "\n-\t Rope to_string \t-";
  let _ =
    Utils.run_to_string_time "Svelte to_string Rope" svelte rope_to_string
  in
  let _ = Utils.run_to_string_time "Rust to_string Rope" rust rope_to_string in
  let _ = Utils.run_to_string_time "Seph to_string Rope" seph rope_to_string in
  let _ =
    Utils.run_to_string_time "Automerge to_string Rope" automerge rope_to_string
  in
  ()
