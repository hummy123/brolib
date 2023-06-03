open Brolib

let brope_insert pos ins_str rope = Brope.insert pos ins_str rope
let brope_delete _ _ rope = rope
let brope_to_string rope = Brope.to_string rope

let () =
  Printf.printf "\n-\t brope edit traces \t-";
  let svelte =
    Utils.run_txns_time "Svelete Brope" Sveltecomponent.data Brope.empty
      brope_insert brope_delete
  in
  let rust =
    Utils.run_txns_time "Rustcode Brope" Rustcode.data Brope.empty brope_insert
      brope_delete
  in
  let seph =
    Utils.run_txns_time "Sephblog Brope" Sephblog.data Brope.empty brope_insert
      brope_delete
  in
  let automerge =
    Utils.run_txns_time "Automerge Brope" Automerge.data Brope.empty
      brope_insert brope_delete
  in
  Printf.printf "\n-\t brope to_string \t-";
  let _ =
    Utils.run_to_string_time "Svelte to_string Brope" svelte brope_to_string
  in
  let _ =
    Utils.run_to_string_time "Rust to_string Brope" rust brope_to_string
  in
  let _ =
    Utils.run_to_string_time "Seph to_string Brope" seph brope_to_string
  in
  let _ =
    Utils.run_to_string_time "Automerge to_string Brope" automerge
      brope_to_string
  in
  (* Printf.printf "%s" automerge_str; *)
  ()
