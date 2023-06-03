open Brolib

let brope_insert pos ins_str rope = Brope.insert pos ins_str rope
let brope_delete _ _ rope = rope

let () =
  Printf.printf "\n-\t brope edit traces \t-";
  let _ =
    Utils.run_txns_time "Svelete Brope" Sveltecomponent.data Brope.empty
      brope_insert brope_delete
  in
  let _ =
    Utils.run_txns_time "Rustcode Brope" Rustcode.data Brope.empty brope_insert
      brope_delete
  in
  let _ =
    Utils.run_txns_time "Sephblog Brope" Sephblog.data Brope.empty brope_insert
      brope_delete
  in
  let _ =
    Utils.run_txns_time "Automerge Brope" Automerge.data Brope.empty
      brope_insert brope_delete
  in
  ()
