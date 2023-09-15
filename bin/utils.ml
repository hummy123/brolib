(* Times any function and returns the output of that function. *)
let time_func title f =
  let title = "\nStarting " ^ title ^ "...\n" in
  let _ = Printf.printf "%s" title in
  let t = Sys.time () in
  let x = f () in
  let endTime = (Sys.time () -. t) *. 1000.0 in
  let _ = Printf.printf "Execution time: %f ms\n" endTime in
  x

(* Runs an empty rope throught the edit trace, resulting rope when done. *)
let run_txns_result_generic initial (arr : (int * int * string) array) f_ins
    f_del =
  Array.fold_left
    (fun rope (pos, del_num, ins_str) ->
      let rope = if del_num > 0 then f_del pos del_num rope else rope in
      if ins_str <> String.empty then f_ins pos ins_str rope else rope)
    initial arr

let run_txns_time title arr initial f_ins f_del =
  time_func title (fun _ -> run_txns_result_generic initial arr f_ins f_del)

let run_substring_result rope num_chars f =
  let half_length = num_chars / 2 in
  let start = half_length / 2 in
  f start half_length rope

let run_substring_time title rope num_chars f =
  time_func title (fun _ -> run_substring_result rope num_chars f)

let run_to_string_time title rope f = time_func title (fun _ -> f rope)

(* Prints running times, given a title, empty rope, an insert function, a deletion function and a to_string function. *)
let run_edit_traces title empty f_ins f_del f_to_string =
  Printf.printf "\n-\t%s edit traces" title;
  let svelte =
    run_txns_time ("Svelte " ^ title) Sveltecomponent.data empty f_ins f_del
  in
  let rust = run_txns_time ("Rust " ^ title) Rustcode.data empty f_ins f_del in
  let seph = run_txns_time ("Seph " ^ title) Sephblog.data empty f_ins f_del in
  let automerge =
    run_txns_time ("Automerge " ^ title) Automerge.data empty f_ins f_del
  in
  Printf.printf "\n-\t%s to string" title;
  let _ = run_to_string_time ("Svelte to_string " ^ title) svelte f_to_string in
  let _ = run_to_string_time ("Rust to_string " ^ title) rust f_to_string in
  let _ = run_to_string_time ("Seph to_string " ^ title) seph f_to_string in
  let _ =
    run_to_string_time ("Automerge to_string " ^ title) automerge f_to_string
  in
  ()

let get_svelte empty f_ins f_del =
  run_txns_time "svelte" Sveltecomponent.data empty f_ins f_del
