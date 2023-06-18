open Brolib

let rope_insert pos ins_str rope = Utf8_rope.insert pos ins_str rope
let rope_delete start length rope = Utf8_rope.delete start length rope
let rope_to_string rope = Utf8_rope.to_string rope

let print_lines rope =
  let stats = Utf8_rope.stats rope in
  let lines_text = Utf8_rope.lines_sub 0 stats.num_lines rope in
  Printf.printf "sub_lines output:\n%s\n" lines_text.string

(* Debugging function for printing line break differences different from expected. *)
let line_break_at_expected rope =
  Utf8_rope.fold
    (fun _ str lines ->
      let real = Utf8_rope.count_line_breaks str in
      if real = lines then ()
      else (
        Printf.printf "line: %i\n" (Array.length lines);
        Printf.printf "real: %i\n" (Array.length real);
        Printf.printf "line\n";
        let _ = Array.map (fun x -> Printf.printf "%i\n" x) lines in
        Printf.printf "real\n";
        let _ = Array.map (fun x -> Printf.printf "%i\n" x) real in
        Array.iter2
          (fun opt real ->
            Printf.printf "line: %i\n" opt;
            Printf.printf "real: %i\n" real)
          lines real;
        failwith "real difference"))
    () rope

let () =
  Printf.printf "\n-\t Rope edit traces \t-";
  let svelte =
    Utils.run_txns_time "Svelete Rope" Sveltecomponent.data Utf8_rope.empty
      rope_insert rope_delete
  in
  let _ = line_break_at_expected svelte in
  let rust =
    Utils.run_txns_time "Rustcode Rope" Rustcode.data Utf8_rope.empty
      rope_insert rope_delete
  in
  let _ = line_break_at_expected rust in
  let seph =
    Utils.run_txns_time "Sephblog Rope" Sephblog.data Utf8_rope.empty
      rope_insert rope_delete
  in
  let _ = line_break_at_expected seph in
  let automerge =
    Utils.run_txns_time "Automerge Rope" Automerge.data Utf8_rope.empty
      rope_insert rope_delete
  in
  let _ = line_break_at_expected automerge in
  Printf.printf "\n-\t Rope to_string \t-";
  let svelte_string =
    Utils.run_to_string_time "Svelte to_string Rope" svelte rope_to_string
  in
  let _ = Utils.run_to_string_time "Rust to_string Rope" rust rope_to_string in
  let _ = Utils.run_to_string_time "Seph to_string Rope" seph rope_to_string in
  let _ =
    Utils.run_to_string_time "Automerge to_string Rope" automerge rope_to_string
  in
  (* let out_buffer = Buffer.create (String.length svelte_string) in *)
  (* Buffer.add_string out_buffer svelte_string; *)
  (* let oc = open_out "svelte.txt" in *)
  (* let _ = Buffer.output_buffer oc out_buffer in *)
  (* let _ = close_out oc in *)
  ()
