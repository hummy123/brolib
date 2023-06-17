open Brolib

let rope_insert pos ins_str rope = Utf8_rope.insert pos ins_str rope
let rope_delete start length rope = Utf8_rope.delete start length rope
let rope_to_string rope = Utf8_rope.to_string rope

let line_break_at_expected rope =
  Utf8_rope.fold
    (fun _ str lines ->
      Array.fold_left
        (fun _ pos ->
          if pos < 0 then failwith "negative";
          let chr = String.unsafe_get str pos in
          if chr = '\r' || chr = '\n' then ()
          else (
            Printf.printf "pos: %i\n" pos;
            Printf.printf "chr: %s\n" (String.make 1 chr |> String.escaped);
            let sub = String.sub str (pos - 1) 3 |> String.escaped in
            Printf.printf "sub: %s\n\n" sub))
        () lines)
    () rope

let () =
  Printf.printf "\n-\t Rope edit traces \t-";
  let svelte =
    Utils.run_txns_time "Svelete Rope" Sveltecomponent.data Utf8_rope.empty
      rope_insert rope_delete
  in
  let _ = line_break_at_expected svelte in
  (* let rust = *)
  (*   Utils.run_txns_time "Rustcode Rope" Rustcode.data Utf8_rope.empty *)
  (*     rope_insert rope_delete *)
  (* in *)
  (* let _ = line_break_at_expected rust in *)
  (* let seph = *)
  (*   Utils.run_txns_time "Sephblog Rope" Sephblog.data Utf8_rope.empty *)
  (*     rope_insert rope_delete *)
  (* in *)
  (* let automerge = *)
  (*   Utils.run_txns_time "Automerge Rope" Automerge.data Utf8_rope.empty *)
  (*     rope_insert rope_delete *)
  (* in *)
  (* Printf.printf "\n-\t Rope to_string \t-"; *)
  (* let _ = *)
  (*   Utils.run_to_string_time "Svelte to_string Rope" svelte rope_to_string *)
  (* in *)
  (* let _ = Utils.run_to_string_time "Rust to_string Rope" rust rope_to_string in *)
  (* let _ = Utils.run_to_string_time "Seph to_string Rope" seph rope_to_string in *)
  (* let _ = *)
  (*   Utils.run_to_string_time "Automerge to_string Rope" automerge rope_to_string *)
  (* in *)
  ()
