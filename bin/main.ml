open Brolib

let del _ _ r = r
let s _ = ""
let rec loop () = loop ()

let () =
  (* Print out the time it takes to run the four data sets. *)
  Utils.run_edit_traces "slice_rope" Bal_rope.empty Bal_rope.insert
    Bal_rope.delete s
