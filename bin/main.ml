open Brolib

let del _ _ r = r
let s _ = ""

let () =
  (* (* Print out the time it takes to run the four data sets. *) *)
  Utils.run_edit_traces "slice_rope" Slice_rope.empty Slice_rope.insert
    Slice_rope.delete s;
  ()
