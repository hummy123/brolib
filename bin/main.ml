open Brolib

let () =
  (* (* Print out the time it takes to run the four data sets. *) *)
  Utils.run_edit_traces "bal_rope" Bal_rope.empty Bal_rope.insert
    Bal_rope.delete Bal_rope.to_string;
  ()
