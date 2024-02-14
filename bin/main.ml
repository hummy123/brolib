open Brolib

let () =
  (* (* Print out the time it takes to run the four data sets. *) *)
  Utils.run_edit_traces "Tiny_rope" Tiny_rope.empty Tiny_rope.insert
    Tiny_rope.delete Tiny_rope.to_string;

  Utils.run_edit_traces "mem_rope" Mem_rope.empty Mem_rope.insert
    Mem_rope.delete Mem_rope.to_string;
  ()
