let build_avl () =
  let rec loop num map =
    if num = 100_000 then map
    else
      let map = Avl.add num map in
      loop (num + 1) map
  in
  loop 0 Avl.E

let build_broset () =
  let rec loop num tree =
    if num = 100_000 then tree
    else
      let tree = Brolib.insert num tree in
      loop (num + 1) tree
  in
  loop 0 Brolib.N0

let () =
  let _ = build_broset () in
  Printf.printf "\nInsert:\n";
  Printf.printf "\nBro:\n";
  let start_time = Sys.time () in
  let _ = build_broset () in
  let end_time = Sys.time () in
  let difference = end_time -. start_time in
  Printf.printf "%f\n" difference;

  Printf.printf "AVL:\n";
  let start_time = Sys.time () in
  let _ = build_avl () in
  let end_time = Sys.time () in
  let difference = end_time -. start_time in
  Printf.printf "%f\n" difference
