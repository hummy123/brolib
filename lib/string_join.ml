let join_start old_str new_str cur_index =
  let bytes = Bytes.create (cur_index + String.length new_str) in
  (* Add first half of old string to Bytes. *)
  let _ = Bytes.unsafe_blit_string old_str 0 bytes 0 cur_index in
  (* Add insert string to Bytes. *)
  let _ =
    Bytes.unsafe_blit_string new_str 0 bytes cur_index (String.length new_str)
  in
  Bytes.unsafe_to_string bytes

let join_last old_str new_str cur_index =
  let bytes =
    Bytes.create (String.length new_str + (String.length old_str - cur_index))
  in
  let _ = Bytes.unsafe_blit_string new_str 0 bytes 0 (String.length new_str) in
  let _ =
    Bytes.unsafe_blit_string old_str cur_index bytes (String.length new_str)
      (String.length old_str - cur_index)
  in
  Bytes.unsafe_to_string bytes

let join_three old_str new_str cur_index =
  (* Create mutable byte array containing length of both strings. *)
  let bytes = Bytes.create (String.length old_str + String.length new_str) in
  (* Copy the first half of the old string to the start of the Bytes. *)
  let _ = Bytes.unsafe_blit_string old_str 0 bytes 0 cur_index in
  (* Copy the second half of the old string to the end of the Bytes. *)
  let _ =
    Bytes.unsafe_blit_string old_str cur_index bytes
      (cur_index + String.length new_str)
      (String.length old_str - cur_index)
  in
  (* Copy the newly inserted string to the middle of the Bytes. *)
  let _ =
    Bytes.unsafe_blit_string new_str 0 bytes cur_index (String.length new_str)
  in
  Bytes.unsafe_to_string bytes

let del_middle start_idx end_idx str =
  let bytes = Bytes.create (start_idx + (String.length str - end_idx)) in
  let _ = Bytes.unsafe_blit_string str 0 bytes 0 start_idx in
  let _ =
    Bytes.unsafe_blit_string str end_idx bytes start_idx
      (String.length str - end_idx)
  in
  Bytes.unsafe_to_string bytes
