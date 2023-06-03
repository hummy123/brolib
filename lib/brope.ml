type brope =
  | N0 of string
  | N1 of brope
  | N2 of brope * int * int * brope
  (* Aux constructors. *)
  | L2 of string
  | N3 of brope * brope * brope

let max_string_length = 1024
let empty = N0 ""

let rec ins cur_index string = function
  | N0 str ->
      if cur_index = 0 then
        if String.length str + String.length string <= max_string_length then
          N0 (string ^ str)
        else failwith ""
      else if cur_index = String.length str then N0 (str ^ string)
      else
        let sub1 = String.sub str 0 cur_index in
        let sub2 = String.sub str cur_index (String.length str - cur_index) in
        N0 (sub1 ^ string ^ sub2)
  | _ -> failwith ""
