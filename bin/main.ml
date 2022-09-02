type encoding =
  | Integer of int
  | String of string
  | List of encoding list
  | Dict of (string * encoding) list
  | Null
  | End

type b =
  | Byte of string
  | Empty

let counter = ref 0

let file =
  "d8:announce43:udp://tracker.coppersurfer.tk:6969/announce10:created \
   by13:uTorrent/187013:creation \
   datei1462355939e8:encoding5:UTF-84:infod6:lengthi124234e4:name9:puppy.jpg12:piece \
   lengthi16384eee"
;;

let get_byte () =
  if String.length file = !counter
  then Empty
  else (
    counter := !counter + 1;
    Byte (String.sub file (!counter - 1) 1))
;;

let rec parser buffer =
  let concat = List.fold_left (fun a x -> a ^ x) "" in
  let get_buffer byte =
    match byte with
    | "e" -> End
    | "i" -> Integer 0
    | "l" -> List []
    | "d" -> Dict []
    | x
      when x = "0"
           || x = "1"
           || x = "2"
           || x = "3"
           || x = "4"
           || x = "5"
           || x = "6"
           || x = "7"
           || x = "8"
           || x = "9" -> String x
  in
  let rec until_end buffer =
    match get_byte () with
    | Byte "e" | Byte ":" -> buffer
    | Byte x -> until_end (buffer @ [ x ])
    | Empty -> buffer
  in
  let rec decrement_reader count buffer =
    if count = 0
    then buffer
    else (
      match get_byte () with
      | Byte x -> decrement_reader (count - 1) (buffer @ [ x ])
      | Empty -> buffer)
  in
  let get_int_until_end () = int_of_string (concat (until_end [])) in
  let get_string_until_end x =
    let length_of_read = x :: until_end [] in
    concat (decrement_reader (int_of_string (concat length_of_read)) [])
  in
  match buffer with
  | Null ->
    (match get_byte () with
     | Byte x -> parser (get_buffer x)
     | Empty -> buffer)
  | End -> buffer
  | Integer a -> Integer (get_int_until_end ())
  | String x -> String (get_string_until_end x)
  | List a ->
    let inside = parser Null in
    if inside = End then buffer else parser (List (a @ [ inside ]))
  | Dict a ->
    let key = parser Null in
    (match key with
     | End -> buffer
     | String key_val ->
       let value = parser Null in
       parser (Dict (a @ [ key_val, value ])))
;;
(* 
type channel = Bytes of read_bytes list

let parser_lists encoding pos =
  let get_char local_pos = String.sub encoding local_pos 1 in
  let concat = List.fold_left (fun a x -> a ^ x) "" in
  let int_parser pos =
    let rec int_parser_until_e local_pos count buffer =
      let char = get_char local_pos in
      match char with
      | "e" -> int_of_string (concat buffer), count
      | x -> int_parser_until_e (local_pos + 1) (count + 1) (buffer @ [ x ])
    in
    int_parser_until_e pos 0 []
  in
  let rec read_until_e local_pos count buffer =
    let char = get_char local_pos in
    match char with
    | "e" -> buffer, count
    | "i" ->
      let int_value, shift = int_parser (local_pos + 1) in
      read_until_e (local_pos + shift + 1) (count + 1) (buffer @ [ Integer int_value ])
  in
  read_until_e pos 0 []
;;

(**int parser*)
let int_parser encoding pos =
  let get_char local_pos = String.sub encoding local_pos 1 in
  let rec int_parser_until_e local_pos count buffer =
    let char = get_char local_pos in
    match char with
    | "e" -> int_of_string (concat buffer), count
    | x -> int_parser_until_e (local_pos + 1) (count + 1) (buffer @ [ x ])
  in
  int_parser_until_e pos 0 []
;;

(* let add_value t x a =
  match t with
  | Dict dictionary -> (x, a) :: dictionary
  | _ -> Error "cant add"
;; *)

let file = "example.iso.torrent"

(* int parser *)
let parser encoding pos =
  let get_char local_pos = String.sub encoding local_pos 1 in
  let rec int_parser_until_e local_pos count buffer =
    let char = get_char local_pos in
    match char with
    | "i" -> int_parser_until_e (pos + 1) 0 []
    | "e" -> buffer
    | x -> int_parser_until_e (local_pos + 1) (count + 1) (buffer @ [ x ])
  in
  int_parser_until_e pos 0 []
;;

(* byte string parser *)
let parser encoding pos =
  let concat = List.fold_left (fun a x -> a ^ x) "" in
  let get_char local_pos = String.sub encoding local_pos 1 in
  let rec read_x_bits bit_number start_pos buffer =
    if bit_number = 0
    then concat buffer
    else (
      let char = get_char start_pos in
      read_x_bits (bit_number - 1) (start_pos + 1) (buffer @ [ char ]))
  in
  let rec read_until_e local_pos count buffer =
    let char = get_char local_pos in
    match char with
    | "0" | "1" | "2" | "3" | "4" | "5" | "6" | "7" | "8" | "9" ->
      read_until_e (local_pos + 1) (count + 1) (buffer @ [ char ])
    | ":" -> read_x_bits (int_of_string (concat buffer)) (local_pos + 1) []
  in
  read_until_e pos 0 []
;;

let print_bytes_from file =
  let ic = open_in file in
  let try_read () =
    try Some (input_byte ic) with
    | End_of_file -> None
  in
  let rec loop acc =
    match try_read () with
    (* match w `d` in ascii which is 100*)
    | x -> loop (x :: acc)
  in
  loop []
;;

exception InvalidChar of string

let parser encoding pos =
  let get_char temp_pos = String.sub encoding temp_pos 1 in
  let rec read_until_e local_pos count buffer =
    let char = get_char local_pos in
    match char with
    | "e" -> buffer
    | x -> read_until_e (local_pos + 1) (count + 1) (buffer @ [ x ])
  in
  match get_char pos with
  | "i" -> read_until_e (pos + 1) 0 []
  | _ -> raise (InvalidChar "Incorrect parsing type")
;;

print_bytes_from file

let read_lines_bytes name : int list =
  let ic = open_in name in
  let try_read () =
    try Some (input_line ic) with
    | End_of_file -> None
  in
  let rec loop acc =
    match try_read () with
    | Some s -> loop acc @ [ s ]
    | None ->
      close_in ic;
      acc
  in
  loop []
;; *)
