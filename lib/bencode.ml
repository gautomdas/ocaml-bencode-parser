exception DecodingError of string
exception EncodingError of string
exception DictionaryError of string

type encoding =
  | Integer of int
  | ByteString of bytes
  | List of encoding list
  | Dict of (string * encoding) list
  | Start
  | End

type byte =
  | Byte of char
  | Buffer of bytes
  | Empty

let decoder input_chanel =
  let get_n_bytes n =
    if n = 1
    then (
      try Byte (input_char input_chanel) with
      | End_of_file ->
        close_in input_chanel;
        Empty)
    else (
      let buf = Bytes.create n in
      let read_bytes = input input_chanel buf 0 n in
      if n = read_bytes
      then Buffer buf
      else (
        close_in input_chanel;
        Empty))
  in
  let rec decode buffer =
    let concat = List.fold_left (fun a x -> a ^ Char.escaped x) "" in
    let get_buffer byte =
      match byte with
      | 'e' -> End
      | 'i' -> Integer 0
      | 'l' -> List []
      | 'd' -> Dict []
      | x when x >= '0' && x <= '9' -> ByteString (Bytes.make 1 x)
      | _ -> raise (DecodingError "Invalid Buffer Byte Data Type")
    in
    let rec until_end buffer =
      match get_n_bytes 1 with
      | Byte 'e' | Byte ':' -> buffer
      | Byte x -> until_end (buffer @ [ x ])
      | Empty -> buffer
      | _ -> raise (DecodingError "Only Parses Until End on Bytes and End")
    in
    let get_int_until_end () = int_of_string (concat (until_end [])) in
    let get_string_until_end x =
      let length_of_read = x :: until_end [] in
      match get_n_bytes (int_of_string (concat length_of_read)) with
      | Buffer b -> b
      | Byte b -> Bytes.make 1 b
      | Empty -> Bytes.empty
    in
    match buffer with
    | Start ->
      (match get_n_bytes 1 with
       | Byte x -> decode (get_buffer x)
       | Empty -> buffer
       | _ -> raise (DecodingError "Impossible Match"))
    | End -> buffer
    | Integer _ -> Integer (get_int_until_end ())
    | ByteString x -> ByteString (get_string_until_end (Bytes.get x 0))
    | List a ->
      let inside = decode Start in
      if inside = End then buffer else decode (List (a @ [ inside ]))
    | Dict a ->
      let key = decode Start in
      (match key with
       | End -> buffer
       | ByteString key_val ->
         let value = decode Start in
         decode (Dict (a @ [ Bytes.to_string key_val, value ]))
       | _ -> raise (DecodingError "Invalid Dictionary Key"))
  in
  decode Start
;;

let encoder encoding =
  let rec encode buffer str =
    match buffer with
    | Integer a -> "i" ^ string_of_int a ^ "e"
    | ByteString a ->
      let string_val = Bytes.to_string a in
      (* can replace with Bytes.length a *)
      string_of_int (String.length string_val) ^ ":" ^ string_val
    | List a ->
      (match a with
       | [] -> "l" ^ str ^ "e"
       | hd :: tl ->
         let cumulative_s = str ^ encode hd "" in
         encode (List tl) cumulative_s)
    | Dict a ->
      (match a with
       | [] -> "d" ^ str ^ "e"
       | hd :: tl ->
         let str_hd, second_tail = hd in
         let cumulative_s =
           str ^ encode (ByteString (Bytes.of_string str_hd)) "" ^ encode second_tail ""
         in
         encode (Dict tl) cumulative_s)
    | _ -> raise (EncodingError "Incorrect Encoding Type")
  in
  encode encoding ""
;;

let rec get encoding key =
  match encoding with
  | Dict a ->
    (match a with
     | [] -> None
     | hd :: tl ->
       let hd_key, value = hd in
       if hd_key = key then Some value else get (Dict tl) key)
  | _ -> raise (DictionaryError "'get only' works on encodings of type Dict []")
;;

let get_info_hash encoding =
  Sha1.to_hex (Sha1.string (encoder (Option.get (get encoding "info"))))
;;
