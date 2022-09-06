open Bencode

let ic = open_in "./bin/file.torrent"
let decoded = decoder ic
let info_hash = get_info_hash decoded
let encoded = encoder decoded;;

Printf.printf "Info hash value for torrent file: %s\n%!" info_hash;;

Printf.printf
  "Original File Hash: %s\n%!Decoded and Re-Encoded File Hash:  %s\n%!"
  (Sha1.to_hex (Sha1.input ic))
  (Sha1.to_hex (Sha1.string encoded))
