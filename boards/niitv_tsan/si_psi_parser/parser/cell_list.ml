let name = "cell_list_descriptor"

let decode_lat lat =
  Printf.sprintf "%f deg" @@ ((float_of_int @@ (lat * 90)) /. 32268.)

let decode_lon lon =
  Printf.sprintf "%f deg" @@ ((float_of_int @@ (lon * 180)) /. 32268.)

let rec decode_second_loop off x =
  if Bitstring.bitstring_length x = 0 then []
  else
    match%bitstring x with
    | {| cell_id_ext : 8
       ; subcell_lat : 16 : save_offset_to (off_1)
       ; subcell_lon : 16 : save_offset_to (off_2)
       ; sub_ext_lat : 12 : save_offset_to (off_3)
       ; sub_ext_lon : 12 : save_offset_to (off_4)
       ; rest        : -1 : save_offset_to (off_5), bitstring
       |}
      ->
        let lat, ext_lat = (decode_lat subcell_lat, decode_lat sub_ext_lat) in
        let lon, ext_lon = (decode_lon subcell_lon, decode_lon sub_ext_lon) in
        let nodes =
          [
            Node.make ~offset:off 8 "cell_id_extension" (Dec (Int cell_id_ext));
            Node.make ~parsed:lat ~offset:(off + off_1) 16 "subcell_latitude"
              (Dec (Int subcell_lat));
            Node.make ~parsed:lon ~offset:(off + off_2) 16 "subcell_longtitude"
              (Dec (Int subcell_lon));
            Node.make ~parsed:ext_lat ~offset:(off + off_3) 12
              "subcell_extent_of_latitude" (Dec (Int sub_ext_lat));
            Node.make ~parsed:ext_lon ~offset:(off + off_4) 12
              "subcell_extent_of_longtitude" (Dec (Int sub_ext_lon));
          ]
        in
        let name = Printf.sprintf "Subcell %s" (string_of_int cell_id_ext) in
        let node = Node.make ~offset:off 64 name (List nodes) in
        node :: decode_second_loop (off + off_5) rest

let rec parse bs off =
  if Bitstring.bitstring_length bs = 0 then []
  else
    match%bitstring bs with
    | {| cell_id      : 16
       ; cell_lat     : 16 : save_offset_to (off_1)
       ; cell_lon     : 16 : save_offset_to (off_2)
       ; cell_ext_lat : 12 : save_offset_to (off_3)
       ; cell_ext_lon : 12 : save_offset_to (off_4)
       ; loop_length  : 8  : save_offset_to (off_5)
       ; snd_loop     : loop_length * 8 : save_offset_to (off_6), bitstring
       ; rest         : -1 : save_offset_to (off_7), bitstring
       |}
      ->
        let lat, ext_lat = (decode_lat cell_lat, decode_lat cell_ext_lat) in
        let lon, ext_lon = (decode_lon cell_lon, decode_lon cell_ext_lon) in
        let name = Printf.sprintf "Cell %s" (string_of_int cell_id) in
        let snd_loop = decode_second_loop (off + off_6) snd_loop in
        let nodes =
          [
            Node.make ~offset:off 16 "cell_id" (Dec (Int cell_id));
            Node.make ~parsed:lat ~offset:(off + off_1) 16 "cell_latitude"
              (Dec (Int cell_lat));
            Node.make ~parsed:lon ~offset:(off + off_2) 16 "cell_longtitude"
              (Dec (Int cell_lon));
            Node.make ~parsed:ext_lat ~offset:(off + off_3) 16
              "cell_extent_of_latitude" (Dec (Int cell_ext_lat));
            Node.make ~parsed:ext_lon ~offset:(off + off_4) 16
              "cell_extent_of_longtitude" (Dec (Int cell_ext_lon));
            Node.make ~offset:(off + off_5) 8 "subcell_info_loop_length"
              (Dec (Int loop_length));
            Node.make ~offset:(off + off_6) (loop_length * 8) "Subcells"
              (List snd_loop);
          ]
        in
        let real_length = 88 + (loop_length * 8) in
        let node = Node.make ~offset:off real_length name (List nodes) in
        node :: parse rest (off + off_7)
