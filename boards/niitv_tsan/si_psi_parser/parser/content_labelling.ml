(* FIXME *)
(* This might be working, but can be re-organized I guess *)
let name = "content_labelling_descriptor"

let parse_time_base = function
  | 0 -> "No content time base defined in this descriptor"
  | 1 -> "Use of STC"
  | 2 -> "Use of NPT"
  | x when x > 7 && x < 16 -> "Use of privately defined content time base"
  | _ -> "Reserved"

let parse_metadata_app = function
  | 0x0010 -> "ISO 15706 (ISAN) encoded in its binary form"
  | 0x0011 -> "ISO 15706-2 (V-ISAN) encoded in its binary form"
  | 0xFFFF -> "Defined by the metadata_application_format_identifier field"
  | x when x >= 0x0100 && x < 0xFFFF -> "User defined"
  | _ -> "Reserved"

let decode_2 bs off tbi =
  match tbi with
  | 1 -> (
      match%bitstring bs with
      | {| reserved_1 : 7
         ; cont_value : 33 : save_offset_to (off_1)
         ; reserved_2 : 7  : save_offset_to (off_2)
         ; md_value   : 33 : save_offset_to (off_3)
         ; rest       : -1 : save_offset_to (off_4), bitstring
         |}
        ->
          let nodes =
            [
              Node.make ~offset:off 7 "reserved" (Bits (Int reserved_1));
              Node.make ~offset:(off + off_1) 33 "content_time_base_value"
                (Hex (Int64 cont_value));
              Node.make ~offset:(off + off_2) 7 "reserved"
                (Bits (Int reserved_2));
              Node.make ~offset:(off + off_3) 33 "metadata_time_base_value"
                (Hex (Int64 md_value));
            ]
          in
          nodes @ Bytes.parse ~offset:(off + off_4) rest "private_data_byte" )
  | 2 -> (
      match%bitstring bs with
      | {| reserved_1 : 7
         ; cont_value : 33 : save_offset_to (off_1)
         ; reserved_2 : 7  : save_offset_to (off_2)
         ; md_value   : 33 : save_offset_to (off_3)
         ; reserved_3 : 1  : save_offset_to (off_4)
         ; contentid  : 7  : save_offset_to (off_5)
         ; rest       : -1 : save_offset_to (off_6), bitstring
         |}
        ->
          let nodes =
            [
              Node.make ~offset:off 7 "reserved" (Bits (Int reserved_1));
              Node.make ~offset:(off + off_1) 33 "content_time_base_value"
                (Hex (Int64 cont_value));
              Node.make ~offset:(off + off_2) 7 "reserved"
                (Bits (Int reserved_2));
              Node.make ~offset:(off + off_3) 33 "metadata_time_base_value"
                (Hex (Int64 md_value));
              Node.make ~offset:(off + off_4) 1 "reserved"
                (Bits (Bool reserved_3));
              Node.make ~offset:(off + off_5) 33 "contentId"
                (Hex (Int contentid));
            ]
          in
          nodes @ Bytes.parse ~offset:(off + off_6) rest "private_data_byte" )
  | _ -> (
      match%bitstring bs with
      | {| tbad_length : 8
         ; reserved    : tbad_length * 8 : save_offset_to (off_1), bitstring
         ; rest        : -1  : save_offset_to (off_2), bitstring
         |}
        ->
          let nodes =
            [
              Node.make ~offset:off 8 "time_base_association_data_length"
                (Bits (Int tbad_length));
            ]
          in
          let nodes =
            nodes @ Bytes.parse ~offset:(off + off_1) reserved "reserved"
          in
          nodes @ Bytes.parse ~offset:(off + off_2) rest "private_data_byte" )

let decode_1 bs off =
  match%bitstring bs with
  | {| true             : 1
     ; ctb_ind          : 4 : save_offset_to (off_1)
     ; reserved_1       : 3 : save_offset_to (off_2)
     ; crir_len         : 8 : save_offset_to (off_3)
     ; cont_ref_id_byte : crir_len * 8 : save_offset_to (off_4), bitstring
     ; rest             : -1 : save_offset_to (off_5), bitstring
     |}
    ->
      let parsed = parse_time_base ctb_ind in
      let cri =
        match
          Text_decoder.decode @@ Util.Bitstring.to_cstruct cont_ref_id_byte
        with
        | Ok s -> s
        | Error _ -> "Unable to decode"
      in
      let nodes =
        [
          Node.make ~offset:off 1 "content_reference_id_record_flag"
            (Bits (Bool true));
          Node.make ~parsed ~offset:(off + off_1) 4
            "content_time_base_indicator" (Hex (Int ctb_ind));
          Node.make ~offset:(off + off_2) 3 "reserved" (Bits (Int reserved_1));
          Node.make ~offset:(off + off_3) 8 "content_reference_id_record_length"
            (Dec (Int crir_len));
          Node.make ~offset:(off + off_4) (crir_len * 8) "content_reference_id"
            (String cri);
        ]
      in
      nodes @ decode_2 rest (off + off_5) ctb_ind
  | {| false      : 1
     ; ctb_ind    : 4 : save_offset_to (off_1)
     ; reserved_1 : 3 : save_offset_to (off_2)
     ; rest       : -1 : save_offset_to (off_3), bitstring
     |}
    ->
      let parsed = parse_time_base ctb_ind in
      let nodes =
        [
          Node.make ~offset:off 1 "content_reference_id_record_flag"
            (Bits (Bool false));
          Node.make ~parsed ~offset:(off + off_1) 4
            "content_time_base_indicator" (Hex (Int ctb_ind));
          Node.make ~offset:(off + off_2) 3 "reserved" (Bits (Int reserved_1));
        ]
      in
      nodes @ decode_2 rest (off + off_3) ctb_ind

let parse bs off =
  match%bitstring bs with
  | {| metadata : 16
     ; mafid    : 32 : save_offset_to (off_1)
     ; rest     : -1 : save_offset_to (off_2), bitstring
     |}
    ->
      let parsed = parse_metadata_app metadata in
      [
        Node.make ~offset:off 16 "metadata_application_format"
          (Hex (Int 0xFFFF));
        Node.make ~parsed ~offset:(off + off_1) 32
          "metadata_application_format_identifier" (Hex (Int32 mafid));
      ]
      @ decode_1 rest (off + off_2)
  | {| maf  : 16
     ; rest : -1 : save_offset_to (off_1), bitstring
     |} ->
      let parsed = parse_metadata_app maf in
      [
        Node.make ~parsed ~offset:off 16 "metadata_application_format"
          (Hex (Int maf));
      ]
      @ decode_1 rest (off + off_1)
