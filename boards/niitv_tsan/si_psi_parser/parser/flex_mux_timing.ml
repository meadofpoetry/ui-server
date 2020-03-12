let name = "FlexMuxTiming descriptor"

let parse bs off =
  match%bitstring bs with
  | {| fcr_es_id       : 16
     ; fcr_resolution  : 32 : save_offset_to (off_1)
     ; fcr_length      : 8  : save_offset_to (off_2)
     ; fmx_rate_length : 8  : save_offset_to (off_3)
     |}
    ->
      [
        Node.make ~offset:off 16 "FCR_ES_ID" (Hex (Int fcr_es_id));
        Node.make ~offset:(off + off_1) 32 "FCRResolution"
          (Hex (Int32 fcr_resolution));
        Node.make ~offset:(off + off_2) 8 "FCRLength" (Dec (Int fcr_length));
        Node.make ~offset:(off + off_3) 8 "FmxRateLength"
          (Dec (Int fmx_rate_length));
      ]
