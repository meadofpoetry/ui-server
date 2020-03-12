val parse_exn : string -> Si_psi_parser_types.Node.t list

val parse :
  string -> (Si_psi_parser_types.Node.t list, [ `Msg of string ]) result
