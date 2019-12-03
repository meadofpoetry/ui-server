module type Table = sig
  val parse : Bitstring.bitstring -> Node.t list
end

module Node = Node

module Unknown = struct
  let parse _ = []
end

let parse_exn (buf : string) : Node.t list =
  let bs = Bitstring.bitstring_of_string buf in
  match%bitstring bs with
  | {| table_id : 8
     ; _        : -1 : bitstring
     |} ->
      let (module Table : Table) =
        match table_id with
        | 0x00 -> (module PAT)
        | 0x01 -> (module CAT)
        | 0x02 -> (module PMT)
        | 0x03 -> (module TSDT)
        | 0x40 | 0x41 -> (module NIT)
        | 0x42 | 0x46 -> (module SDT)
        | 0x4A -> (module BAT)
        | 0x70 -> (module TDT)
        | 0x71 -> (module RST)
        | 0x72 -> (module ST)
        | 0x73 -> (module TOT)
        | 0x7E -> (module DIT)
        | 0x7F -> (module SIT)
        | x when x >= 0x4E && x <= 0x6F -> (module EIT)
        | _ -> (module Unknown)
      in
      Table.parse bs

let parse (buf : string) : (Node.t list, [`Msg of string]) result =
  try Ok (parse_exn buf) with exn -> Error (`Msg (Printexc.to_string exn))
