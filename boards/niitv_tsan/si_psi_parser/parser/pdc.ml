let name = "PDC_descriptor"

let parse_label pil =
  match%bitstring pil with
  | {| day   : 5
     ; month : 4
     ; hour  : 5
     ; min   : 6
     |} ->
      let month =
        match month with
        | 1 -> "January"
        | 2 -> "February"
        | 3 -> "March"
        | 4 -> "April"
        | 5 -> "May"
        | 6 -> "June"
        | 7 -> "July"
        | 8 -> "August"
        | 9 -> "September"
        | 10 -> "October"
        | 11 -> "November"
        | 12 -> "December"
        | x -> Printf.sprintf "%d" x
      in
      Printf.sprintf "%d %s %d:%d" day month hour min

let parse bs off =
  match%bitstring bs with
  | {| rfu : 4
     ; pil : 20 : save_offset_to (off_1), bitstring
     |} ->
      let parsed = parse_label pil in
      let pil = match%bitstring pil with {|pil : 20|} -> pil in
      [
        Node.make ~offset:off 4 "reserved_future_use" (Bits (Int rfu));
        Node.make ~parsed ~offset:(off + off_1) 20
          "programme_identification_label" (Bits (Int pil));
      ]
