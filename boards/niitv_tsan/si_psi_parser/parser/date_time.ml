let parse_date (days : int) : Ptime.date option =
  let date =
    match Ptime.of_date @@ (1858, 11, 17) with
    | None -> raise Not_found
    | Some x -> x
  in
  let sec = 3600 * 24 * days in
  let span = Ptime.Span.of_int_s sec in
  match Ptime.add_span date span with
  | None -> None
  | Some x -> Some (Ptime.to_date x)

let parse_time (bs : Bitstring.t) : Ptime.time option =
  try
    match Bitstring.bitstring_length bs with
    | 24 -> (
        match%bitstring bs with
        | {| hr1  : 4
           ; hr2  : 4
           ; min1 : 4
           ; min2 : 4
           ; sec1 : 4
           ; sec2 : 4
           |}
          ->
            let hr, min, sec =
              ((hr1 * 10) + hr2, (min1 * 10) + min2, (sec1 * 10) + sec2)
            in
            Some ((hr, min, sec), 0) )
    | 16 -> (
        match%bitstring bs with
        | {| hr1  : 4
           ; hr2  : 4
           ; min1 : 4
           ; min2 : 4
           |}
          ->
            let hr, min = ((hr1 * 10) + hr2, (min1 * 10) + min2) in
            Some ((hr, min, 0), 0) )
    | _ -> None
  with _ -> None

let parse_timestamp (bs : Bitstring.t) : Ptime.t option =
  try
    match%bitstring bs with
    | {| x    : 40 |} when Int64.equal x 0xFFFFFFFFFFL -> None
    | {| date : 16
       ; time : 24 : bitstring
       |} -> (
        match (parse_date date, parse_time time) with
        | None, _ | _, None -> None
        | Some date, Some time -> Ptime.of_date_time (date, time) )
  with _ -> None

let parse_duration (bs : Bitstring.t) : Ptime.span option =
  let date = Ptime.to_date @@ Ptime.epoch in
  match parse_time bs with
  | None -> None
  | Some x -> (
      match Ptime.of_date_time (date, x) with
      | None -> None
      | Some x -> Some (Ptime.to_span x) )
