(* TODO add nomimal and measured frequency values? *)
type t =
  { lock : bool
  ; power : float option
  ; mer : float option
  ; ber : float option
  ; freq : int option
  ; bitrate : int option
  } [@@deriving yojson, eq]

let power_to_string = function
  | None -> "-"
  | Some x -> Printf.sprintf "%g dBm" x

let mer_to_string = function
  | None -> "-"
  | Some x -> Printf.sprintf "%g dBm" x

let ber_to_string = function
  | None -> "-"
  | Some x -> Printf.sprintf "%0.3e" x

let frequency_to_string = function
  | None -> "-"
  | Some x -> Printf.sprintf "%d Hz" x

let bitrate_to_string = function
  | None -> "-"
  | Some x -> Printf.sprintf "%d Mbit/s" x

let to_string (x : t) =
  Printf.sprintf "lock: %b, \
                  power: %s, \
                  MER: %s, \
                  BER: %s, \
                  frequency: %s, \
                  bitrate: %s"
    x.lock
    (power_to_string x.power)
    (mer_to_string x.mer)
    (ber_to_string x.ber)
    (frequency_to_string x.freq)
    (bitrate_to_string x.bitrate)
