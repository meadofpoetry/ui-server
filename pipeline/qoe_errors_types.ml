type labels = [ `Black
              | `Luma
              | `Freeze
              | `Diff
              | `Blocky
              | `Silence_shortt
              | `Silence_moment
              | `Loudness_shortt
              | `Loudness_moment
              ] [@@deriving yojson]
  
let labels_of_int = function
  | 0 -> `Black
  | 1 -> `Luma
  | 2 -> `Freeze
  | 3 -> `Diff
  | 4 -> `Blocky
  | 5 -> `Silence_shortt
  | 6 -> `Silence_moment
  | 7 -> `Loudness_shortt
  | 8 -> `Loudness_moment
  | _ -> failwith "Qoe_errors.labels_of_int: wrong int"
