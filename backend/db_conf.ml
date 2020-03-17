type t = {
  socket_path : string;
  cleanup : Time.Period.Hours.t;
  password : string;
}
[@@deriving yojson]

let default =
  {
    socket_path = "/tmp";
    cleanup = Time.Period.Hours.of_int 24;
    password = "ats3";
  }

let of_string s =
  Yojson.Safe.from_string s |> of_yojson |> function
  | Ok v -> v
  | Error e -> failwith e

let to_string x = to_yojson x |> Yojson.Safe.to_string

let equal : t -> t -> bool = ( = )
