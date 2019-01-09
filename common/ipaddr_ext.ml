module V4 = struct
  include Ipaddr.V4

  let equal (t1:t) (t2:t) =
    Int32.equal (to_int32 t1) (to_int32 t2)

  let to_yojson (x:t) : Yojson.Safe.json =
    let s = to_string x in `String s

  let of_yojson : Yojson.Safe.json -> (t,string) result = function
    | `String s -> (match of_string s with
                    | Some a -> Ok a
                    | None -> Error ("bad address: " ^ s))
    | _ -> Error "not an ipv4 addr"
end

module V6 = struct
  include Ipaddr.V6

  let equal (t1:t) (t2:t) =
    let eq = CCEqual.pair Int64.equal Int64.equal in
    eq (to_int64 t1) (to_int64 t2)

  let to_yojson (x:t) : Yojson.Safe.json =
    let s = to_string x in `String s

  let of_yojson : Yojson.Safe.json -> (t,string) result = function
    | `String s -> (match of_string s with
                    | Some a -> Ok a
                    | None -> Error ("bad address: " ^ s))
    | _ -> Error "not an ipv6 addr"

end

include (Ipaddr:module type of struct include Ipaddr end
                               with module V4 := Ipaddr.V4
                               with module V6 := Ipaddr.V6)
