open Containers

type t =
  { chan : int
  ; freq : int
  ; spec : bool
  ; name : string
  }

let mhz = 1_000_000

let to_string chan freq special =
  if special
  then Printf.sprintf "ТВК S%02d (%d МГц)" chan (freq / mhz)
  else Printf.sprintf "ТВК %02d (%d МГц)" chan (freq / mhz)

module Terrestrial = struct

  let lst : t list =
    let f = function
      | x when x <= 7 ->
         let chan = x + 5 in
         let freq = (chan * 8 + 130) * mhz in
         { chan; freq; spec = false; name = to_string chan freq false }
      | x ->
         let chan = x + 13 in
         let freq = (chan * 8 + 306) * mhz in
         { chan; freq; spec = false; name = to_string chan freq false }
    in
    List.map f (List.range 1 56)

end

module Cable = struct

  let (lst : t list) =
    let f = function
      | 1 ->
         let chan = 1 in
         let freq = 5250000 in
         let spec = false in
         { chan; freq; spec; name = to_string chan freq spec }
      | 2 ->
         let chan = 2 in
         let freq = 6200000 in
         let spec = false in
         { chan; freq; spec; name = to_string chan freq spec }
      | x when x >= 3 && x <= 5 ->
         let chan = x in
         let freq = (x * 8 + 56) * mhz in
         let spec = false in
         { chan; freq; spec; name = to_string chan freq spec }
      | x when x >= 6 && x <= 13 ->
         let chan = x - 5 in
         let freq = (x * 8 + 66) * mhz in
         let spec = true in
         { chan; freq; spec; name = to_string chan freq spec }
      | x when x >= 14 && x <= 20 ->
         let chan = x - 8 in
         let freq = (x * 8 + 66) * mhz in
         let spec = false in
         { chan; freq; spec; name = to_string chan freq spec }
      | x when x >= 21 && x <= 50 ->
         let chan = x - 10 in
         let freq = (x * 8 + 66) * mhz in
         let spec = true in
         { chan; freq; spec; name = to_string chan freq spec }
      | x                         ->
         let chan = x - 30 in
         let freq = (x * 8 + 66) * mhz in
         let spec = false in
         { chan; freq; spec; name = to_string chan freq spec }
    in
    List.map f (List.range 1 99)

end
