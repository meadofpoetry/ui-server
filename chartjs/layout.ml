type num_or_obj

type padding = Number of int
             | Object of padding_obj
 and padding_obj =
   { left   : int
   ; right  : int
   ; top    : int
   ; bottom : int
   }

class type coord =
  object
    method left   : int Js.optdef_prop
    method right  : int Js.optdef_prop
    method top    : int Js.optdef_prop
    method bottom : int Js.optdef_prop
  end

class type t =
  object
    method padding : num_or_obj Js.t Js.optdef_prop
  end

let cast_padding_number (x : num_or_obj Js.t) = Base.Cast.to_number x
let cast_padding_object (x : num_or_obj Js.t) : coord Js.t Js.opt = Base.Cast.to_object x

let to_obj ?padding () =
  (match padding with
   | Some x -> [| "padding", (match x with
                              | Number n -> Js.Unsafe.inject n
                              | Object o -> [| "left",   Js.Unsafe.inject o.left
                                             ; "right",  Js.Unsafe.inject o.right
                                             ; "top",    Js.Unsafe.inject o.top
                                             ; "bottom", Js.Unsafe.inject o.bottom |]
                                            |> Js.Unsafe.obj
                                            |> Js.Unsafe.inject) |]
   | None  -> [| |])
  |> Js.Unsafe.obj
