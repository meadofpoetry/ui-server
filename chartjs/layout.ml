open Base

type num_or_obj

type padding = [ `I of int
               | `O of padding_obj
               ]
and padding_obj =
  { left   : int
  ; right  : int
  ; top    : int
  ; bottom : int
  }

class type coord =
  object
    method left   : int Js.prop
    method right  : int Js.prop
    method top    : int Js.prop
    method bottom : int Js.prop
  end

class type t_js =
  object
    method padding : num_or_obj Js.t Js.prop
  end

class t () = object(self)
  inherit [t_js] base_option ()

  (** The padding to add inside the chart. *)
  method padding : padding =
    match Cast.to_int obj##.padding with
    | Some x -> `I x
    | None   -> let (o:coord Js.t) = Js.Unsafe.coerce obj##.padding in
                `O { left   = o##.left
                   ; right  = o##.right
                   ; top    = o##.top
                   ; bottom = o##.bottom
                   }
  method set_padding : padding -> unit = function
    | `I x -> obj##.padding := Js.Unsafe.coerce @@ Js.number_of_float @@ float_of_int x
    | `O x -> obj##.padding := Js.Unsafe.coerce @@ object%js
                                                     val mutable left   = x.left
                                                     val mutable right  = x.right
                                                     val mutable top    = x.top
                                                     val mutable bottom = x.bottom
                                                   end

  initializer
    self#set_padding @@ `I 0
end
