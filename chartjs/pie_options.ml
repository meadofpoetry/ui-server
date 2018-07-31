open Base

class type t_js =
  object
    inherit Options.t_js
    method cutoutPercentage : Js.number Js.t Js.prop
    method rotation         : Js.number Js.t Js.prop
    method circumference    : Js.number Js.t Js.prop
  end

class t () =
  let o : t_js Js.t = Js.Unsafe.coerce @@ Js.Unsafe.obj [||] in
  object

    inherit Options.t o ()

  end
