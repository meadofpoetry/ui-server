let (%>) = CCFun.(%>)

type typ = Line
         | Bar
         | Radar
         | Pie
         | Doughnut
         | Polar
         | Bubble
         | Scatter

let typ_to_string = function
  | Line   -> "line"   | Bar      -> "bar"      | Radar -> "radar"
  | Pie    -> "pie"    | Doughnut -> "doughnut" | Polar -> "polarArea"
  | Bubble -> "bubble" | Scatter  -> "scatter"

class type config =
  object
    method duration : int Js.prop
    method lazy_    : bool Js.t Js.prop
    method easing   : Js.js_string Js.t Js.prop
  end

let config_to_obj ?duration ?lazy_ ?easing () : config Js.t =
  Options.Obj.cons_option "duration" duration []
  |> Options.Obj.map_cons_option ~f:Js.bool "lazy" lazy_
  |> Options.Obj.map_cons_option ~f:(Options.Animations.easing_to_string %> Js.string) "easing" easing
  |> Array.of_list
  |> Js.Unsafe.obj

class type chart =
  object
    method destroy        : unit -> unit Js.meth
    method update         : config Js.t -> unit Js.meth
    method reset          : unit -> unit Js.meth
    method render         : config Js.t -> unit Js.meth
    method stop           : unit -> chart Js.t Js.meth
    method resize         : unit -> chart Js.t Js.meth
    method clear          : unit -> chart Js.t Js.meth
    method toBase64Image  : unit -> Js.js_string Js.t Js.meth
    method generateLegend : unit -> Js.js_string Js.t Js.meth
  end
