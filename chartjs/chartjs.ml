[@@@ocaml.warning "-60"]

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

module type Chart = sig

  class type dataset = object end

  type data_fmt

  val typ : typ

  val data : data_fmt -> Js.Unsafe.any

end

module Make(Chart : Chart) = struct

  class type dataset =
    object
      inherit Chart.dataset
      method label_ : Js.js_string Js.t Js.prop
      method data_  : Js.number Js.t Js.js_array Js.t Js.prop
    end

  class type data =
    object
      method labels_   : Js.js_string Js.t Js.js_array Js.t Js.prop
      method datasets_ : dataset Js.t Js.js_array Js.t Js.prop
    end

  class type conf =
    object
      method type_   : Js.js_string Js.t Js.prop
      method data    : data Js.t Js.prop
      method options : Options.t Js.t Js.optdef_prop
    end

  class type t =
    object
    end

  let constr : (Dom_html.canvasElement Js.t -> conf Js.t -> t Js.t) Js.constr =
    Js.Unsafe.global##.Chart

  let create ?id ?width ?height () =
    let open Tyxml_js.Html in
    canvas ~a:(CCOpt.map_or ~default:[] (fun x -> [a_id x]) id
               |> (fun attrs -> CCOpt.map_or ~default:attrs (fun x -> (a_width x) :: attrs) width)
               |> (fun attrs -> CCOpt.map_or ~default:attrs (fun x -> (a_height x) :: attrs) height))
           []

  let attach (data : Chart.data_fmt) conf elt =
    new%js constr elt conf

end


module Line_ : Chart = struct

  type data_fmt = Numbers of int list
                | Points  of point list
   and point = { x : int; y : int }

  type 'a point_setting = Single of 'a
                        | Multi of 'a list

  type cubic_interpolation_mode = Default
                                | Monotone

  type stepped_line = Bool of bool
                    | Before
                    | After

  class type dataset =
    object

    end

  let typ = Line

  let data (data : data_fmt) =
    (match data with
     | Numbers x -> Array.of_list x |> Js.array
     | Points x  -> let point_to_obj (p : point) = Js.Unsafe.obj [| "x", Js.Unsafe.inject p.x
                                                                  ; "y", Js.Unsafe.inject p.y |] in
                    List.map point_to_obj x
                    |> Array.of_list
                    |> Js.array)
    |> Js.Unsafe.inject

end
