open Components_tyxml

let duration_to_string (duration : Time.Period.t) : string =
  Printf.sprintf "%g" (Time.Period.to_float_s duration)

let duration_of_string (s : string) : Time.Period.t option =
  match float_of_string_opt s with
  | None -> None
  | Some x -> Time.Period.of_float_s x

let service_to_string ?name id =
  match name with
  | None -> Printf.sprintf "Service %d" id
  | Some x -> x

module Const = struct
  let default_duration = Time.Period.of_int_s 60
end

module CSS = struct
  let root = "pipeline-parameter-chart"

  let chart_wrapper = BEM.add_element root "chart-wrapper"

  let title = BEM.add_element root "title"

  let legend = BEM.add_element root "legend"

  let legend_wrapper = BEM.add_element root "legend-wrapper"

  let legend_module = BEM.add_element root "legend-module"

  let legend_dataset = BEM.add_element root "legend-dataset"

  let legend_color = BEM.add_element root "legend-color"

  let legend_hidden = BEM.add_modifier legend "hidden"
end

module Make
    (Xml : Intf.Xml)
    (Svg : Svg_sigs.T with module Xml := Xml)
    (Html : Html_sigs.T with module Xml := Xml and module Svg := Svg) =
struct
  open Html
  open Xml.W
  open Xml.Wutils

  let make_legend_service
      ?(classes = return [])
      ?(a = [])
      ?(background_color = "initial")
      ?(border_color = "initial")
      ?(hidden = false)
      ?name
      ~id
      () =
    let background = Printf.sprintf "background-color: %s;" background_color in
    let border = Printf.sprintf "border-color: %s;" border_color in
    let classes =
      fmap
        (fun x -> CSS.legend_dataset :: x |> Utils.cons_if hidden CSS.legend_hidden)
        classes
    in
    let children =
      [ span
          ~a:
            [ a_class (return [ CSS.legend_color ])
            ; a_style (return (background ^ border))
            ]
          (nil ())
      ; txt (return (service_to_string ?name id))
      ]
    in
    div ~a:(a_class classes :: a) (const children)

  let create_legend_items ?(classes = return []) ?(a = []) ?(rows = nil ()) () =
    let classes = fmap (fun x -> CSS.legend :: x) classes in
    table ~a:(a_class classes :: a) rows

  let create ?(classes = return []) ?(a = []) ?(duration = Const.default_duration) ~typ ()
      =
    let classes = fmap (fun x -> CSS.root :: x) classes in
    let children =
      [ div
          ~a:[ a_class (return [ CSS.title ]) ]
          (const [ txt (return (Util.measure_typ_to_human_string typ)) ])
      ; div ~a:[ a_class (return [ CSS.legend_wrapper ]) ] (const [])
      ; div ~a:[ a_class (return [ CSS.chart_wrapper ]) ] (const [ canvas (const []) ])
      ]
    in
    div
      ~a:
        ([ a_class classes
         ; a_user_data "type" (return (Util.measure_typ_to_string typ))
         ; a_user_data "duration" (return (duration_to_string duration))
         ]
        @ a)
      (const children)
end
