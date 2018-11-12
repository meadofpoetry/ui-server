open Containers

include Line_types

module Axes    = Axes.Cartesian
module Dataset = Line_dataset
module Options = Line_options

class type data_js =
  object
    method datasets : Dataset.t_js Js.t Js.js_array Js.t Js.prop
  end

let datasets_to_js datasets =
  List.map (fun x -> Js.Unsafe.coerce x#get_obj) datasets
  |> Array.of_list
  |> Js.array

let to_data_js datasets : data_js Js.t =
  object%js
    val mutable datasets = datasets_to_js datasets
  end

class t ?width ?height
        ~(options : Options.t)
        ~(datasets : #Dataset.t_base list) () =
  let data = to_data_js datasets in
  object
    inherit Base_chart.t ?width ?height
              ~typ:`Line
              ~options
              ~data:(Js.Unsafe.inject data)
              ()

    method set_datasets : 'a. (#Dataset.t_base as 'a) list -> unit =
      fun x -> data##.datasets := (datasets_to_js x)

    method options = options
  end

module Chart : sig
  type t

  type node =
    [ `Id of string
    | `Canvas of Dom_html.canvasElement Js.t
    | `Context of Dom_html.canvasRenderingContext2D Js.t
    ]

  val make : node -> Line_dataset_.Config.t -> t

end = struct

  include Line_dataset_.Chart

  type node =
    [ `Id of string
    | `Canvas of Dom_html.canvasElement Js.t
    | `Context of Dom_html.canvasRenderingContext2D Js.t
    ]

  let make (node : node) (config : Line_dataset_.Config.t) : t =
    let node = match node with
      | `Id s -> Ojs.string_to_js s
      | `Canvas c -> Obj.magic c
      | `Context c -> Obj.magic c in
    Line_dataset_.Chart.new_chart node config

end
