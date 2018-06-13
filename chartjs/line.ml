open Containers
open Base

include Line_types

module Axes    = Axes.Cartesian
module Dataset = Line_dataset
module Options = Line_options

class type data_js =
  object
    method datasets  : Dataset.t_js Js.t Js.js_array Js.t Js.prop
  end

let to_data_js datasets : data_js Js.t =
  object%js
    val mutable datasets = List.map (fun x -> x#get_obj) datasets |> Array.of_list |> Js.array
  end

class ['a,'b] t ~(options:Options.t) ~(datasets:('a,'b) Dataset.t list) () =
  let data = to_data_js datasets |> Js.Unsafe.inject in
  object
    inherit [Line_options.t,Line_options.t_js] Base_chart.t ~typ:`Line ~options ~data ()

    method datasets = datasets
    method options  = options
  end
