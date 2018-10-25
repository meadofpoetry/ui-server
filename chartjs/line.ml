open Containers
open Base

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
