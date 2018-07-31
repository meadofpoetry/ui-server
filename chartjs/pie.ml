open Containers
open Base

module Options = Pie_options
module Dataset = Pie_dataset

class type data_js =
  object
    method datasets : Dataset.t_js Js.t Js.js_array Js.t Js.prop
    method labels   : Js.js_string Js.t Js.js_array Js.t Js.prop
  end

let to_data_js datasets labels : data_js Js.t =
  object%js
    val mutable datasets =
      List.map (fun x -> Js.Unsafe.coerce x#get_obj) datasets
      |> Array.of_list
      |> Js.array
    val mutable labels =
      List.map (fun x -> Js.string x) labels
      |> Array.of_list
      |> Js.array
  end

class t ~(options:Options.t)
        ~(labels:string list)
        ~(datasets:#Dataset.t_base list) () =
  let data = to_data_js datasets labels |> Js.Unsafe.inject in
  object
    inherit Base_chart.t ~typ:`Pie ~options ~data ()
  end
