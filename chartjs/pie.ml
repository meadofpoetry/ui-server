open Containers
open Base

module Options = Pie_options
module Dataset = Pie_dataset

class type data_js =
  object
    method datasets : Dataset.t_js Js.t Js.js_array Js.t Js.prop
    method labels   : Js.js_string Js.t Js.js_array Js.t Js.prop
  end

let datasets_to_js datasets =
  List.map (fun x -> Js.Unsafe.coerce x#get_obj) datasets
  |> Array.of_list
  |> Js.array

let labels_to_js labels =
  List.map (fun x -> Js.string x) labels
  |> Array.of_list
  |> Js.array

let to_data_js datasets labels : data_js Js.t =
  object%js
    val mutable datasets = datasets_to_js datasets
    val mutable labels = labels_to_js labels
  end

class ['a] t
        ?width ?height
        ?(options : Options.t option)
        ~(labels : string list)
        ~(datasets : 'a Dataset.t list) () =
  let options = match options with
    | Some x -> x
    | None -> new Options.t () in
  let data = to_data_js datasets labels in
  object
    inherit Base_chart.t ?width ?height
              ~typ:`Pie ~options ~data:(Js.Unsafe.inject data) ()

    method set_datasets (x : 'a Dataset.t list) =
      data##.datasets := (datasets_to_js x)

    method set_labels (x : string list) =
      data##.labels := (labels_to_js x)
  end
