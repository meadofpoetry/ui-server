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

class ['a] t
        ?(options:Options.t option)
        ~(labels:string list)
        ~(datasets:'a Dataset.t list) () =
  let options = match options with
    | Some x -> x
    | None   -> new Options.t () in
  let data = to_data_js datasets labels in
  object
    inherit Base_chart.t ~typ:`Pie ~options ~data:(Js.Unsafe.inject data) ()

    method set_labels (x:string list) =
      let l = List.map Js.string x |> Array.of_list |> Js.array in
      data##.labels := l
  end
