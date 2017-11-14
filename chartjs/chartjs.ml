[@@@ocaml.warning "-60"]

module Options = Options

module type Chart = sig

  type dataset

  type data_fmt

  val typ : Base.typ

  val data : data_fmt -> Js.Unsafe.any Js.js_array Js.t

end

let create_canvas ?id ?width ?height () =
  let open Tyxml_js.Html in
  canvas ~a:(CCOpt.map_or ~default:[] (fun x -> [a_id x]) id
             |> (fun attrs -> CCOpt.map_or ~default:attrs (fun x -> (a_width x) :: attrs) width)
             |> (fun attrs -> CCOpt.map_or ~default:attrs (fun x -> (a_height x) :: attrs) height))
         []

module Line = Line
