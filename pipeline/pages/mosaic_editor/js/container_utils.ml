open Js_of_ocaml
open Components

module Attr = struct
  let title = "data-title"
  let aspect = "data-aspect"
end

let get_cell_title (cell : Dom_html.element Js.t) : string =
  match Element.get_attribute cell Attr.title with
  | None -> ""
  | Some s -> s

let set_cell_title (cell : Dom_html.element Js.t) (title : string) : unit =
  Element.set_attribute cell Attr.title title
