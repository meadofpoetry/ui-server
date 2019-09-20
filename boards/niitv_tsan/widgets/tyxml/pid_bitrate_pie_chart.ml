open Components_tyxml

module CSS = struct
  let root = Util.CSS.root ^ "-pid-bitrate-pie"

  let title = BEM.add_element root "title"

  let wrapper = BEM.add_element root "wrapper"
end

module Make
    (Xml : Xml_sigs.NoWrap)
    (Svg : Svg_sigs.NoWrap with module Xml := Xml)
    (Html : Html_sigs.NoWrap with module Xml := Xml and module Svg := Svg) =
struct
  open Html

  let create_title ?(classes = []) ?(a = []) ?title ?(children = []) () =
    let classes = CSS.title :: classes in
    span ~a:(a_class classes :: a) (Utils.map_cons_option txt title children)

  let create_wrapper ?(classes = []) ?(a = []) ?canvas ?children () =
    let classes = CSS.wrapper :: classes in
    let children =
      match children with
      | Some x -> x
      | None ->
          let canvas =
            match canvas with
            | None -> Html.canvas []
            | Some x -> x
          in
          [canvas]
    in
    div ~a:(a_class classes :: a) children

  let create ?(classes = []) ?(a = []) ?title ?wrapper ?children () =
    let classes = CSS.root :: classes in
    let children =
      match children with
      | Some x -> x
      | None ->
          let title =
            match title with
            | None -> create_title ~title:"Битрейт" ()
            | Some (`Text s) -> create_title ~title:s ()
            | Some (`Element e) -> e
          in
          let wrapper =
            match wrapper with
            | None -> create_wrapper ()
            | Some x -> x
          in
          [title; wrapper]
    in
    div ~a:(a_class classes :: a) children
end

module F = Make (Tyxml.Xml) (Tyxml.Svg) (Tyxml.Html)
