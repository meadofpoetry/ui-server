open Components_tyxml

module CSS = struct
  let root = Util.CSS.root ^ "-pid-bitrate-pie"

  let title = BEM.add_element root "title"

  let wrapper = BEM.add_element root "wrapper"
end

module Make
    (Xml : Intf.Xml)
    (Svg : Svg_sigs.T with module Xml := Xml)
    (Html : Html_sigs.T with module Xml := Xml and module Svg := Svg) =
struct
  open Xml.W
  open Html

  let create_title ?(classes = return []) ?(a = []) ~title () =
    let classes = fmap (fun x -> CSS.title :: x) classes in
    span ~a:(a_class classes :: a) (singleton (return (txt (return title))))

  let create_wrapper ?(classes = return []) ?(a = []) ?canvas () =
    let classes = fmap (fun x -> CSS.wrapper :: x) classes in
    let children =
      let canvas =
        match canvas with
        | None -> Html.canvas (nil ())
        | Some x -> x
      in
      singleton (return canvas)
    in
    div ~a:(a_class classes :: a) children

  let create ?(classes = return []) ?(a = []) ?title ?wrapper () =
    let classes = fmap (fun x -> CSS.root :: x) classes in
    let children =
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
    div ~a:(a_class classes :: a) (Xml.Wutils.const children)
end

module F = Make (Impl.Xml) (Impl.Svg) (Impl.Html)
