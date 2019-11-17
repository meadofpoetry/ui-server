open Application_types
open Components_tyxml

module CSS = struct
  let root = Util.CSS.root ^ "-stream-select"

  let label = BEM.add_element root "label"

  let select = BEM.add_element root "select"
end

let rec stream_to_string (s : Stream.t) =
  let src =
    match s.source.node with
    | Entry (Input i) -> "Вход " ^ Topology.get_input_name i
    | Entry (Board b) -> "Плата " ^ Topology.get_board_name b
    | Stream s -> "Поток,  " ^ stream_to_string s
  in
  match Stream.Source.to_string s.source.info with
  | "" -> src
  | s -> Printf.sprintf "%s. %s" src s

module Make
    (Xml : Intf.Xml)
    (Svg : Svg_sigs.T with module Xml := Xml)
    (Html : Html_sigs.T with module Xml := Xml and module Svg := Svg) =
struct
  open Html
  open Xml.W
  open Xml.Wutils
  module Select_markup = Select.Make (Xml) (Svg) (Html)

  let ( @:: ) = cons

  let create_option ?classes ?a ~stream () =
    let text = return (stream_to_string stream) in
    let value = return (Yojson.Safe.to_string (Stream.to_yojson stream)) in
    Select_markup.Native.option ?classes ?a ~value ~text ()

  let create_label ?(classes = return []) ?(a = []) ?label () =
    let classes = fmap (fun x -> CSS.label :: x) classes in
    let default_text =
      "Выберите поток для отображения данных"
    in
    let text =
      match label with
      | None -> return default_text
      | Some x ->
          fmap
            (function
              | None -> default_text
              | Some x -> x)
            x
    in
    let label = return (txt text) in
    span ~a:(a_class classes :: a) (label @:: nil ())

  let create_select ?(classes = return []) ?(a = []) ?streams () =
    let classes = fmap (fun x -> CSS.select :: x) classes in
    let options =
      match streams with
      | None -> nil ()
      | Some streams -> map (fun x -> create_option ~stream:x ()) (totlist streams)
    in
    Select_markup.Native.select
      ~a
      ~classes
      ~outlined:true
      ~label:(return "Потоки")
      ~options
      ()

  let create ?(classes = return []) ?(a = []) ?streams () =
    let classes = fmap (fun x -> CSS.root :: x) classes in
    let label = return (create_label ()) in
    let select = return (create_select ?streams ()) in
    div ~a:(a_class classes :: a) (label @:: select @:: nil ())
end
