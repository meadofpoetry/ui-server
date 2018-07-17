open Containers
open Components
open Board_types.Streams.TS
open Lwt_result.Infix
open Common
open Board_types
open Widget_structure_common

type config =
  { stream       : Stream.id
  } [@@deriving yojson]

let default_config =
  { stream       = Single
  }

(* Widget default name *)
let name = "Структура"

(* Settings widget *)
let settings = None

let (^::) = List.cons_maybe

let make_parsed () =
  let base_class = Markup.CSS.add_element base_class "parsed" in
  let body       = Dom_html.createDiv Dom_html.document |> Widget.create in
  let ()         = body#add_class base_class in
  body#widget

let make_hexdump_options hexdump =
  let base_class = Markup.CSS.add_element base_class "hexdump-options" in
  let base =
    new Select.t
      ~label:"Основание"
      ~items:[ `Item (new Select.Item.t ~value:`Hex ~text:"16" ())
             ; `Item (new Select.Item.t ~value:`Dec ~text:"10" ())
             ; `Item (new Select.Item.t ~value:`Bin ~text:"2" ()) ]
      () in
  let width =
    new Select.t
      ~label:"Ширина"
      ~items:[ `Item (new Select.Item.t ~value:4  ~text:"4"  ())
             ; `Item (new Select.Item.t ~value:8  ~text:"8" ~selected:true ())
             ; `Item (new Select.Item.t ~value:16 ~text:"16" ())
             ; `Item (new Select.Item.t ~value:32 ~text:"32" ()) ]
      () in
  let line_numbers  = new Switch.t ~state:true () in
  let line_numbers' = new Form_field.t ~input:line_numbers
                        ~label:"Номера" () in
  let options = new Hbox.t
                  ~widgets:[ base#widget
                           ; width#widget
                           ; line_numbers'#widget ] () in
  let () = options#add_class base_class in
  let _  = React.S.map hexdump#set_line_numbers line_numbers#s_state in
  let _  = React.S.map (function
               | Some x -> hexdump#set_width x
               | None   -> ()) width#s_selected_value in
  let _  = React.S.map (function
               | Some x -> hexdump#set_base x
               | None   -> ()) base#s_selected_value in
  options#widget

let make_hexdump (signal:string React.signal) =
  let config  = Hexdump.to_config ~width:16 () in
  let hexdump = new Hexdump.t ~config "" () in
  let _s = React.S.map hexdump#set_bytes signal in
  Lwt_react.S.keep _s;
  hexdump

let make_dump_header base_class () =
  (* CSS classes *)
  let header_class = Markup.CSS.add_element base_class "header" in
  let title_class  = Markup.CSS.add_element base_class "title" in
  (* Elements *)
  let title     = new Typography.Text.t
                    ~adjust_margin:false
                    ~text:"Выберите секцию таблицы SI/PSI для захвата" () in
  let subtitle  = new Typography.Text.t
                    ~adjust_margin:false
                    ~split:true
                    ~text:"" () in
  let button    = new Ui_templates.Buttons.Get.t
                    ~style:`Raised
                    ~label:"Загрузить" () in
  let title_box = new Vbox.t
                    ~widgets:[ title#widget
                             ; subtitle#widget ] () in
  let header    = new Hbox.t
                    ~halign:`Space_between
                    ~widgets:[ title_box#widget
                             ; button#widget] () in
  (* CSS classes setup *)
  let () = title#add_class title_class in
  let () = header#add_class header_class in
  header#widget, title, subtitle, button

let make_dump (event:dumpable React.event) =
  let base_class = Markup.CSS.add_element base_class "dump" in
  let header, title, subtitle, button = make_dump_header base_class () in
  let s, push = React.S.create "" in
  let parsed  = make_parsed () in
  let hexdump = make_hexdump s in
  let options = make_hexdump_options hexdump in
  let () =
    React.E.map (fun {name;get;prev} ->
        let open Lwt.Infix in
        let text   = new Typography.Text.t ~text:"" () in
        let err x  = Ui_templates.Placeholder.create_with_error ~text:x () in
        let ph  x  = Ui_templates.Placeholder.create_with_icon
                       ~icon:"info"
                       ~text:x () in
        let get    = fun () ->
          Lwt.catch (fun () ->
              get ()
              >|= (function
                   | Ok _    -> parsed#set_empty ();
                                Dom.appendChild parsed#root text#root
                   | Error s -> parsed#set_empty ();
                                Dom.appendChild parsed#root (err s)#root))
            (fun e ->
              parsed#set_empty ();
              Dom.appendChild parsed#root (err @@ Printexc.to_string e)#root;
              Lwt.return_unit) in
        let _s   =
          React.S.map (function
              | Some raw ->
                 parsed#set_empty ();
                 text#set_text raw;
                 Dom.appendChild parsed#root text#root;
                 push raw
              | None     ->
                 parsed#set_empty ();
                 Dom.appendChild parsed#root (ph "Нет захваченных данных")#root;
                 push "") prev in
        let () = button#set_getter (Some get) in
        let () = title#set_text @@ fst name in
        let () = subtitle#set_text @@ snd name in
        let () = button#set_disabled false in
        _s) event
    |> Lwt_react.E.keep in
  let vsplit = new Vsplit.t parsed hexdump () in
  let vbox   = new Vbox.t ~widgets:[ header
                                   ; (new Divider.t ())#widget
                                   ; vsplit#widget
                                   ; (new Divider.t ())#widget
                                   ; options#widget ] () in
  vbox#add_class base_class;
  vbox#widget

class t ?(config=default_config)
        ~(state:Common.Topology.state React.signal)
        ~(stream:Common.Stream.t option React.signal)
        ~(init:structure option)
        ~(event:structure option React.event)
        (control:int)
        () =
  let ( >|= ) = Lwt.Infix.( >|= ) in
  let stream_panel_class = Markup.CSS.add_element base_class "stream-panel" in
  let id  = config.stream in
  let ph  = Ui_templates.Placeholder.create_with_icon
              ~icon:"warning" ~text:"Нет потока" () in
  let box = Dom_html.createDiv Dom_html.document
            |> Widget.create in
  let e_dumpable, push = React.E.create () in
  let ()  = Dom.appendChild box#root ph#root in
  let make_event init = React.E.fmap (fun x -> x) event in
  let make_struct init =
    let event = make_event init in
    let wdg   = Widget_structure_stream.make_stream id
                  init event push control in
    wdg in
  let ts_s = match init with
    | None ->
       let next = Lwt_react.E.next @@ React.E.fmap (fun x -> x) event in
       next >|= (make_struct)
    | Some init ->
       let wdg   = make_struct init in
       Lwt.return wdg in
  let dump = make_dump e_dumpable in
  object(self)
    inherit Hsplit.t box dump ()

    initializer
      Dom.appendChild box#root ph#root;
      ts_s
      >|= (fun w -> box#set_empty (); Dom.appendChild box#root w#root)
      |> Lwt.ignore_result;
      box#add_class stream_panel_class;
      self#add_class base_class
  end

let make
      ?(config:config option)
      ~(state:Common.Topology.state React.signal)
      ~(stream:Common.Stream.t option React.signal)
      ~(init:structure option)
      ~(event:structure option React.event)
      (control:int)
      () =
  new t ?config ~state ~stream ~init ~event control ()
