open Js_of_ocaml
open Js_of_ocaml_tyxml
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

module UI = struct

  open Js_of_ocaml_lwt

  let ( >>= ) = Lwt.bind

  let make_input ~label () : int Textfield.t =
    Textfield.make_textfield
      ~label
      (Integer (Some 1, None))

  let make_empty_placeholder
      (table_dialog, value : Dialog.t * (unit -> int option * int option))
      (grid : Grid.t) =
    let table =
      Icon_button.make
        ~on_click:(fun _ _ ->
            table_dialog#open_await ()
            >>= function
            | Close | Destroy | Custom _ -> Lwt.return_unit
            | Accept ->
              match value () with
              | None, _ | _, None -> Lwt.return_unit
              | Some cols, Some rows ->
                grid#reset ~cols ~rows ();
                Lwt.return_unit)
        ~icon:Icon.SVG.(make_simple Path.table_plus)#root
        () in
    let wizard =
      Icon_button.make
        ~icon:Icon.SVG.(make_simple Path.auto_fix)#root
        () in
    let content = Box.make ~dir:`Row [wizard; table] in
    Ui_templates.Placeholder.With_icon.make
      ~font:Body_1
      ~icon:content#root
      ~text:"Мозаика пуста. \n\
             Воспользуйтесь мастером настройки \n\
             или начните с создания таблицы!"
      ()

  let add_table_dialog () =
    let cols = make_input ~label:"Число столбцов" () in
    let rows = make_input ~label:"Число строк" () in
    let title =
      Tyxml_js.To_dom.of_element
      @@ Dialog.Markup.create_title_simple
        ~title:"Добавление таблицы"
        () in
    let content =
      Tyxml_js.To_dom.of_element
      @@ Dialog.Markup.create_content
        ~classes:[Box.CSS.root; Box.CSS.vertical]
        ~content:[cols#markup; rows#markup]
        () in
    let submit =
      Button.attach
      @@ Dialog.make_action ~action:Accept ~label:"Применить" () in
    let actions = Dialog.(
        [ make_action ~action:Close ~label:"Отмена" ()
        ; Js.Unsafe.coerce submit#root
        ]) in
    let check_input () = match cols#value, rows#value with
      | None, _ | _, None -> submit#set_disabled true
      | Some _, Some _ -> submit#set_disabled false in
    let listeners =
      Lwt_js_events.(
        [ inputs cols#input_element (fun _ _ -> check_input (); Lwt.return_unit)
        ; inputs rows#input_element (fun _ _ -> check_input (); Lwt.return_unit)
        ]) in
    let dialog = Dialog.make
        ~classes:[Page_mosaic_editor_tyxml.Container_editor.CSS.dialog_add_table]
        ~title ~content ~actions () in
    (* Set initial submit button state *)
    check_input ();
    (* Clear event listeners on destroy *)
    dialog#set_on_destroy (fun () ->
        List.iter Lwt.cancel listeners);
    dialog, (fun () -> cols#value, rows#value)

end
