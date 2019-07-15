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

module UI = struct

  let ( >>= ) = Lwt.bind

  let make_input ~label () : int Textfield.t =
    Textfield.make_textfield
      ~label
      (Integer (None, None))

  let make_empty_placeholder (table_dialog : Dialog.t) (grid : Grid.t) =
    let table =
      Icon_button.make
        ~on_click:(fun _ _ ->
            table_dialog#open_await ()
            >>= fun _ -> Lwt.return_unit)
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
    Dialog.make
      ~title:(Dialog.Markup.create_title_simple ~title:"Добавление таблицы" ())
      ~content:(Dialog.Markup.create_content
                  ~classes:[Box.CSS.root; Box.CSS.vertical]
                  ~content:[cols#markup; rows#markup]
                  ())
      ~actions:Dialog.Markup.(
          [ create_action ~action:Close ~label:"Отмена" ()
          ; create_action ~action:Accept ~label:"Применить" ()
          ])
      ()


end
