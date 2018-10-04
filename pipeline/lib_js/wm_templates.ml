open Containers
open Components
open Wm_types
open Wm_components

(* let widget =
 *   let fmt   =
 *     let open Table in
 *     let open Format in
 *     Table.((to_column ~sortable:true "Название", String (Some (fun x -> "Шаблон '" ^ x ^ "'")))
 *            :: (to_column ~sortable:true "Аудио", String None)
 *            :: (to_column ~sortable:true "Текст", String None)
 *            :: (to_column ~sortable:true "Канал", String None)
 *            :: (to_column ~sortable:true "Ошибки",String None)
 *            :: [])
 *   in
 *   let table     = new Table.t ~fmt () in
 *   let audio_pos = [| "Снизу"; "Сверху"; "Справа"; "Слева" |] in
 *   let text_pos  = [| "По центру"; "Внизу"; "Сверху"|] in
 *   let ch_pos    = [| "По центру"; "Внизу"; "Сверху"|] in
 *   let err_pos   = [| "По центру"; "Внизу"; "Сверху"|] in
 *   let make_row i () =
 *     let name  = Printf.sprintf "№%d" i in
 *     let audio = audio_pos.(Random.run (Random.int 4)) in
 *     let text  = text_pos.(Random.run (Random.int 3)) in
 *     let ch    = ch_pos.(Random.run (Random.int 3)) in
 *     let err   = err_pos.(Random.run (Random.int 3)) in
 *     table#add_row (name :: audio :: text :: ch :: err :: []) in
 *   List.iteri (fun i _ -> make_row i () |> ignore) @@ List.range' 0 7;
 *   table#widget *)

let add_template ?f_def ?f_new text grid =
  let text_w = new Typography.Text.t ~text () in
  let left   = new Hbox.t ~valign:`Start ~widgets:[text_w#widget] () in
  let box    = new Hbox.t ~halign:`Start ~widgets:[left#widget] () in
  let positions = grid#positions in
  let x,y = List.fold_while (fun (_,i) _ ->
      let r = List.fold_left (fun acc (x : Dynamic_grid.Position.t) -> if x.y = i * 30
                               then
                                 (let right = x.x + x.w in
                                  if right > acc then right else acc)
                               else acc) 0 positions in
      if 120 - r >= 30 then ((r,i*30), `Stop) else ((0, i+1), `Continue)) (0,0) (List.range 0 3)
  in
  let item = Dynamic_grid.Item.to_item
      ~pos:{ x ; y ; w = 30 ; h = 30 }
      ~resizable:false
      ~draggable:false
      ~selectable:true
      ~widget:box#widget
      ~value:()
      ()
  in
  (match f_def, f_new with
  | None, Some f -> box#listen Dom_events.Typ.dblclick f |> ignore (*FIXME should be item root el*)
  | Some f, None -> box#listen Dom_events.Typ.dblclick (f box#append_child) |> ignore
  | _, _         -> ());
  grid#add item |> ignore

let make_grid ~dlg_def ~dlg_new ~name =
  let (props:Dynamic_grid.grid) =
    Dynamic_grid.to_grid ~cols:120 ~row_height:10 ~min_col_width:10 ~items_margin:(10,10) ()
  in
  let def_txt = new Typography.Text.t ~text:"По умолчанию" () in
  let default = new Hbox.t ~valign:`Start ~widgets:[def_txt#widget] () in
  let grid = new Dynamic_grid.t ~grid:props ~items:[] () in
  let def_fun meth = (fun _ _ ->
      let open Lwt.Infix in
      dlg_def#show_await ()
      >>= (function
           | `Accept -> Lwt.return @@ meth default#widget
           | `Cancel -> Lwt.return ())
      |> ignore;
      false) in
  let create () = (fun _ _ ->
      let open Lwt.Infix in
      dlg_new#show_await ()
      >>= (function
          | `Accept ->(match React.S.value name#s_input with
              | Some x -> Lwt.return @@ add_template ~f_def:def_fun x grid
              | None -> Lwt.return ())
          | `Cancel -> Lwt.return())
      |> ignore;
      false) in
  add_template ~f_new:(create ()) "Создать шаблон" grid;
  add_template ~f_def:def_fun "Шаблон №1" grid;
  add_template ~f_def:def_fun "Шаблон №2" grid;
  grid

class t () =

  let dlg_def =
    new Dialog.t
      ~actions:[ new Dialog.Action.t ~typ:`Decline ~label:"Отмена" ()
               ; new Dialog.Action.t ~typ:`Accept ~label:"Ok" () ]
      ~title:"Сделать шаблоном по умолчанию?"
      ~content:(`Widgets []) () in
  let () = dlg_def#add_class "wm-confirmation-dialog" in
  let name = new Textfield.t
    ~input_id:"name"
    ~label:"Имя нового шаблона"
    ~input_type:Widget.Text
    ~help_text:{ validation = true
               ; persistent = false
               ; text       = Some "Введите имя шаблона"
               }
    () in
  let () = name#set_required true in
  let box  = new Vbox.t ~widgets:[name] () in
  let dlg_new =
    new Dialog.t
      ~actions:[ new Dialog.Action.t ~typ:`Decline ~label:"Отмена" ()
               ; new Dialog.Action.t ~typ:`Accept ~label:"Ok" () ]
      ~title:"Введите имя шаблона"
      ~content:(`Widgets [box]) () in
  let () = dlg_new#add_class "wm-confirmation-dialog" in
  let grid = make_grid ~dlg_def ~dlg_new ~name in
  let cell = new Layout_grid.Cell.t ~span:12 ~widgets:[ grid#widget
                                                      ; dlg_def#widget
                                                      ; dlg_new#widget] () in

  object

    inherit Layout_grid.t ~cells:[cell] ()

  end

let page () = new t ()
