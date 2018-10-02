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


let widget =
  let (props:Dynamic_grid.grid) =
    Dynamic_grid.to_grid ~rows:20 ~cols:30
      ~row_height:10 ~min_col_width:10
      ~vertical_compact:true ~items_margin:(10,10) ()
  in
  let text   = new Typography.Text.t ~text:"Шаблон" () in
  let left   = new Hbox.t ~valign:`Center
    ~widgets:[text#widget ] () in
  let box    = new Hbox.t ~halign:`Space_between
    ~widgets:[left#widget] () in
  let text1  = new Typography.Text.t ~text:"Шаблон 1" () in
  let left1  = new Hbox.t ~valign:`Center
    ~widgets:[text1#widget ] () in
  let box1   = new Hbox.t ~halign:`Space_between
    ~widgets:[left1#widget] () in
  let items  = [ Dynamic_grid.Item.to_item
                   ~pos:{ x = 0
                        ; y = 0
                        ; w = 15
                        ; h = 10 }
                   ~resizable:false
                   ~draggable:false
                   ~selectable:true
                   ~widget:box#widget
                   ~value:()
                   ()
               ; Dynamic_grid.Item.to_item
                   ~pos:{ x = 15
                        ; y = 0
                        ; w = 15
                        ; h = 10 }
                   ~resizable:false
                   ~draggable:false
                   ~selectable:true
                   ~widget:box1#widget
                   ~value:()
                   ()
               ]
  in
  let grid = new Dynamic_grid.t ~grid:props ~items () in
  grid#widget

class t () =

  let cell  = new Layout_grid.Cell.t ~span:12 ~widgets:[widget] () in

  object

    inherit Layout_grid.t ~cells:[cell] ()

  end

let page () = new t ()
