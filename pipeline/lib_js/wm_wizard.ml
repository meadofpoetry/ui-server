open Containers
open Components
open Wm_components

let get_items_in_row ~(resolution:int*int) ~(item_ar:int*int) items =
  let num           = List.length items in
  let resolution_ar = Utils.resolution_to_aspect resolution
                      |> (fun (x,y) -> (float_of_int x) /. (float_of_int y))
  in
  let item_ar       = (float_of_int @@ fst item_ar) /. (float_of_int @@ snd item_ar) in
  let squares =
    List.mapi (fun rows _ ->
        let rows = rows + 1 in
        let cols = ceil (float_of_int num /. float_of_int rows) in
        let w = 1. in
        let h = w /. resolution_ar in
        if Float.((w /. cols *. item_ar) *. float_of_int rows <= h)
        then int_of_float cols, rows, (w /. cols *. w /. cols *. item_ar)
        else int_of_float cols, rows,
             (h /. (float_of_int rows) *. h /. (float_of_int rows) *. item_ar)
      ) items in
  let (cols:int), _, _ =
    List.fold_left (fun acc x ->
        let _,_,sq = x in
        let _,_,gr = acc in
        if Float.(gr > sq) then acc else x)
                   (0,0,0.) squares in
  cols

let position_widget ~(pos:Wm.position) (widget:string * Wm.widget) : string * Wm.widget =
  let s,v    = widget in
  let cpos   = Utils.to_grid_position pos in
  let wpos   = Option.map_or ~default:cpos (Dynamic_grid.Position.correct_aspect cpos) v.aspect in
  let x      = cpos.x   + ((cpos.w - wpos.w) / 2) in
  let y      = cpos.y   + ((cpos.h - wpos.h) / 2) in
  Printf.printf "cpos: %s, wpos: %s\n"
                (Dynamic_grid.Position.to_string cpos)
                (Dynamic_grid.Position.to_string { wpos with x;y});
  let pos    = {wpos with x;y} |> Utils.of_grid_position in
  s,{ v with position = pos }

let to_checkboxes (widgets:(string * Wm.widget) list) =
  let domains =
    List.fold_left (fun acc (_,(wdg:Wm.widget)) ->
        if List.exists (fun x -> String.equal x wdg.domain) acc
        then acc
        else wdg.domain :: acc) [] widgets
    |> List.rev
  in
  let wds =
    List.fold_left (fun acc str ->
        let label = str in
        let checkbox = new Checkbox.t () in
        checkbox#set_id label;
        let form_field =
          new Form_field.t ~label ~input:checkbox () in
        List.append acc [form_field]
      ) [] domains in
  let checkbox = new Checkbox.t () in
  let check_all = new Form_field.t ~label:"Выбрать все" ~input:checkbox () in
  React.E.map (fun checked ->
      if not checked
      then List.iter (fun x -> x#input_widget#set_checked false) wds
      else List.iter (fun x -> x#input_widget#set_checked true) wds)
  @@ React.S.changes checkbox#s_state |> ignore;
  check_all :: wds

let to_layout ~resolution (widgets:(string * Wm.widget) list) =
  let ar_x,ar_y    = 16,9 in
  let video_widgets = List.filter (fun (x: string * Wm.widget) ->
      String.equal (snd x).type_ "video") widgets in
  let items_in_row = get_items_in_row ~resolution ~item_ar:(ar_x,ar_y) video_widgets in
  List.mapi (fun i (n,(v:Wm.widget)) ->
      let video_w = (fst resolution) / items_in_row in
      let name = v.domain ^ " " ^ n in
      (*             let s   = Printf.sprintf "Контейнер #%d" (succ i) in*)
      let video_h = (ar_y * video_w) / ar_x in
      let row = i / items_in_row in
      let video_x = (i - items_in_row * row) * video_w in
      let video_y = row * video_h in
      let (video_pos : Wm.position) = { left =video_x
                                      ; top = video_y
                                      ; right = video_x + video_w
                                      ; bottom = video_y + video_h
                                      } in
      let audio_h = video_h in
      let audio_w = 30 in
      let audio_x = video_x + video_w - 30 in
      let audio_y = video_y in
      let audio = List.find (fun (_, (x:Wm.widget)) ->
          String.equal x.domain v.domain && String.equal x.domain "soundbar") widgets in
      let (audio_pos : Wm.position) = { left =audio_x
                                      ; top = audio_y
                                      ; right = audio_x + audio_w
                                      ; bottom = audio_y + audio_h
                                      } in
      let video_wdg = position_widget ~pos:video_pos (n,v) in
      let audio_wdg = position_widget ~pos:audio_pos audio in
      let container   = ({ position = video_pos
                         ; widgets  = [video_wdg;audio_wdg]
                         }:Wm.container)
      in
      name,container)
    video_widgets

let to_dialog (wm:Wm.t) =
  let e,push     = React.E.create () in
  let checkboxes = to_checkboxes wm.widgets in
  let box        = new Vbox.t ~widgets:checkboxes () in
  let dialog     = new Dialog.t
                       ~title:"Выберите виджеты"
                       ~scrollable:true
                       ~content:(`Widgets [box])
                       ~actions:[ new Dialog.Action.t ~typ:`Decline ~label:"Отмена" ()
                                ; new Dialog.Action.t ~typ:`Accept  ~label:"Применить"  ()
                                ]
                       ()
  in
  let show = fun () ->
    Lwt.bind (dialog#show_await ())
             (function
              | `Accept -> let widgets =
                             List.filter_map (fun x ->
                                 if not @@ x#input_widget#checked
                                 then None
                                 else let id = x#input_widget#id in
                                   List.find_pred (fun (wdg: string * Wm.widget) ->
                                       String.equal id (snd wdg).domain)
                                     wm.widgets)
                                             checkboxes
                           in Lwt.return @@ push @@ to_layout ~resolution:wm.resolution widgets
              | `Cancel -> Lwt.return ())
  in
  dialog,e,show
