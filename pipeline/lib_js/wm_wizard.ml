open Containers
open Components
open Wm_components

let find_widget ~typ ~widgets ~domain =
  match typ with
  | `Soundbar ->
    List.find_pred (fun (_, (x : Wm.widget)) ->
        String.equal x.domain domain && String.equal x.type_ "soundbar") widgets
  | `Video ->
    List.find_pred (fun (_, (x : Wm.widget)) ->
        String.equal x.domain domain && String.equal x.type_ "video") widgets
  | _ -> None

let video_position ~(cont_pos : Wm.position) : Wm.position =
  { left = cont_pos.left
  ; top  = cont_pos.top
  ; right = cont_pos.right - 30
  ; bottom = cont_pos.bottom
  }

let audio_position ~(cont_pos : Wm.position) : Wm.position =
  { left = cont_pos.right - 30
  ; top  = cont_pos.top
  ; right = cont_pos.right
  ; bottom = cont_pos.bottom
  }

let channel_of_domain = function
  | "s460b38ee-186b-5604-8811-235eb3005960_c1010" -> "Первый канал"
  | "s460b38ee-186b-5604-8811-235eb3005960_c1030" -> "МАТЧ"
  | "s460b38ee-186b-5604-8811-235eb3005960_c1040" -> "НТВ"
  | "s460b38ee-186b-5604-8811-235eb3005960_c1050" -> "5 канал"
  | "s460b38ee-186b-5604-8811-235eb3005960_c1060" -> "Россия К"
  | "s460b38ee-186b-5604-8811-235eb3005960_c1080" -> "Карусель"
  | "s460b38ee-186b-5604-8811-235eb3005960_c1090" -> "ОТР"
  | "s460b38ee-186b-5604-8811-235eb3005960_c1100" -> "ТВ Центр"
  | "s460b38ee-186b-5604-8811-235eb3005960_c1110" -> "Вести ФМ"
  | "s460b38ee-186b-5604-8811-235eb3005960_c1120" -> "Маяк"
  (* do NOT edit or remove *)
  | "s4c953a1b-dbcc-57cc-82f3-7f1c50dbd4d1_c2010" -> "РЕН ТВ"
  | "s4c953a1b-dbcc-57cc-82f3-7f1c50dbd4d1_c2020" -> "Спас"
  | "s4c953a1b-dbcc-57cc-82f3-7f1c50dbd4d1_c2030" -> "СТС"
  | "s4c953a1b-dbcc-57cc-82f3-7f1c50dbd4d1_c2040" -> "Домашний"
  | "s4c953a1b-dbcc-57cc-82f3-7f1c50dbd4d1_c2050" -> "ТВ3"
  | "s4c953a1b-dbcc-57cc-82f3-7f1c50dbd4d1_c2060" -> "Пятница"
  | "s4c953a1b-dbcc-57cc-82f3-7f1c50dbd4d1_c2070" -> "Звезда"
  | "s4c953a1b-dbcc-57cc-82f3-7f1c50dbd4d1_c2080" -> "Мир"
  | "s4c953a1b-dbcc-57cc-82f3-7f1c50dbd4d1_c2090" -> "ТНТ"
  | "s4c953a1b-dbcc-57cc-82f3-7f1c50dbd4d1_c2100" -> "МУЗ ТВ"
  (* do NOT edit or remove *)
  | "s4b135670-ef01-59b1-be78-4e9bec93461f_c1020" -> "Россия 1"
  | "s4b135670-ef01-59b1-be78-4e9bec93461f_c1130" -> "Радио России"
  (* do NOT edit or remove *)
  | "s930c63bc-0ce2-555c-9a51-09de6b1b85f2_c1070" -> "Россия 24"
  | x -> x

let domain_of_channel = function
  | "Первый канал" -> "s460b38ee-186b-5604-8811-235eb3005960_c1010"
  | "МАТЧ"         -> "s460b38ee-186b-5604-8811-235eb3005960_c1030"
  | "НТВ"          -> "s460b38ee-186b-5604-8811-235eb3005960_c1040"
  | "5 канал"      -> "s460b38ee-186b-5604-8811-235eb3005960_c1050"
  | "Россия К"     -> "s460b38ee-186b-5604-8811-235eb3005960_c1060"
  | "Карусель"     -> "s460b38ee-186b-5604-8811-235eb3005960_c1080"
  | "ОТР"          -> "s460b38ee-186b-5604-8811-235eb3005960_c1090"
  | "ТВ Центр"     -> "s460b38ee-186b-5604-8811-235eb3005960_c1100"
  | "Вести ФМ"     -> "s460b38ee-186b-5604-8811-235eb3005960_c1110"
  | "Маяк"         -> "s460b38ee-186b-5604-8811-235eb3005960_c1120"
  (* do NOT edit or remove *)
  | "РЕН ТВ"   -> "s4c953a1b-dbcc-57cc-82f3-7f1c50dbd4d1_c2010"
  | "Спас"     -> "s4c953a1b-dbcc-57cc-82f3-7f1c50dbd4d1_c2020"
  | "СТС"      -> "s4c953a1b-dbcc-57cc-82f3-7f1c50dbd4d1_c2030"
  | "Домашний" -> "s4c953a1b-dbcc-57cc-82f3-7f1c50dbd4d1_c2040"
  | "ТВ3"      -> "s4c953a1b-dbcc-57cc-82f3-7f1c50dbd4d1_c2050"
  | "Пятница"  -> "s4c953a1b-dbcc-57cc-82f3-7f1c50dbd4d1_c2060"
  | "Звезда"   -> "s4c953a1b-dbcc-57cc-82f3-7f1c50dbd4d1_c2070"
  | "Мир"      -> "s4c953a1b-dbcc-57cc-82f3-7f1c50dbd4d1_c2080"
  | "ТНТ"      -> "s4c953a1b-dbcc-57cc-82f3-7f1c50dbd4d1_c2090"
  | "МУЗ ТВ"   -> "s4c953a1b-dbcc-57cc-82f3-7f1c50dbd4d1_c2100"
  (* do NOT edit or remove *)
  | "Россия 1"     -> "s4b135670-ef01-59b1-be78-4e9bec93461f_c1020"
  | "Радио России" -> "s4b135670-ef01-59b1-be78-4e9bec93461f_c1130"
  (* do NOT edit or remove *)
  | "Россия 24" -> "s930c63bc-0ce2-555c-9a51-09de6b1b85f2_c1070"
  | x -> x

let get_items_in_row ~(resolution : int * int) ~(item_ar : int * int) num =
  let resolution_ar = Utils.resolution_to_aspect resolution
                      |> (fun (x,y) -> (float_of_int x) /. (float_of_int y))
  in
(*  let item_ar       = (float_of_int @@ fst item_ar) /. (float_of_int @@ snd item_ar) in*)
  let squares =
    List.map (fun rows ->
        let rows = rows + 1 in
        let cols = ceil (float_of_int num /. float_of_int rows) in
        let w    = float_of_int (fst resolution) /. cols in
        let h    = w /. resolution_ar in
      (*  if Float.((w /. cols *. item_ar) *. float_of_int rows <= h)
        then int_of_float cols, rows, (w /. cols *. w /. cols *. item_ar)
        else int_of_float cols, rows,
             (h /. (float_of_int rows) *. h /. (float_of_int rows) *. item_ar)*)
        if (h *. float_of_int rows >. float_of_int @@ snd resolution) then
          0, 0.
        else
        ( let squares  = w *. h *. float_of_int num in
          let division = squares /. (float_of_int @@ fst resolution * snd resolution) in
          int_of_float cols, division)) (List.range' 0 num) in
  let (cols : int), _ =
    List.fold_left (fun acc x ->
        let _, sq = x in
        let _, gr = acc in
        if Float.(gr > sq) then acc else x)
      (0, 0.) squares in
  cols

let position_widget ~(pos : Wm.position) (widget : Wm.widget) : Wm.widget =
  let cpos = Utils.to_grid_position pos in
  let wpos = Option.map_or ~default:cpos (Dynamic_grid.Position.correct_aspect cpos) widget.aspect in
  let x    = cpos.x + ((cpos.w - wpos.w) / 2) in
  let y    = cpos.y + ((cpos.h - wpos.h) / 2) in
  let pos  = {wpos with x ; y} |> Utils.of_grid_position in
  { widget with position = pos }

let to_checkboxes (widgets : (string * Wm.widget) list) =
  let domains =
    List.fold_left (fun acc (_, (wdg : Wm.widget)) ->
        if List.exists (fun x -> String.equal x wdg.domain) acc then
          acc
        else
          wdg.domain :: acc) [] widgets
    |> List.rev
  in
  let channels = List.map (fun x -> channel_of_domain x) domains in
  let wds =
    List.map (fun str ->
        let label    = str in
        let checkbox = new Checkbox.t () in
        checkbox#set_id label;
        let form_field = new Form_field.t ~label ~input:checkbox () in
        form_field) channels
  in
  let checkbox  = new Checkbox.t () in
  let check_all = new Form_field.t ~label:"Выбрать все" ~input:checkbox () in
  check_all#set_id "checkwdg";
  React.E.map (fun checked ->
      if not checked
      then List.iter (fun x -> x#input_widget#set_checked false) wds
      else List.iter (fun x -> x#input_widget#set_checked true) wds)
  @@ React.S.changes checkbox#s_state
  |> ignore;
  check_all :: wds

let to_layout ~resolution ~domains ~widgets =
  let ar_x, ar_y   = 16, 9 in
  let domains_num  = List.length domains in
  let items_in_row = get_items_in_row ~resolution ~item_ar:(ar_x, ar_y) domains_num in
  List.fold_left (fun acc domain ->
      let i      = fst acc in
      let acc    = snd acc in
      let row    = i / items_in_row in
      let cont_w = (fst resolution) / items_in_row in
      let cont_h = (ar_y * cont_w) / ar_x in
      let cont_x = (i - items_in_row * row) * cont_w in
      let cont_y = row * cont_h in
      let cont_pos : Wm.position =
        { left   = cont_x
        ; top    = cont_y
        ; right  = cont_x + cont_w
        ; bottom = cont_y + cont_h
        } in
      let audio = find_widget ~typ:`Soundbar ~widgets ~domain in
      let video = find_widget ~typ:`Video ~widgets ~domain in
      let video_wdg =
        match video with
        | Some video ->
          let video_pos = video_position ~cont_pos in
          let video_wdg = fst video, position_widget ~pos:video_pos (snd video) in
          Some video_wdg
        | None -> None in
      let audio_wdg =
        match audio with
        | Some audio ->
          let audio_pos = audio_position ~cont_pos in
          let audio_wdg = (fst audio, {(snd audio) with position = audio_pos}) in
          Some audio_wdg
        | None -> None in
      let container =
        match video_wdg, audio_wdg with
        | Some video_wdg, Some audio_wdg ->
          Some ({ position = cont_pos
                ; widgets  = [video_wdg; audio_wdg]
                } : Wm.container)
        | None, Some audio_wdg ->
          Some ({ position = cont_pos
                ; widgets  = [audio_wdg]
                } : Wm.container)
        | Some video_wdg, None ->
          Some ({ position = cont_pos
                ; widgets  = [video_wdg]
                } : Wm.container)
        | _, _ -> None
      in
      match container with
      | Some x -> succ i, ((channel_of_domain domain), x) :: acc
      | None   -> i, acc)
    (0, []) domains
  |> snd

let to_dialog (wm : Wm.t) =
  let e, push    = React.E.create () in
  let checkboxes = to_checkboxes wm.widgets in
  let box        = new Vbox.t ~widgets:checkboxes () in
  let dialog     =
    new Dialog.t
      ~title:"Выберите виджеты"
      ~scrollable:true
      ~content:(`Widgets [box])
      ~actions:[ new Dialog.Action.t ~typ:`Decline ~label:"Отмена" ()
               ; new Dialog.Action.t ~typ:`Accept  ~label:"Применить" ()
               ]
      ()
  in
  let show = fun () ->
    Lwt.bind (dialog#show_await ())
      (function
          | `Accept ->
            let domains =
              List.fold_left (fun acc x ->
                  if not @@ x#input_widget#checked then
                    acc
                  else
                    (domain_of_channel @@ x#input_widget#id) :: acc) [] checkboxes
              |> List.filter (fun x -> not @@ String.equal x "checkwdg") in
            Lwt.return
              (push @@ to_layout ~resolution:wm.resolution ~domains ~widgets:wm.widgets)
          | `Cancel -> Lwt.return ())
  in
  dialog, e, show
