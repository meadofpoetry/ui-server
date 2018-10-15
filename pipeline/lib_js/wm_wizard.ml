open Containers
open Components
open Wm_components

let find_domains (widgets : (string * Wm.widget) list)=
    List.fold_left (fun acc (_, (wdg : Wm.widget)) ->
        if List.exists (fun x -> String.equal x wdg.domain) acc then
          acc
        else
          wdg.domain :: acc) [] widgets
    |> List.rev

let find_widget ~typ ~widgets ~domain =
  match typ with
  | `Soundbar ->
    List.find_pred (fun (_, (x : Wm.widget)) ->
        String.equal x.domain domain && String.equal x.type_ "soundbar") widgets
  | `Video ->
    List.find_pred (fun (_, (x : Wm.widget)) ->
        String.equal x.domain domain && String.equal x.type_ "video") widgets
  | _ -> None

let video_position ~(cont_pos : Wm.position) ~audio : Wm.position =
  match audio with
  | `With_audio ->
    { left   = cont_pos.left
    ; top    = cont_pos.top
    ; right  = cont_pos.right - 30
    ; bottom = cont_pos.bottom
    }
  | `Without_audio ->
    { left   = cont_pos.left
    ; top    = cont_pos.top
    ; right  = cont_pos.right
    ; bottom = cont_pos.bottom
    }

let audio_position ~(cont_pos : Wm.position) ~video : Wm.position =
  match video with
  | `With_video ->
    { left   = cont_pos.right - 30
    ; top    = cont_pos.top
    ; right  = cont_pos.right
    ; bottom = cont_pos.bottom
    }
  | `Without_video ->
    { left   = cont_pos.left
    ; top    = cont_pos.top
    ; right  = cont_pos.right
    ; bottom = cont_pos.bottom
    }

let channel_of_domain = function
  (* do NOT edit or remove
   * first multiplex *)
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
  (* do NOT edit or remove
   * second multiplex *)
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
  (* do NOT edit or remove
   * first multiplex *)
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
  (* do NOT edit or remove
   * second multiplex *)
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
  let calculate_cols_rows () =
    let resolution_ar =
      Utils.resolution_to_aspect resolution
      |> (fun (x,y) -> (float_of_int x) /. (float_of_int y)) in
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
            0, 0, 0.
          else
            ( let squares  = w *. h *. float_of_int num in
              let division =
                squares /. (float_of_int @@ fst resolution * snd resolution) in
              int_of_float cols, rows, division)) (List.range' 0 num) in
    let (cols : int), (rows : int), _ =
      List.fold_left (fun acc x ->
          let _, _, sq = x in
          let _, _, gr = acc in
          if Float.(gr > sq) then acc else x)
        (0, 0, 0.) squares in
    cols, rows in
  if ( Float.equal (float_of_int (fst resolution) /. float_of_int (snd resolution))
     (float_of_int (fst item_ar) /. float_of_int (snd item_ar))) then
    match num with
    | 1 -> 1, 1
    | 2 -> 2, 1
    | 3 | 4 -> 2, 2
    | 5 | 6 -> 3, 2
    | x when x >= 7  && x <= 9  -> 3, 3
    | x when x >= 10 && x <= 12 -> 4, 3
    | x when x >= 13 && x <= 16 -> 4, 4
    | x when x >= 17 && x <= 20 -> 5, 4
    | x when x >= 21 && x <= 25 -> 5, 5
    | x when x >= 26 && x <= 30 -> 6, 5
    | x when x >= 31 && x <= 36 -> 6, 6
    | x when x >= 37 && x <= 42 -> 7, 6
    | x when x >= 43 && x <= 49 -> 7, 7
    | x when x >= 50 && x <= 56 -> 8, 7
    | x when x >= 57 && x <= 64 -> 8, 8
    | x when x >= 65 && x <= 72 -> 9, 8
    | x when x >= 73 && x <= 81 -> 9, 9
    | x when x >= 82 && x <= 90 -> 10, 9
    | x when x >= 90 && x <= 100 -> 10, 10
    | _ -> calculate_cols_rows ()
  else calculate_cols_rows ()

let position_widget ~(pos : Wm.position) (widget : Wm.widget) : Wm.widget =
  let cpos = Utils.to_grid_position pos in
  let wpos =
    Option.map_or
      ~default:cpos
      (Dynamic_grid.Position.correct_aspect cpos)
      widget.aspect in
  let x    = cpos.x + ((cpos.w - wpos.w) / 2) in
  let y    = cpos.y + ((cpos.h - wpos.h) / 2) in
  let pos  = {wpos with x ; y} |> Utils.of_grid_position in
  { widget with position = pos }

let make_widget (widget : string * Wm.widget) =
  let domain = (snd widget).domain in
  let typ    = (snd widget).type_ in
  let label  =
    match (snd widget).description with
    | "video widget"    -> "Видео"
    | "soundbar widget" -> "Аудио"
    | x -> x in
  let checkbox = new Checkbox.t () in
  checkbox#set_id @@ domain ^ "|" ^ typ;
  checkbox, new Tree.Item.t ~text:label ~secondary_text:(channel_of_domain domain)
    ~graphic:checkbox ~value:() ()

let make_channels (widgets : (string * Wm.widget) list) =
  let domains  = find_domains widgets in
  let channels = List.map (fun x -> channel_of_domain x) domains in
  let wdg_chbs, ch_chbs, items =
    List.map (fun channel ->
        let label   = channel in
        let domain  = domain_of_channel channel in
        let widgets =
          List.filter (fun (_, (wdg : Wm.widget)) ->
              String.equal domain wdg.domain) widgets in
        let checkboxes, wds =
          List.split @@ List.map (fun widget -> make_widget widget) widgets in
        let checkbox = new Checkbox.t () in
        checkbox#set_id label;
        React.E.map (fun checked ->
            if checked then
              List.iter (fun ch -> ch#set_checked true) checkboxes
            else
              List.iter (fun ch -> ch#set_checked false) checkboxes)
        @@ React.S.changes checkbox#s_state
        |> ignore;
        List.iter (fun check ->
            React.E.map (fun checked ->
                if not checked
                && Bool.equal (React.S.value checkbox#s_state) true then
                  checkbox#set_checked false)
            @@ React.S.changes check#s_state
            |> ignore) checkboxes;
        let nested  = new Tree.t ~items:wds () in
        checkboxes, checkbox,
        new Tree.Item.t ~text:label ~graphic:checkbox
          ~nested ~value:() ()) channels
    |> List.fold_left (fun (first, second, third) (a, b, c) ->
        a :: first, b :: second, c :: third) ([], [], []) in
  (List.concat wdg_chbs), ch_chbs, new Tree.t ~items ()

let make_streams (widgets : (string * Wm.widget) list) =
  let stream_of_domain domain =
    let len = String.length domain - 6 in
    String.sub domain 0 len in
  let streams =
    List.fold_left (fun acc (x : string * Wm.widget) ->
        let stream = stream_of_domain (snd x).domain in
        if List.exists (fun x ->
            String.equal stream x) acc then
          acc
        else
          stream :: acc) [] widgets in
  let streams_of_widgets =
    List.map (fun stream ->
        let wds =
          List.filter (fun (x : string * Wm.widget) ->
              let wdg_stream = stream_of_domain (snd x).domain in
              String.equal stream wdg_stream) widgets in
      stream, wds) streams in
  let checkboxes, items =
    List.fold_left (fun acc (stream, wds) ->
        let wdg_chbs,ch_chbs, nested = make_channels wds in
        let checkbox = new Checkbox.t () in
        checkbox#set_id stream;
        React.E.map (fun checked ->
            if checked then
              List.iter (fun ch -> ch#set_checked true) ch_chbs
            else
              List.iter (fun ch -> ch#set_checked false) ch_chbs)
        @@ React.S.changes checkbox#s_state
        |> ignore;
        List.iter (fun check ->
            React.E.map (fun checked ->
                if not checked
                && Bool.equal (React.S.value checkbox#s_state) true then
                  checkbox#set_checked false)
            @@ React.S.changes check#s_state
            |> ignore) ch_chbs;
        let stream_node = new Tree.Item.t ~text:stream ~graphic:checkbox
          ~nested ~value:() () in
        (wdg_chbs @ (fst acc)), stream_node :: (snd acc)) ([], []) streams_of_widgets in
  checkboxes, new Tree.t ~items ()
(* TODO streams must be the first level of the menu *)

let to_layout ~resolution ~widgets =
  let ar_x, ar_y = 16, 9 in
  let domains    = find_domains widgets in
  let num        = List.length domains in
  if num <> 0 then
    let cols, rows =
      get_items_in_row ~resolution ~item_ar:(ar_x, ar_y) num in
    let remain = List.length domains - (cols * (rows - 1)) in
    (* greatest is the number of containers we should increase in size,
     * multiplier is the number to multiply width and height on *)
    let greatest, multiplier =
      if rows < cols
      && remain <> cols then
        if float_of_int remain /. float_of_int cols <=. 0.5 then
          remain, 2
        else
          (* if the number of remaining containers
           * is greater than the half of the row *)
          1, 2
      else
        remain, 1 in
    let cont_std_w = fst resolution / cols in
    let cont_std_h = (ar_y * cont_std_w) / ar_x in
    (*  let start = (snd resolution - rows_act * cont_h) / 2 in *)
    List.fold_left (fun acc domain ->
        let i       = fst acc in
        let acc     = snd acc in
        let row_num = i / cols in
        let channel = channel_of_domain domain in
        let cont_w  =
          if i + 1 > List.length domains - remain then
            fst resolution / cols * multiplier
          else
            fst resolution / cols in
        let cont_h = (ar_y * cont_w) / ar_x in
        let greater_num = i - (num - greatest) in
        (* the number of greater elements behind this *)
        let cont_x = if greater_num > 0 then      (* magical *)
            (i - cols * row_num - greater_num)    (* do not touch *)
            * cont_std_w + greater_num * cont_w
          else
            (i - cols * row_num) * cont_std_w in
        let cont_y = row_num * cont_std_h in
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
            let video_pos =
              match audio with
              | Some _ -> video_position ~audio:`With_audio ~cont_pos
              | None  ->  video_position ~audio:`Without_audio ~cont_pos in
            let video_wdg =
              fst video, position_widget ~pos:video_pos (snd video) in
            Some video_wdg
          | None -> None in
        let audio_wdg =
          match audio with
          | Some audio ->
            let audio_pos =
              match video with
              | Some _ -> audio_position ~video:`With_video ~cont_pos
              | None   -> audio_position ~video:`Without_video ~cont_pos in
            let audio_wdg =
              fst audio, {(snd audio) with position = audio_pos} in
            Some audio_wdg
          | None -> None in
        let container =
          if cont_pos.left >= 0 && cont_pos.right <= fst resolution
             && cont_pos.top >= 0 && cont_pos.bottom <= snd resolution
          then
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
          else
            (Printf.printf "Error building container %s!\n" channel;
             None) in
        match container with
        | Some x -> succ i, (channel, x) :: acc
        | None   -> i, acc)
      (0, []) domains
    |> snd
  else
    []

let to_dialog (wm : Wm.t) =
  let e, push    = React.E.create () in
  let checkboxes, widget = make_streams wm.widgets in
  let box        = new Vbox.t ~widgets:[widget#widget] () in
  let dialog     =
    new Dialog.t
      ~title:"Выберите виджеты"
      ~scrollable:true
      ~content:(`Widgets [box])
      ~actions:[ new Dialog.Action.t ~typ:`Decline ~label:"Отмена" ()
               ; new Dialog.Action.t ~typ:`Accept  ~label:"Применить" ()
               ]
      () in
  let show = fun () ->
    Lwt.bind (dialog#show_await ())
      (function
          | `Accept ->
            let wds =
              List.fold_left (fun acc x ->
                  if not @@ x#checked then
                    acc
                  else
                    x#id :: acc) [] checkboxes in
            let wds = List.map (fun x ->
                  let index = String.index x '|' in
                  let length = String.length x - index - 1 in
                  String.sub x 0 index, String.sub x (index + 1) length) wds in
            let widgets =
              List.fold_left (fun acc (domain, typ) ->
                  match List.find_pred (fun (name, (wdg : Wm.widget)) ->
                      String.equal domain wdg.domain
                      && String.equal typ wdg.type_) wm.widgets with
                  | Some x -> x :: acc
                  | None   -> acc) [] wds in
            Lwt.return
              (push @@ to_layout ~resolution:wm.resolution ~widgets)
          | `Cancel -> Lwt.return ()) in
  dialog, e, show
