open Containers
open Components
open Wm_components
open Common.Stream

let split_three l =
  List.fold_left (fun (first, second, third) (a, b, c) ->
        a :: first, b :: second, c :: third) ([], [], []) l

let find_domains (widgets : (string * Wm.widget) list)=
    List.fold_left (fun acc (_, (wdg : Wm.widget)) ->
        if List.exists (fun x -> String.equal x wdg.domain) acc then
          acc
        else
          wdg.domain :: acc) [] widgets
    |> List.rev

let find_widget ~typ ~widgets ~domain =
  let find_with_typ typ =
    List.find_pred (fun (_, (x : Wm.widget)) ->
        String.equal x.domain domain && String.equal x.type_ typ) widgets in
  match typ with
  | `Soundbar -> find_with_typ "soundbar"
  | `Video    -> find_with_typ "video"
  | _         -> None

let soundbar_ch_w = 10
let channels = 2

let video_position ~(cont_pos : Wm.position) ~audio : Wm.position =
  match audio with
  | `With_audio ->
    { left   = cont_pos.left
    ; top    = cont_pos.top
    ; right  = cont_pos.right - soundbar_ch_w * channels
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
    { left   = cont_pos.right - soundbar_ch_w * channels
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
let stream_of_domain domain = String.sub domain 1 (String.length domain - 13)

let channel_of_domain domain = String.sub domain (String.length domain - 10) 4

let pid_of_domain domain = String.sub domain (String.length domain - 4) 4

let parse_stream stream signal =
  let default = "", "" in
  let structure = React.S.value signal in
  let id = Common.Stream.ID.of_string stream in
  match List.find_pred (fun (x : Structure.packed) ->
      Common.Stream.ID.equal x.structure.id id) structure with
  | None -> default
  | Some packed -> Common.Stream.Source.to_string packed.source.source.info,
                   Common.Url.to_string packed.structure.uri

let parse_channel domain (signal : Structure.Streams.t React.signal) =
  let default   = "", "" in
  let structure = React.S.value signal in
  let channel   = int_of_string @@ channel_of_domain domain in
  let id = Common.Stream.ID.of_string @@ stream_of_domain domain in
  match List.find_pred (fun (x : Structure.packed) ->
      Common.Stream.ID.equal x.structure.id id) structure with
  | None -> default
  | Some packed ->
    match List.find_pred (fun (ch : Structure.channel) ->
        ch.number = channel)
        packed.structure.channels with
    | None -> default
    | Some channel -> channel.service_name, channel.provider_name

let parse_widget domain (signal : Structure.Streams.t React.signal) =
  let default   = "", "" in
  let structure = React.S.value signal in
  let channel   = int_of_string @@ channel_of_domain domain in
  let pid_      = int_of_string @@ pid_of_domain domain in
  let id = Common.Stream.ID.of_string @@ stream_of_domain domain in
  match List.find_pred (fun (x : Structure.packed) ->
      Common.Stream.ID.equal x.structure.id id) structure with
  | None -> default
  | Some packed ->
    match List.find_pred (fun (ch : Structure.channel) ->
        ch.number = channel)
        packed.structure.channels with
    | None -> default
    | Some channel ->
      match List.find_pred (fun (pid : Structure.pid) ->
          pid.pid = pid_) channel.pids with
      | None -> default
      | Some pid -> string_of_int pid.pid, pid.stream_type_name

let get_items_in_row ~(resolution : int * int) ~(item_ar : int * int) num =
  let calculate_cols_rows () =
    let resolution_ar =
      Utils.resolution_to_aspect resolution
      |> (fun (x, y) -> (float_of_int x) /. (float_of_int y)) in
    let squares =
      List.map (fun rows ->
          let rows = rows + 1 in
          let cols = ceil (float_of_int num /. float_of_int rows) in
          let w    = float_of_int (fst resolution) /. cols in
          let h    = w /. resolution_ar in
          if (h *. float_of_int rows >. float_of_int @@ snd resolution) then
            0, 0, 0.
          else
            ( let squares  = w *. h *. float_of_int num in
              int_of_float cols, rows, squares)) (List.range' 0 num) in
    let (cols : int), (rows : int), _ =
      List.fold_left (fun acc x ->
          let _, _, sq = x in
          let _, _, gr = acc in
          if Float.(gr > sq) then acc else x)
        (1, 1, 1.) squares in
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

(* position_widget_1 works better but it still adds slight distortion to an image *)
let position_widget_1 ~(pos : Wm.position) (widget : Wm.widget) : Wm.widget =
  match widget.aspect with
  | Some aspect ->
    let w = pos.right - pos.left in
    let h = pos.bottom - pos.top in
    let new_w, new_h =
      if w / (fst aspect) * (snd aspect) > h then
        h / (snd aspect) * (fst aspect), h
      else
        w, w / (fst aspect) * (snd aspect) in
    if new_w > 0 && new_w <= w
       && new_h > 0 && new_h <= h then
      let left = pos.left + (w - new_w) / 2 in
      let top  = pos.top + (h - new_h) / 2 in
      let position  = { pos with left
                               ; top
                               ; right = left + new_w
                               ; bottom = top + new_h
                      } in
      { widget with position }
    else
      widget
  | None -> widget

(* position_widget works properly, but leaves more blank space *)
let position_widget ~(pos : Wm.position) (widget : Wm.widget) : Wm.widget =
  let cpos = Utils.to_grid_position pos in
  let wpos =
    Option.map_or
      ~default:cpos
      (Dynamic_grid.Position.correct_aspect cpos)
      widget.aspect in
  let x    = cpos.x + ((cpos.w - wpos.w) / 2) in
  let y    = cpos.y + ((cpos.h - wpos.h) / 2) in
  let pos  = { wpos with x ; y } |> Utils.of_grid_position in
  { widget with position = pos }

(* makes a checkbox with id of domains and typ, and a tree item named by channel*)
let make_widget (widget : string * Wm.widget) signal =
  let domain = (snd widget).domain in
  let typ    = (snd widget).type_ in
  let text, secondary_text = parse_widget domain signal in
  let checkbox = new Checkbox.t () in
  checkbox#set_id @@ domain ^ "|" ^ typ;
  checkbox,
  new Tree.Item.t ~text ~secondary_text  ~graphic:checkbox ~value:() ()

(* makes all the widgets checkboxes with IDs, checkboxes of channels Tree items,
 * and a Tree.t containing all given channels *)
let make_channels (widgets : (string * Wm.widget) list) signal =
  let domains  = find_domains widgets in
  let channels =
    List.map (fun domain -> parse_channel domain signal, domain) domains in
  let wdg_chbs, ch_chbs, items =
    List.map (fun (channel, domain) ->
        let text, secondary_text = channel in
        let widgets =
          List.filter (fun (_, (wdg : Wm.widget)) ->
              String.equal domain wdg.domain) widgets in
        let checkboxes, wds =
          List.split @@ List.map (fun widget -> make_widget widget signal) widgets in
        let checkbox = new Checkbox.t () in
        checkbox#set_id text;
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
        new Tree.Item.t ~text ~secondary_text ~graphic:checkbox
          ~nested ~value:() ()) channels
    |> split_three in
  (List.concat wdg_chbs), ch_chbs, new Tree.t ~items ()

(* makes all the widget checkboxes with IDs, and a Tree.t containing all streams *)
let make_streams (widgets : (string * Wm.widget) list) tree signal =
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
        let text, secondary_text = parse_stream stream signal in
        let wdg_chbs, chan_chbs, nested = make_channels wds signal in
        let checkbox = new Checkbox.t () in
        checkbox#set_id stream;
        React.E.map (fun checked ->
            if checked then
              List.iter (fun ch -> ch#set_checked true) chan_chbs
            else
              List.iter (fun ch -> ch#set_checked false) chan_chbs)
        @@ React.S.changes checkbox#s_state
        |> ignore;
        List.iter (fun check ->
            React.E.map (fun checked ->
                if not checked
                && Bool.equal (React.S.value checkbox#s_state) true then
                  checkbox#set_checked false)
            @@ React.S.changes check#s_state
            |> ignore) chan_chbs;
        let stream_node =
          new Tree.Item.t
            ~text
            ~secondary_text
            ~graphic:checkbox
            ~nested ~value:()
            () in
        (wdg_chbs @ (fst acc)), stream_node :: (snd acc))
      ([], []) streams_of_widgets in
  List.iter (fun item -> tree#append_item item) items;
  checkboxes

(* makes a list of containers with widgets, calculates its positions *)
let to_layout ~resolution ~widgets =
  let ar_x, ar_y = 16, 9 in
  let domains    = find_domains widgets in
  let num        = List.length domains in
  if num <> 0 then
    let cols, rows =
      get_items_in_row ~resolution ~item_ar:(ar_x, ar_y) num in
    let cont_std_w = fst resolution / cols in
    let cont_std_h = (ar_y * cont_std_w) / ar_x in
    let remain = List.length domains - (cols * (rows - 1)) in
    (* 'greatest' is the number of containers we should increase in size,
     * 'multiplier' is the number to multiply width and height on *)
    let greatest, multiplier =
      if rows < cols
      && remain <> cols then
        if float_of_int remain /. float_of_int cols <=. 0.5 then
          remain, 2
        else
          (* if the number of remaining containers
           * is greater than the half of the row *)
          cols - remain, 2
      else
        remain, 1 in
    List.fold_left (fun acc domain ->
        let i, acc  = acc in
        let row_num = i / cols in
        let channel, _ = "channel", "provider" in
        let cont_w  =
          if i + 1 > List.length domains - remain then
            fst resolution / cols * multiplier
          else
            fst resolution / cols in
        let cont_h = (ar_y * cont_w) / ar_x in
        let greater_num = i - (num - greatest) in
        (* the number of greater elements behind this *)
        let cont_x =
          if greater_num > 0 then               (* magical *)
            (i - cols * row_num - greater_num)  (* do not touch *)
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
              (* actually we should use position_widget here,
               * but it leaves more blank space *)
             (* fst video, {(snd video) with position = video_pos} in *)
              fst video, position_widget_1 ~pos:video_pos (snd video) in
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
             && cont_pos.top >= 0 && cont_pos.bottom <= snd resolution then
            let widgets = match video_wdg, audio_wdg with
              | Some video_wdg, Some audio_wdg -> [video_wdg; audio_wdg]
              | None,           Some audio_wdg -> [audio_wdg]
              | Some video_wdg, None           -> [video_wdg]
              | _ -> [] in
            match widgets with
            | [] -> None
            | widgets -> Some ({ position = cont_pos; widgets } : Wm.container)
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

(* makes a dialog which shows available
 * streams
 *    |_ channels
 *        |_ widgets
 * returns dialog, react event and a fun showing dialog *)
let to_dialog (wm : Wm.t) =
  let open Lwt_result.Infix in
  let e, push   = React.E.create () in
  let checkboxes, push_ch = React.S.create [] in
  let tree    = new Tree.t ~items:[] () in
  let thread =
    Requests.get_structure ()
  >|= (fun init ->
    let e, _ = Requests.get_structure_socket () in
    let signal = React.S.hold init e in
    push_ch @@ make_streams wm.widgets tree signal;
    tree#widget)
  |> Lwt_result.map_err Api_js.Requests.err_to_string in
  let loader = Ui_templates.Loader.create_widget_loader thread in
  let box        = new Vbox.t ~widgets:[loader#widget] () in
  let dialog     =
    new Dialog.t
      ~title:"Выберите виджеты"
      ~scrollable:true
      ~content:(`Widgets [box])
      ~actions:[ new Dialog.Action.t ~typ:`Cancel ~label:"Отмена" ()
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
                    x#id :: acc) [] (React.S.value checkboxes) in
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
