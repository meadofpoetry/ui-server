open Containers
open Components
open Common
open Board_types.Streams.TS
open Lwt_result.Infix
open Api_js.Api_types

type config =
  { stream : Stream.t
  } [@@deriving yojson]

let get_pids ~id control =
  Requests.Streams.HTTP.get_pids ~id ~limit:1 control
  >>= (function
       | Raw s ->
          (match List.head_opt s.data with
           | Some (_, pids) -> Some pids
           | None -> None)
          |> Lwt_result.return
       | _     -> Lwt.fail_with "got compressed")
  |> Lwt_result.map_err Api_js.Requests.err_to_string

let base_class = "qos-niit-pids-summary"

let colors =
  (* TODO remove text color. Write a function to calc it at runtime *)
  let open Color in
  [ Red C500, White
  ; Orange C500, Black
  ; Yellow C500, Black
  ; Green C500, White
  ; Blue C500, White
  ; Purple C500, White
  ; Grey C500, White
  ; Brown C500, White
  ; Pink C500, Black
  ; Blue_grey C500, White
  ; Deep_purple C500, White
  ; Deep_orange C500, White
  ; Indigo C500, White
  ; Amber C500, Black
  ; Light_blue C500, Black
  ]

let make_pie () =
  let open Chartjs.Pie in
  Chartjs.register_empty_state_plugin "Нет данных";
  let _class = Markup.CSS.add_element base_class "pie" in
  let box_class = Markup.CSS.add_element _class "wrapper" in
  let title_class = Markup.CSS.add_element _class "title" in
  let text = "Битрейт" in
  let title = new Typography.Text.t ~font:Caption ~text () in
  let box = Widget.create_div () in
  let dataset = new Dataset.t ~label:"dataset" Float [  ] in
  let piece_label = new Options.Piece_label.t () in
  let options = new Options.t ~piece_label () in
  dataset#set_border_width [0.];
  dataset#set_bg_color
  @@ List.map Fun.(Color.of_material % fst) colors;
  piece_label#set_font_color
  @@ List.map (fun x -> Color.Name (snd x)) colors;
  piece_label#set_render `Label;
  piece_label#set_position `Border;
  options#set_responsive true;
  options#set_maintain_aspect_ratio true;
  options#legend#set_position `Left;
  options#legend#set_display false;
  let pie =
    new t ~options
      ~width:250 ~height:250
      ~labels:[]
      ~datasets:[] () in
  object(self)

    val mutable _rate = None

    inherit Widget.t Dom_html.(createDiv document) ()

    method set_rate : bitrate option -> unit = function
      | None ->
         pie#set_datasets [];
         _rate <- None;
         pie#update None
      | Some ({ total; pids; _ } as rate) ->
         let br =
           List.fold_left (fun acc (pid, br) ->
               let open Float in
               let pct = 100. * (of_int br) / (of_int total) in
               let br = (of_int br) / 1_000_000. in
               (pid, (br, pct)) :: acc) [] pids in
         let pids, oth =
           List.fold_left (fun (pids, oth) (pid, (br, pct)) ->
               if pct >. 1. then (pid, br) :: pids, oth
               else pids, br :: oth) ([], []) br in
         let labels =
           let pids = List.map (fun x -> string_of_int @@ fst x) pids in
           match oth with
           | [] -> pids
           | _  -> pids @ ["Другие"] in
         if Option.is_none _rate
         then pie#set_datasets [dataset];
         _rate <- Some rate;
         let data =
           let pids = List.map snd pids in
           match oth with
           | [] -> pids
           | l  -> pids @ [List.fold_left (+.) 0. l] in
         pie#set_labels labels;
         dataset#set_data data;
         pie#update None

    initializer
      box#add_class box_class;
      box#append_child pie;
      title#add_class title_class;
      self#set_rate None;
      self#add_class _class;
      self#append_child title;
      self#append_child box;
  end

module Pids_box = struct

  let _class = Markup.CSS.add_element base_class "pids"
  let box_class = Markup.CSS.add_element _class "box"
  let pid_class = Markup.CSS.add_element _class "pid"
  let title_class = Markup.CSS.add_element _class "title"
  let lost_class = Markup.CSS.add_modifier pid_class "lost"

  let make_pid (pid : pid_info) =
    object(self)
      inherit Widget.t Dom_html.(createSpan document) ()

      method update (pid : pid_info) : unit =
        self#add_or_remove_class (not pid.present) lost_class

      method pid : int =
        pid.pid

      initializer
        self#update pid;
        self#add_class pid_class;
        self#set_text_content @@ Printf.sprintf "%d" pid.pid

    end

  let make_title (pids : pid_info list) =
    Printf.sprintf "PIDs (%d)" @@ List.length pids

  class t (init : pid_info list) () =
    let text = make_title init in
    let title = new Typography.Text.t ~font:Caption ~text () in
    let hbox = new Hbox.t ~widgets:[] () in
    object(self)

      val mutable _pids = []

      inherit Vbox.t ~widgets:[title#widget; hbox#widget] ()

      method add_pid (x : pid_info) =
        let pid = make_pid x in
        _pids <- pid :: _pids;
        hbox#append_child pid

      initializer
        _pids <- List.map make_pid init;
        List.iter hbox#append_child _pids;
        self#add_class _class;
        title#add_class title_class;
        hbox#add_class box_class;

  end

end

let make_timestamp_string (timestamp : Time.t option) =
  let tz_offset_s = Ptime_clock.current_tz_offset_s () in
  let s = match timestamp with
    | None -> "-"
    | Some t -> Time.to_human_string ?tz_offset_s t
  in
  "Обновлено: " ^ s

class t (timestamp : Time.t option)
        (init : pid_info list)
        () =
  let content_class = Markup.CSS.add_element base_class "content" in
  let pie = make_pie () in
  let title = "Краткая сводка" in
  let subtitle = make_timestamp_string timestamp in
  let title = new Card.Primary.title title () in
  let subtitle = new Card.Primary.subtitle subtitle () in
  let text_box = Widget.create_div () in
  let primary = new Card.Primary.t ~widgets:[text_box] () in
  let info = new Pids_box.t init () in
  let content = Widget.create_div () in
  let media = new Card.Media.t ~widgets:[content] () in
  object(self)
    inherit Card.t ~widgets:[] ()

    method update (x : pids) =
      ()

    method set_rate (x : bitrate option) =
      pie#set_rate x

    initializer
      self#add_class base_class;
      text_box#append_child title;
      text_box#append_child subtitle;
      content#add_class content_class;
      content#append_child pie;
      content#append_child info;
      self#append_child primary;
      self#append_child @@ new Divider.t ();
      self#append_child media

  end

let make ?(init : (pids option, string) Lwt_result.t option)
      (stream : Stream.t)
      (control : int) =
  let init = match init with
    | Some x -> x
    | None -> get_pids ~id:stream.id control in
  init
  >|= (function
       | None -> None, []
       | Some x -> Some x.timestamp, x.pids)
  >|= (fun (ts, data) -> new t ts data ())
  |> Ui_templates.Loader.create_widget_loader
