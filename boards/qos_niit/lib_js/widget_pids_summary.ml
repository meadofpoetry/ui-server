open Containers
open Components
open Common
open Board_types.Streams.TS
open Lwt_result.Infix
open Api_js.Api_types

type config =
  { stream : Stream.t
  } [@@deriving yojson]

let base_class = "qos-niit-pids-summary"

let colors =
  Color.[ Red C500
        ; Orange C500
        ; Yellow C500
        ; Green C500
        ; Blue C500
        ; Purple C500
        ; Grey C500
        ; Brown C500
        ; Pink C500
        ; Blue_grey C500
        ; Deep_purple C500
        ; Deep_orange C500
        ; Indigo C500
        ; Amber C500
        ; Light_blue C500 ]
  |> List.map Color.rgb_of_name

let font_colors =
  let open CSS.Color in
  [ White
  ; Black
  ; Black
  ; White
  ; White
  ; White
  ; White
  ; White
  ; Black
  ; White
  ; Black
  ; White
  ; Black
  ; Black ]
  |> List.map (fun x -> Name x)

let make_pie (bitrate:bitrate React.event) =
  let dataset     = new Chartjs.Pie.Dataset.t ~label:"dataset" Float [  ] in
  let piece_label = new Chartjs.Pie.Options.Piece_label.t () in
  let options     = new Chartjs.Pie.Options.t ~piece_label () in
  dataset#set_bg_color colors;
  piece_label#set_render `Label;
  piece_label#set_position `Border;
  piece_label#set_font_color font_colors;
  options#set_responsive true;
  options#legend#set_position `Left;
  options#legend#set_display false;
  let pie = new Chartjs.Pie.t
              ~options
              ~labels:[]
              ~datasets:[dataset] () in
  pie#set_width "100%";
  pie#set_height "100%";
  let _e =
    React.E.map (fun (bitrate:bitrate) ->
        let br =
          List.fold_left (fun acc (pid, br) ->
              let pct = 100. *. (float_of_int br)
                        /. (float_of_int bitrate.total) in
              let br = (float_of_int br) /. 1_000_000. in
              (pid, (br, pct)) :: acc) [] bitrate.pids in
        let pids, oth =
          List.fold_left (fun (pids, oth) (pid, (br, pct)) ->
              if pct >. 1. then (pid, br) :: pids, oth
              else pids, br :: oth) ([], []) br in
        let labels =
          let pids = List.map (fun x -> string_of_int @@ fst x) pids in
          match oth with
          | [] -> pids
          | _  -> pids @ ["Другие"] in
        let data =
          let pids = List.map snd pids in
          match oth with
          | [] -> pids
          | l  -> pids @ [List.fold_left (+.) 0. l] in
        pie#set_labels labels;
        dataset#set_data data;
        pie#update None) bitrate in
  Lwt_react.E.keep _e;
  (* let title = new Typography.Text.t
   *               ~font:Subtitle_1
   *               ~text:"Распределение битрейта"
   *               () in *)
  let pie_wrapper = Widget.create_div () in
  (* let () = pie_wrapper#append_child title in *)
  let () = pie_wrapper#append_child pie in
  let () = pie_wrapper#add_class @@ Markup.CSS.add_element base_class "pie" in
  pie_wrapper

let make_info (pids:pid_info list)
      (bitrate:bitrate React.event) =
  Widget.create_div ()

let make_summary (pids:pid_info list)
      (bitrate:bitrate React.event) =
  let pie   = make_pie bitrate in
  let info  = make_info pids bitrate in
  let hbox  = new Hbox.t ~widgets:[ pie#widget; info#widget ] () in
  let media = new Card.Media.t ~widgets:[ hbox ] () in
  let card  = new Card.t ~widgets:[ media ] () in
  let ()    = card#add_class base_class in
  card

let make ~(config:config)
      (init:(pid_info list, string) Lwt_result.t)
      (bitrate:bitrate React.event)
      control =
  (* let id = match config.stream.id with
   *   | `Ts id -> id
   *   | _      -> failwith "bad id" in *)
  let loader =
    init
    >|= (fun init -> make_summary init bitrate)
    >|= Widget.coerce
    |> Ui_templates.Loader.create_widget_loader
  in loader
