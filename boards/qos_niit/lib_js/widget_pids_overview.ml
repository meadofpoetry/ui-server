open Containers
open Components
open Common
open Board_types.Streams.TS
open Lwt_result.Infix
open Api_js.Api_types

type config =
  { stream : Stream.t
  }

let name = "PIDs"

let base_class = "qos-niit-pid-overview"

let settings = None

let to_pid_extra (has_pcr:bool) (is_scrambled:bool) =
  let pcr = match has_pcr with
    | false -> None
    | true  ->
       Some (new Icon.SVG.t ~icon:Icon.Markup.SVG.Path.Clock_outline ()) in
  let scr = match is_scrambled with
    | false -> None
    | true  ->
       Some (new Icon.SVG.t ~icon:Icon.Markup.SVG.Path.Lock ()) in
  let widgets = List.(cons_maybe pcr (cons_maybe scr [])) in
  new Hbox.t ~widgets ()

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

let update_row row total br pid =
  let cur, per, min, max =
    let open Table in
    match row#cells with
    | _ :: _ :: _ :: _ :: a :: b :: c :: d :: _ ->
       a, b, c, d in
  let pct = 100. *. (float_of_int br)
            /. (float_of_int total) in
  let br = (float_of_int br) /. 1_000_000. in
  cur#set_value @@ Some br;
  per#set_value @@ Some pct;
  (match min#value with
   | None -> min#set_value (Some br)
   | Some v -> if br <. v then min#set_value (Some br));
  (match max#value with
   | None -> max#set_value (Some br)
   | Some v -> if br >. v then max#set_value (Some br));
  br, pct

let make_table (init:pid_info list)
      (bitrate:bitrate React.event) =
  let dataset = new Chartjs.Pie.Dataset.t ~label:"dataset" Float [] in
  let options = new Chartjs.Pie.Options.t () in
  dataset#set_bg_color colors;
  options#set_responsive true;
  let pie = new Chartjs.Pie.t
              ~options
              ~labels:[]
              ~datasets:[dataset] () in
  (* FIXME should remember preffered state *)
  let is_hex  = false in
  let dec_pid_fmt = Table.(Int None) in
  let hex_pid_fmt = Table.(Int (Some (Printf.sprintf "0x%04X"))) in
  let br_fmt  = Table.(Option (Float None, "-")) in
  let pct_fmt = Table.(Option (Float (Some (Printf.sprintf "%.2f")), "-")) in
  let fmt =
    let open Table in
    let open Format in
    let to_sort_column = to_column ~sortable:true in
    (   to_sort_column "PID",             if is_hex then hex_pid_fmt
                                          else dec_pid_fmt)
    :: (to_sort_column "Тип",             String None)
    :: (to_column      "Доп. инфо",       Widget None)
    :: (to_sort_column "Сервис",          Option (String None, ""))
    :: (to_sort_column "Битрейт, Мбит/с", br_fmt)
    :: (to_sort_column "%",               pct_fmt)
    :: (to_sort_column "Min, Мбит/с",     br_fmt)
    :: (to_sort_column "Max, Мбит/с",     br_fmt)
    :: [] in
  let table = new Table.t ~dense:true ~fmt () in
  let on_change = fun (x:bool) ->
    List.iter (fun row ->
        let open Table in
        match row#cells with
        | pid :: _ ->
           pid#set_format (if x then hex_pid_fmt else dec_pid_fmt))
    table#rows in
  let switch  = new Switch.t ~state:is_hex ~on_change () in
  let hex     = new Form_field.t ~input:switch ~label:"HEX IDs" () in
  let actions = new Card.Actions.t ~widgets:[ hex#widget ] () in
  let media   = new Card.Media.t ~widgets:[ table ] () in
  let card    = new Card.t ~widgets:[ pie#widget
                                    ; actions#widget
                                    ; (new Divider.t ())#widget
                                    ; media#widget ] () in
  let add_row (pid:pid_info) =
    let pid_type = match pid.pid_type with
      | SEC l   ->
         let s = List.map Fun.(Mpeg_ts.(table_to_string % table_of_int)) l
                 |> String.concat ", " in
         "SEC -> " ^ s
      | PES x   ->
         let s = Mpeg_ts.stream_type_to_string x in
         "PES -> " ^ s
      | ECM x   -> "ECM -> " ^ (string_of_int x)
      | EMM x   -> "EMM -> " ^ (string_of_int x)
      | Null    -> "Null"
      | Private -> "Private" in
    let extra = to_pid_extra pid.has_pcr pid.scrambled in
    table#add_row (
        pid.pid :: pid_type :: extra :: pid.service
        :: None :: None :: None :: None :: []) in
  List.iter Fun.(ignore % add_row) init;
  let _ =
    React.E.map (fun (bitrate:bitrate) ->
        let _, br =
          List.fold_left (fun (rows, acc) (pid, br) ->
              let open Table in
              match List.find_opt (fun (row:'a Row.t) ->
                        let cell = match row#cells with a :: _ -> a in
                        cell#value = pid) rows with
              | Some x ->
                 let br, pct = update_row x bitrate.total br pid in
                 List.remove ~eq:Equal.physical ~x rows,
                 (List.cons (pid, (br, pct)) acc)
              | None   -> rows, acc) (table#rows, []) bitrate.pids in
        let pids, oth =
          List.fold_left (fun (pids, oth) (pid, (br, pct)) ->
              if pct >. 1. then (pid, br) :: pids, oth
              else pids, br :: oth) ([], []) br in
        let labels =
          let pids = List.map (fun x -> string_of_int @@ fst x) pids in
          let oth  = match oth with
            | [] -> None
            | _  -> Some "Другие" in
          List.cons_maybe oth pids in
        let data = (List.fold_left (+.) 0. oth) :: (List.map snd pids) in
        pie#set_labels labels;
        dataset#set_data data;
        pie#update None;
        bitrate) bitrate in
  let () = card#add_class base_class in
  card

let make ~(config:config) control =
  let id   = match config.stream.id with
    | `Ts id -> id
    | `Ip _  -> failwith "UDP" in
  let init =
    Requests.Streams.HTTP.get_pids ~id ~limit:1 control
    >>= (function
         | Raw s -> Lwt_result.return s.data
         | _     -> Lwt.fail_with "got compressed") in
  let loader =
    init
    >|= (fun init ->
      let e, sock = Requests.Streams.WS.get_bitrate ~id control in
      let pids = match List.head_opt init with
        | Some (_, pids) -> pids.pids
        | None -> [] in
      make_table pids e)
    >|= Widget.coerce
    |> Lwt_result.map_err Api_js.Requests.err_to_string
    |> Ui_templates.Loader.create_widget_loader
  in loader


