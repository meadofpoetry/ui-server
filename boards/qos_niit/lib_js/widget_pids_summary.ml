open Containers
open Components
open Common
open Board_types.Streams.TS
open Lwt_result.Infix
open Api_js.Api_types
open Widget_common

type config =
  { stream : Stream.t
  } [@@deriving yojson]

module Pid_info = struct
  type t = pid_info

  let compare (a : t) (b : t) : int =
    Int.compare a.pid b.pid
end

module Set = Set.Make(Pid_info)

let base_class = "qos-niit-pids-summary"

(* TODO improve tooltip. add pid type, bitrate units *)
module Pie = struct

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
        ~datasets:[] ()
    in
    pie, dataset

  class t ?(hex = false) () =
    let _class = Markup.CSS.add_element base_class "pie" in
    let box_class = Markup.CSS.add_element _class "wrapper" in
    let title_class = Markup.CSS.add_element _class "title" in
    let text = "Битрейт" in
    let title = new Typography.Text.t ~font:Caption ~text () in
    let box = Widget.create_div () in
    let pie, dataset = make_pie () in
    object(self)

      val mutable _hex = hex
      val mutable _rate = None

      inherit Widget.t Dom_html.(createDiv document) ()

      method set_hex (x : bool) : unit =
        _hex <- x;
        match _rate with
        | None -> ()
        | Some (pids, oth) ->
           pie#set_labels @@ self#make_labels pids oth;
           pie#update None;

      method set_rate : bitrate option -> unit = function
        | None ->
           pie#set_datasets [];
           _rate <- None;
           pie#update None
        | Some { total; pids; _ } ->
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
           let labels = self#make_labels pids oth in
           if Option.is_none _rate
           then pie#set_datasets [dataset];
           _rate <- Some (pids, oth);
           let data =
             let pids = List.map snd pids in
             match oth with
             | [] -> pids
             | l  -> pids @ [List.fold_left (+.) 0. l] in
           pie#set_labels labels;
           dataset#set_data data;
           pie#update None

      (* Private methods *)

      method private make_labels pids oth =
        let to_string =
          if _hex then PID.to_hex_string
          else PID.to_dec_string in
        let pids = List.map (fun x -> to_string @@ fst x) pids in
        match oth with
        | [] -> pids
        | _  -> pids @ ["Другие"]

      initializer
        box#add_class box_class;
        box#append_child pie;
        title#add_class title_class;
        self#set_rate None;
        self#add_class _class;
        self#append_child title;
        self#append_child box;
    end

end

(* TODO show only N pids, and allow user to expand if necessary
   TODO sort pids by value *)
module Info = struct

  let _class = Markup.CSS.add_element base_class "info"
  let rate_class = Markup.CSS.add_element _class "rate"

  let make_rate () =
    let na = "n/a" in
    let to_string = Printf.sprintf "%f Мбит/с" in
    let set meta = function
      | None -> meta#set_text na
      | Some x -> meta#set_text @@ to_string x in
    let total, set_total =
      let meta = new Typography.Text.t ~text:na () in
      let item = new Item_list.Item.t
                   ~text:"Общий битрейт: "
                   ~value:None
                   ~meta
                   () in
      item, set meta in
    let effective, set_effective =
      let meta = new Typography.Text.t ~text:na () in
      let item = new Item_list.Item.t
                   ~text:"Полезный битрейт: "
                   ~value:None
                   ~meta
                   () in
      item, set meta in
    let list =
      new Item_list.t
        ~dense:true
        ~non_interactive:true
        ~items:[ `Item total
               ; `Item effective ] ()
    in
    list#add_class rate_class;
    list, set_total, set_effective

  module Pids = struct

    let _class = Markup.CSS.add_element _class "pids"
    let box_class = Markup.CSS.add_element _class "box"
    let pid_class = Markup.CSS.add_element _class "pid"
    let title_class = Markup.CSS.add_element _class "title"
    let lost_class = Markup.CSS.add_modifier pid_class "lost"

    let make_title num =
      Printf.sprintf "PIDs (%d)" num

    let make_pid ?(hex = false) (pid : pid_info) =
      object(self)
        inherit Widget.t Dom_html.(createSpan document) ()

        method update (pid : pid_info) : unit =
          self#add_or_remove_class (not pid.present) lost_class

        method set_hex (x : bool) : unit =
          let s = match x with
            | true -> PID.to_hex_string self#pid
            | false -> PID.to_dec_string self#pid in
          self#set_text_content s

        method pid : int =
          pid.pid

        initializer
          self#update pid;
          self#add_class pid_class;
          self#set_hex hex
      end

    class t ?hex (init : pid_info list) () =
      let text = make_title @@ List.length init in
      let title = new Typography.Text.t ~font:Caption ~text () in
      let pids_box = new Hbox.t ~widgets:[] () in
      object(self)

        val mutable _pids = []

        inherit Vbox.t
                  ~widgets:[ title#widget
                           ; pids_box#widget] ()

        method update ~(lost : Set.t)
                 ~(found : Set.t)
                 ~(changed : Set.t) : unit =
          Set.iter self#remove_pid lost;
          Set.iter self#update_pid changed;
          Set.iter self#add_pid found;
          title#set_text @@ make_title @@ List.length _pids;

        method set_hex (x : bool) : unit =
          List.iter (fun cell -> cell#set_hex x) _pids

        (* Private methods *)

        method private update_pid (x : pid_info) : unit =
          match List.find_opt (fun cell ->
                    cell#pid = x.pid) _pids with
          | None -> ()
          | Some cell -> cell#update x

        method private add_pid (x : pid_info) : unit =
          let pid = make_pid x in
          _pids <- pid :: _pids;
          pids_box#append_child pid

        method private remove_pid (x : pid_info) : unit =
          match List.find_opt (fun cell ->
                    cell#pid = x.pid) _pids with
          | None -> ()
          | Some cell ->
             pids_box#remove_child cell;
             cell#destroy ();
             _pids <- List.remove ~eq:Widget.equal ~x:cell _pids

        initializer
          _pids <- List.map (make_pid ?hex) init;
          List.iter pids_box#append_child _pids;
          self#add_class _class;
          title#add_class title_class;
          pids_box#add_class box_class;

      end

  end

  class t ?hex (init : pid_info list) () =
    let rate, set_total, set_effective = make_rate () in
    let pids = new Pids.t ?hex init () in
    object(self)

      val mutable _pids = []

      inherit Vbox.t
                ~widgets:[ rate#widget
                         ; (new Divider.t ())#widget
                         ; pids#widget ] ()

      method set_rate (rate : bitrate option) =
        match rate with
        | None -> set_total None; set_effective None
        | Some x ->
           let null = List.Assoc.get ~eq:(=) 0x1FFF x.pids
                      |> Option.get_or ~default:0 in
           let e = x.total - null in
           let e = Float.(of_int e / 1_000_000.) in
           let v = Float.(of_int x.total / 1_000_000.) in
           set_total (Some v);
           set_effective (Some e);

      method update ~(lost : Set.t)
               ~(found : Set.t)
               ~(changed : Set.t) : unit =
        pids#update ~lost ~found ~changed;

      method set_hex (x : bool) : unit =
        pids#set_hex x

      initializer
        self#add_class _class

  end

end

class t (timestamp : Time.t option)
        (init : pid_info list)
        () =
  (* FIXME read from storage *)
  let is_hex = false in
  let content_class = Markup.CSS.add_element base_class "content" in
  let title = "Сводка" in
  let subtitle = make_timestamp_string timestamp in
  let title = new Card.Primary.title ~large:true title () in
  let subtitle = new Card.Primary.subtitle subtitle () in
  let text_box = Widget.create_div () in
  let pie = new Pie.t ~hex:is_hex () in
  let info = new Info.t ~hex:is_hex init () in
  let content = Widget.create_div () in
  let on_change = fun (x : bool) ->
    pie#set_hex x;
    info#set_hex x in
  let switch = new Switch.t ~state:is_hex ~on_change () in
  let hex = new Form_field.t ~input:switch ~label:"HEX IDs" () in
  let primary = new Card.Primary.t ~widgets:[text_box; hex#widget] () in
  let media = new Card.Media.t ~widgets:[content] () in
  object(self)

    val mutable _timestamp : Time.t option = timestamp
    val mutable _data : Set.t = Set.of_list init

    inherit Card.t ~widgets:[] ()

    method update ({ timestamp; pids } : pids) =
      (* Update timestamp *)
      _timestamp <- Some timestamp;
      subtitle#set_text_content @@ make_timestamp_string _timestamp;
      (* Manage found, lost and updated items *)
      let prev = _data in
      _data <- Set.of_list pids;
      let lost = Set.diff prev _data in
      let found = Set.diff _data prev in
      let inter = Set.inter prev _data in
      let upd = Set.filter (fun (x : pid_info) ->
                    List.mem ~eq:equal_pid_info x pids) inter in
      info#update ~lost ~found ~changed:upd

    method set_rate (x : bitrate option) =
      info#set_rate x;
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
    | None ->
       let open Requests.Streams.HTTP in
       get_last_pids ~id:stream.id control in
  init
  >|= (function
       | None -> None, []
       | Some x -> Some x.timestamp, x.pids)
  >|= (fun (ts, data) -> new t ts data ())
  |> Ui_templates.Loader.create_widget_loader
