open Containers
open Components
open Common
open Lwt_result.Infix
open Api_js.Api_types
open Widget_common
open Board_types

type config =
  { stream : Stream.t
  } [@@deriving yojson]

module Settings = struct

  type t = { hex : bool } [@@deriving eq]

  let (default : t) = { hex = false (* FIXME *) }

  class view ?(settings = default) () =
    let hex_switch =
      new Switch.t
        ~state:settings.hex
        () in
    let hex_form =
      new Form_field.t
        ~input:hex_switch
        ~align_end:true
        ~label:"HEX IDs"
        () in
    let s, set = React.S.create ~eq:equal settings in
    object(self)

      inherit Vbox.t ~widgets:[hex_form] ()

      method apply () : unit =
        let hex = hex_switch#checked in
        set { hex }

      method reset () : unit =
        let { hex } = React.S.value self#s in
        hex_switch#set_checked hex

      method s : t React.signal = s

    end

  let make ?settings () = new view ?settings ()

end

module Pid_info = struct

  include Pid

  let compare (a : t) (b : t) : int =
    Int.compare (fst a) (fst b)
end

module Set = Set.Make(Pid_info)

let base_class = "qos-niit-pids-summary"
let no_sync_class = Markup.CSS.add_modifier base_class "no-sync"
let no_response_class = Markup.CSS.add_modifier base_class "no-response"

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

  let make_pie_options () : Chartjs.Options.t =
    let open Chartjs in
    let hover = Options.Hover.make ~animation_duration:0 () in
    let callbacks =
      Options.Tooltips.Callbacks.make
        (* ~label:(fun ~item ~data ->
         *   let ds_index = item.dataset_index in
         *   let data = Data.t_of_js data in
         *   let dataset = (Data.datasets data).%[ds_index] in
         *   let label = Array.String.get_exn (Data.labels data) item.index in
         *   let value = Array.Float.get_exn (Pie.Dataset.data dataset) item.index in
         *   Printf.sprintf "PID %s: %.3g Мбит/с" label value) *)
        () in
    let tooltips =
      Options.Tooltips.make
        ~callbacks
        () in
    let legend =
      Options.Legend.make
        ~position:`Left
        ~display:false
        () in
    Options.make
      ~responsive:true
      ~maintain_aspect_ratio:true
      ~aspect_ratio:1.0
      ~tooltips
      ~legend
      ~hover
      ()

  let make_pie_dataset () : Chartjs.Pie.Dataset.Float.t =
    let open Chartjs.Pie in
    let background_color =
      List.map Fun.(Color.(string_of_t % of_material % fst)) colors in
    Dataset.Float.make
      ~background_color
      ~border_width:[0]
      ~data:[]
      ()

  let make_pie () =
    let open Chartjs in
    let dataset = make_pie_dataset () in
    let options = make_pie_options () in
    let data = Data.make ~datasets:[] ~labels:[] () in
    let node = `Canvas Dom_html.(createCanvas document) in
    let chart = make ~options ~data `Pie node in
    chart, dataset

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

      inherit Widget.t Dom_html.(createDiv document) () as super

      method init () : unit =
        super#init ();
        box#add_class box_class;
        box#append_child @@ Widget.create @@ Chartjs.canvas pie;
        title#add_class title_class;
        self#set_rate None;
        self#add_class _class;
        self#append_child title;
        self#append_child box;

      method destroy () : unit =
        super#destroy ();
        title#destroy ();
        box#destroy ();
        Chartjs.destroy pie

      method set_hex (x : bool) : unit =
        _hex <- x;
        match _rate with
        | None -> ()
        | Some (pids, oth) ->
           let open Chartjs in
           let data = data pie in
           Data.set_labels data (self#make_labels pids oth);
           update pie None

      method set_rate : Bitrate.t option -> unit = function
        | None ->
           let open Chartjs in
           let data = data pie in
           Data.set_datasets data [];
           _rate <- None;
           update pie None
        | Some { total; pids; _ } ->
           let open Chartjs in
           let data = data pie in
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
           if Option.is_none _rate
           then Data.set_datasets data [dataset];
           _rate <- Some (pids, oth);
           let data' =
             let pids = List.map snd pids in
             match oth with
             | [] -> pids
             | l  -> pids @ [List.fold_left (+.) 0. l] in
           ignore data';
           Data.set_labels data (self#make_labels pids oth);
           Pie.Dataset.Float.set_data dataset data';
           update pie None

      (* Private methods *)

      method private make_labels pids oth =
        let to_string =
          if _hex then PID.to_hex_string
          else PID.to_dec_string in
        let pids = List.map (fun x -> to_string @@ fst x) pids in
        match oth with
        | [] -> pids
        | _  -> pids @ ["Другие"]

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

    let make_pid ?(hex = false) ((pid, info) : Pid.t) =
      object(self)
        inherit Widget.t Dom_html.(createSpan document) () as super

        method init () : unit  =
          super#init ();
          self#update info;
          self#add_class pid_class;
          self#set_hex hex

        method update (info : Pid.info) : unit =
          self#add_or_remove_class (not info.present) lost_class

        method set_hex (x : bool) : unit =
          let s = match x with
            | true -> PID.to_hex_string self#pid
            | false -> PID.to_dec_string self#pid in
          self#set_text_content s

        method pid : int = pid

      end

    class t ?hex (init : Pid.t list) () =
      let text = make_title @@ List.length init in
      let title = new Typography.Text.t ~font:Caption ~text () in
      let pids_box = new Hbox.t ~widgets:[] () in
      object(self)

        val mutable _pids = []

        inherit Vbox.t
                  ~widgets:[ title#widget
                           ; pids_box#widget] () as super

        method init () : unit =
          super#init ();
          _pids <- List.map (make_pid ?hex) init;
          List.iter pids_box#append_child _pids;
          self#add_class _class;
          title#add_class title_class;
          pids_box#add_class box_class;

        method destroy () : unit =
          super#destroy ();
          title#destroy ();
          pids_box#destroy ();

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

        method private update_pid ((pid, info) : Pid.t) : unit =
          match List.find_opt (fun cell ->
                    cell#pid = pid) _pids with
          | None -> ()
          | Some cell -> cell#update info

        method private add_pid (x : Pid.t) : unit =
          let pid = make_pid x in
          _pids <- pid :: _pids;
          (* FIXME sort? *)
          pids_box#append_child pid

        method private remove_pid ((pid, info) : Pid.t) : unit =
          match List.find_opt (fun cell ->
                    cell#pid = pid) _pids with
          | None -> ()
          | Some cell ->
             pids_box#remove_child cell;
             cell#destroy ();
             _pids <- List.remove ~eq:Widget.equal ~x:cell _pids

      end

  end

  class t ?hex (init : Pid.t list) () =
    let rate, set_total, set_effective = make_rate () in
    let pids = new Pids.t ?hex init () in
    object(self)

      val mutable _pids = []

      inherit Vbox.t
                ~widgets:[ rate#widget
                         ; (new Divider.t ())#widget
                         ; pids#widget ] () as super

      method init () : unit =
        super#init ();
        self#add_class _class

      method destroy () : unit =
        super#destroy ();
        pids#destroy ();
        rate#destroy ()

      method set_rate (rate : Bitrate.t option) =
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

  end

end

class t ?(settings : Settings.t option)
        (init : Pid.t list timestamped option) () =
  let init, timestamp = match init with
    | None -> [], None
    | Some { data; timestamp } -> data, Some timestamp in
  let s_time, set_time =
    React.S.create ~eq:(Equal.option Time.equal) timestamp in
  let pie = new Pie.t () in
  let info = new Info.t init () in
  object(self)

    val mutable _data : Set.t = Set.of_list init

    inherit Widget.t Dom_html.(createDiv document) () as super

    method init () : unit =
      super#init ();
      Option.iter self#set_settings settings;
      self#add_class base_class;
      self#append_child pie;
      self#append_child info

    method destroy () : unit =
      super#destroy ();
      pie#destroy ();
      info#destroy ();
      React.S.stop ~strong:true s_time;

    method s_timestamp : Time.t option React.signal =
      s_time

    method update ({ timestamp; data } : Pid.t list timestamped) =
      (* Update timestamp *)
      set_time @@ Some timestamp;
      (* Manage found, lost and updated items *)
      let prev = _data in
      _data <- Set.of_list data;
      let lost = Set.diff prev _data in
      let found = Set.diff _data prev in
      let inter = Set.inter prev _data in
      let upd =
        Set.filter (fun ((_, info) : Pid.t) ->
            List.mem ~eq:Pid.equal_info info @@ List.map snd data) inter in
      info#update ~lost ~found ~changed:upd

    (** Updates widget state *)
    method set_state = function
      | Fine ->
         self#remove_class no_response_class;
         self#remove_class no_sync_class
      | No_sync ->
         self#remove_class no_response_class;
         self#add_class no_sync_class
      | No_response ->
         self#remove_class no_sync_class;
         self#add_class no_response_class

    method set_hex (x : bool) : unit =
      pie#set_hex x;
      info#set_hex x

    method set_rate (x : Bitrate.t option) =
      info#set_rate x;
      pie#set_rate x

    method set_settings (x : Settings.t) : unit =
      self#set_hex x.hex

  end

let make ?(settings : Settings.t option)
      (init : Pid.t list timestamped option) =
  new t ?settings init ()

let make_dashboard_item ?settings init : 'a Dashboard.Item.item =
  let w = make ?settings init in
  let settings = Settings.make ?settings () in
  let s = settings#s in
  let (settings : Dashboard.Item.settings) =
    { widget = settings#widget
    ; ready = React.S.const true
    ; set = (fun () -> Lwt_result.return @@ w#set_settings @@ React.S.value s)
    } in
  let tz_offset_s = Ptime_clock.current_tz_offset_s () in
  let timestamp =
    Dashboard.Item.make_timestamp
      ~time:w#s_timestamp
      ~to_string:(Time.to_human_string ?tz_offset_s)
      () in
  Dashboard.Item.make_item
    ~name:"Сводка"
    ~subtitle:(Timestamp timestamp)
    ~settings
    w
