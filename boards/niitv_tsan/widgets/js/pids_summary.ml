(* open Components
 * open Board_niitv_tsan_types
 * open Application_types
 * 
 * type config =
 *   { stream : Stream.t
 *   } [@@deriving yojson]
 * 
 * module Settings = struct
 *   type t = { hex : bool } [@@deriving eq]
 * 
 *   let (default : t) = { hex = false }
 * 
 *   let make_hex_swith state =
 *     let input = new Switch.t ~state () in
 *     new Form_field.t ~input ~align_end:true ~label:"HEX IDs" ()
 * 
 *   class view ?(settings = default) () =
 *     let hex_switch = make_hex_swith settings.hex in
 *     object
 *       val mutable value = settings
 *       inherit Vbox.t ~widgets:[hex_switch] ()
 *       method value : t = value
 *       method apply () : unit =
 *         let hex = hex_switch#input_widget#checked in
 *         value <- { hex }
 *       method reset () : unit =
 *         let { hex } = value in
 *         hex_switch#input_widget#set_checked hex
 *     end
 * 
 *   let make ?settings () = new view ?settings ()
 * end
 * 
 * module Pid_info = struct
 * 
 *   include Pid
 * 
 *   let compare (a : t) (b : t) : int =
 *     Int.compare (fst a) (fst b)
 * end
 * 
 * module Set = Set.Make(Pid_info)
 * 
 * let base_class = "qos-niit-pids-summary"
 * let no_sync_class = Markup.CSS.add_modifier base_class "no-sync"
 * let no_response_class = Markup.CSS.add_modifier base_class "no-response"
 * 
 * (\* TODO improve tooltip. add pid type, bitrate units *\)
 * module Pie = struct
 * end
 * 
 * (\* TODO show only N pids, and allow user to expand if necessary
 *    TODO sort pids by value *\)
 * module Info = struct
 *   let _class = Markup.CSS.add_element base_class "info"
 *   let rate_class = Markup.CSS.add_element _class "rate"
 * 
 *   let make_rate () =
 *     let na = "n/a" in
 *     let to_string = Printf.sprintf "%f Мбит/с" in
 *     let set meta = function
 *       | None -> meta#set_text na
 *       | Some x -> meta#set_text @@ to_string x in
 *     let total, set_total =
 *       let meta = new Typography.Text.t ~text:na () in
 *       let item = new Item_list.Item.t
 *                    ~text:"Общий битрейт: "
 *                    ~value:None
 *                    ~meta
 *                    () in
 *       item, set meta in
 *     let effective, set_effective =
 *       let meta = new Typography.Text.t ~text:na () in
 *       let item = new Item_list.Item.t
 *                    ~text:"Полезный битрейт: "
 *                    ~value:None
 *                    ~meta
 *                    () in
 *       item, set meta in
 *     let list =
 *       new Item_list.t
 *         ~dense:true
 *         ~non_interactive:true
 *         ~items:[ `Item total
 *                ; `Item effective ] ()
 *     in
 *     list#add_class rate_class;
 *     list, set_total, set_effective
 * 
 *   module Pids = struct
 * 
 *     let _class = Markup.CSS.add_element _class "pids"
 *     let box_class = Markup.CSS.add_element _class "box"
 *     let pid_class = Markup.CSS.add_element _class "pid"
 *     let title_class = Markup.CSS.add_element _class "title"
 *     let lost_class = Markup.CSS.add_modifier pid_class "lost"
 * 
 *     let make_title num =
 *       Printf.sprintf "PIDs (%d)" num
 * 
 *     let make_pid ?(hex = false) ((pid, info) : Pid.t) =
 *       object(self)
 *         inherit Widget.t Js_of_ocaml.Dom_html.(createSpan document) () as super
 * 
 *         method! init () : unit  =
 *           super#init ();
 *           self#update info;
 *           self#add_class pid_class;
 *           self#set_hex hex
 * 
 *         method update (info : Pid.info) : unit =
 *           self#add_or_remove_class (not info.present) lost_class
 * 
 *         method set_hex (x : bool) : unit =
 *           let s = match x with
 *             | true -> PID.to_hex_string self#pid
 *             | false -> PID.to_dec_string self#pid in
 *           self#set_text_content s
 * 
 *         method pid : int = pid
 * 
 *       end
 * 
 *     class t ?hex (init : Pid.t list) () =
 *       let text = make_title @@ List.length init in
 *       let title = new Typography.Text.t ~font:Caption ~text () in
 *       let pids_box = new Hbox.t ~widgets:[] () in
 *       object(self)
 * 
 *         val mutable _pids = []
 * 
 *         inherit Vbox.t
 *                   ~widgets:[ title#widget
 *                            ; pids_box#widget] () as super
 * 
 *         method! init () : unit =
 *           super#init ();
 *           _pids <- List.map (make_pid ?hex) init;
 *           List.iter pids_box#append_child _pids;
 *           self#add_class _class;
 *           title#add_class title_class;
 *           pids_box#add_class box_class;
 * 
 *         method! destroy () : unit =
 *           super#destroy ();
 *           title#destroy ();
 *           pids_box#destroy ();
 * 
 *         method update ~(lost : Set.t)
 *                  ~(found : Set.t)
 *                  ~(changed : Set.t) : unit =
 *           Set.iter self#remove_pid lost;
 *           Set.iter self#update_pid changed;
 *           Set.iter self#add_pid found;
 *           title#set_text @@ make_title @@ List.length _pids;
 * 
 *         method set_hex (x : bool) : unit =
 *           List.iter (fun cell -> cell#set_hex x) _pids
 * 
 *         (\* Private methods *\)
 * 
 *         method private update_pid ((pid, info) : Pid.t) : unit =
 *           match List.find_opt (fun cell ->
 *                     cell#pid = pid) _pids with
 *           | None -> ()
 *           | Some cell -> cell#update info
 * 
 *         method private add_pid (x : Pid.t) : unit =
 *           let pid = make_pid x in
 *           _pids <- pid :: _pids;
 *           (\* FIXME sort? *\)
 *           pids_box#append_child pid
 * 
 *         method private remove_pid ((pid, _) : Pid.t) : unit =
 *           match List.find_opt (fun cell ->
 *                     cell#pid = pid) _pids with
 *           | None -> ()
 *           | Some cell ->
 *              pids_box#remove_child cell;
 *              cell#destroy ();
 *              _pids <- List.remove ~eq:Widget.equal cell _pids
 * 
 *       end
 * 
 *   end
 * 
 *   class t ?hex (init : Pid.t list) () =
 *     let rate, set_total, set_effective = make_rate () in
 *     let pids = new Pids.t ?hex init () in
 *     object(self)
 * 
 *       val mutable _pids = []
 * 
 *       inherit Vbox.t
 *                 ~widgets:[ rate#widget
 *                          ; (new Divider.t ())#widget
 *                          ; pids#widget ] () as super
 * 
 *       method! init () : unit =
 *         super#init ();
 *         self#add_class _class
 * 
 *       method! destroy () : unit =
 *         super#destroy ();
 *         pids#destroy ();
 *         rate#destroy ()
 * 
 *       method set_rate (rate : Bitrate.t option) =
 *         match rate with
 *         | None -> set_total None; set_effective None
 *         | Some x ->
 *            let null = List.Assoc.get ~eq:(=) 0x1FFF x.pids
 *                       |> Option.get_or ~default:0 in
 *            let e = x.total - null in
 *            let e = Float.(of_int e / 1_000_000.) in
 *            let v = Float.(of_int x.total / 1_000_000.) in
 *            set_total (Some v);
 *            set_effective (Some e);
 * 
 *       method update ~(lost : Set.t)
 *                ~(found : Set.t)
 *                ~(changed : Set.t) : unit =
 *         pids#update ~lost ~found ~changed;
 * 
 *       method set_hex (x : bool) : unit =
 *         pids#set_hex x
 * 
 *   end
 * 
 * end
 * 
 * class t ?(settings : Settings.t option)
 *         (init : Pid.t list Time.timestamped option) () =
 *   let init, timestamp = match init with
 *     | None -> [], None
 *     | Some { data; timestamp } -> data, Some timestamp in
 *   let s_time, set_time =
 *     React.S.create ~eq:(Equal.option Time.equal) timestamp in
 *   let pie = new Pie.t () in
 *   let info = new Info.t init () in
 *   object(self)
 * 
 *     val mutable _data : Set.t = Set.of_list init
 * 
 *     inherit Widget.t Js_of_ocaml.Dom_html.(createDiv document) () as super
 * 
 *     method! init () : unit =
 *       super#init ();
 *       Option.iter self#set_settings settings;
 *       self#add_class base_class;
 *       self#append_child pie;
 *       self#append_child info
 * 
 *     method! destroy () : unit =
 *       super#destroy ();
 *       pie#destroy ();
 *       info#destroy ();
 *       React.S.stop ~strong:true s_time;
 * 
 *     method s_timestamp : Time.t option React.signal =
 *       s_time
 * 
 *     method update ({ timestamp; data } : Pid.t list Time.timestamped) =
 *       (\* Update timestamp *\)
 *       set_time @@ Some timestamp;
 *       (\* Manage found, lost and updated items *\)
 *       let prev = _data in
 *       _data <- Set.of_list data;
 *       let lost = Set.diff prev _data in
 *       let found = Set.diff _data prev in
 *       let inter = Set.inter prev _data in
 *       let upd =
 *         Set.filter (fun ((_, info) : Pid.t) ->
 *             List.mem ~eq:Pid.equal_info info @@ List.map snd data) inter in
 *       info#update ~lost ~found ~changed:upd
 * 
 *     (\** Updates widget state *\)
 *     method set_state = function
 *       | Fine ->
 *          self#remove_class no_response_class;
 *          self#remove_class no_sync_class
 *       | No_sync ->
 *          self#remove_class no_response_class;
 *          self#add_class no_sync_class
 *       | No_response ->
 *          self#remove_class no_sync_class;
 *          self#add_class no_response_class
 * 
 *     method set_hex (x : bool) : unit =
 *       pie#set_hex x;
 *       info#set_hex x
 * 
 *     method set_rate (x : Bitrate.t option) =
 *       info#set_rate x;
 *       pie#set_rate x
 * 
 *     method set_settings (x : Settings.t) : unit =
 *       self#set_hex x.hex
 * 
 *   end
 * 
 * let make ?(settings : Settings.t option)
 *       (init : Pid.t list Time.timestamped option) =
 *   new t ?settings init () *)
