open Js_of_ocaml
open Js_of_ocaml_tyxml
open Components
open Application_types
open Board_niitv_tsan_types
include Board_niitv_tsan_widgets_tyxml.Service_overview
module Markup_js = Make (Tyxml_js.Xml) (Tyxml_js.Svg) (Tyxml_js.Html)

let ( >>= ) = Lwt.bind

type event =
  [ `State of [Topology.state | `No_sync]
  | `Bitrate of Bitrate.t option
  | `PIDs of (int * PID.t) list ts
  | `Services of (int * Service.t) list ts ]

module Selector = struct
  let row = "." ^ Data_table.CSS.row

  let icon_button = "." ^ Icon_button.CSS.root

  let table = "." ^ CSS.table

  let placeholder = "." ^ Components_lab.Placeholder.CSS.root
end

module Set = Set.Make (struct
  type t = int * Service.t

  let compare (a : t) (b : t) = compare (fst a) (fst b)
end)

let update_row_bitrate
    (table : 'a Gadt_data_table.t)
    (info : Service.t)
    (bitrate : Bitrate.t)
    row =
  let bps = Util.total_bitrate_for_pids bitrate (Util.service_pids info) in
  let pct = Float.(100. *. (of_int bps /. of_int bitrate.total)) in
  let min, max =
    Gadt_data_table.Fmt_js.(
      match table#get_row_data_lazy row with
      | [_; _; _; _; _; _; min; max] -> min, max)
  in
  let min =
    match min () with
    | None -> Some (Some bps)
    | Some v -> if bps < v then Some (Some bps) else None
  in
  let max =
    match max () with
    | None -> Some (Some bps)
    | Some v -> if bps > v then Some (Some bps) else None
  in
  let data =
    Gadt_data_table.Fmt_js.
      [None; None; None; None; Some (Some bps); Some (Some pct); min; max]
  in
  table#set_row_data_some data row

let update_row_info (table : 'a Gadt_data_table.t) row id (info : Service.t) =
  let data =
    Gadt_data_table.Fmt_js.
      [ Some id
      ; Some info.name
      ; Some info.pmt_pid
      ; Some info.pcr_pid
      ; None
      ; None
      ; None
      ; None ]
  in
  table#set_row_data_some data row

class t ?(init : (int * Service.t) list ts option) elt () =
  object (self)
    val placeholder =
      match Element.query_selector elt Selector.placeholder with
      | Some x -> x
      | None -> Tyxml_js.To_dom.of_div @@ Markup_js.create_empty_placeholder ()

    val service_info = Service_info.make ()

    val table : _ Gadt_data_table.t =
      Gadt_data_table.attach ~fmt:Markup_js.table_fmt
      @@ Element.query_selector_exn elt Selector.table

    val mutable data =
      match init with
      | None -> []
      | Some {data; _} -> data

    val mutable listeners = []

    inherit Widget.t elt () as super

    method! init () : unit =
      self#update_empty_state ();
      super#init ()

    method! initial_sync_with_dom () : unit =
      listeners <-
        Js_of_ocaml_lwt.Lwt_js_events.(
          [clicks table#tbody self#handle_table_body_click] @ listeners);
      super#initial_sync_with_dom ()

    method! destroy () : unit =
      table#destroy ();
      service_info#destroy ();
      super#destroy ()

    method services = data

    method notify : event -> unit =
      function
      | `State x -> self#set_state x
      | `PIDs _ as x -> service_info#notify x
      | `Services x -> self#set_services x
      | `Bitrate rate as x ->
          service_info#notify x;
          self#set_bitrate rate

    method set_state state =
      let no_sync, no_response =
        match state with
        | `Fine -> false, false
        | `No_sync -> true, false
        | `Detect | `Init | `No_response -> false, true
      in
      Element.toggle_class_unit ~force:no_sync super#root CSS.no_sync;
      Element.toggle_class_unit ~force:no_response super#root CSS.no_response
    (** Updates widget state *)

    method set_bitrate : Bitrate.t option -> unit =
      function
      | None -> () (* FIXME do smth *)
      | Some bitrate ->
          List.iter
            (fun row ->
              let id =
                match table#get_row_data_lazy row with
                | f :: _ -> f ()
              in
              let info = List.assoc_opt id data in
              match info with
              | None -> ()
              | Some info -> update_row_bitrate table info bitrate row)
            table#rows
    (** Updates bitrate values *)

    method set_services (services : (int * Service.t) list ts) =
      (* Manage found, lost and updated items *)
      let old = Set.of_list data in
      let cur = Set.of_list services.data in
      data <- services.data;
      (* Handle lost PIDs *)
      Set.iter self#remove_service @@ Set.diff old cur;
      (* Handle found PIDs *)
      Set.iter self#add_service @@ Set.diff cur old;
      (* Update existing PIDs *)
      Set.iter self#update_service @@ Set.inter cur old;
      self#update_empty_state ()

    method private update_service (id, info) =
      match self#find_row id with
      | None -> ()
      | Some row -> update_row_info table row id info

    method private remove_service (id, _) =
      match self#find_row id with
      | None -> ()
      | Some row -> table#table##deleteRow row##.rowIndex

    method private add_service service =
      let (data : _ Markup_js.Fmt.data) = Markup_js.data_of_service_info service in
      let row = table#insert_row (-1) data in
      ignore row

    method private find_row (pid : int) =
      let find row =
        let pid' =
          Gadt_data_table.Fmt_js.(
            match table#get_row_data_lazy row with
            | pid :: _ -> pid ())
        in
        pid = pid'
      in
      List.find_opt find table#rows

    method private update_empty_state () =
      if table#rows_collection##.length = 0
      then Dom.appendChild super#root placeholder
      else Element.remove placeholder

    method private show_service_info (row : Dom_html.tableRowElement Js.t) =
      let id, service_name =
        match table#get_row_data_lazy row with
        | id :: name :: _ -> id (), name ()
      in
      service_info#notify (`Service (List.find_opt (fun (id', _) -> id' = id) data));
      let info_header =
        Tyxml_js.To_dom.of_div @@ Markup_js.create_info_header ~service_name ()
      in
      let back =
        Icon_button.attach @@ Element.query_selector_exn info_header Selector.icon_button
      in
      Element.remove_child_safe super#root table#root;
      Dom.appendChild super#root info_header;
      Dom.appendChild super#root service_info#root;
      Js_of_ocaml_lwt.Lwt_js_events.click back#root
      >>= fun _ ->
      Js_of_ocaml_lwt.Lwt_js_events.request_animation_frame ()
      >>= fun () ->
      Dom.removeChild super#root info_header;
      Dom.removeChild super#root service_info#root;
      Dom.appendChild super#root table#root;
      back#destroy ();
      Lwt.return_unit

    method private handle_table_body_click e _ : unit Lwt.t =
      let target = Dom.eventTarget e in
      let row =
        Js.Opt.bind (Element.closest target Selector.row) (fun row ->
            Dom_html.CoerceTo.tr row)
      in
      Js.Opt.case row Lwt.return self#show_service_info
  end

let attach ?init elt : t = new t ?init (elt : Dom_html.element Js.t) ()

let make ?classes ?attrs ?dense ?init () =
  Markup_js.create ?classes ?attrs ?dense ?init ()
  |> Tyxml_js.To_dom.of_div
  |> attach ?init
