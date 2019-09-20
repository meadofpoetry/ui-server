open Js_of_ocaml
open Js_of_ocaml_tyxml
open Components
open Application_types
open Board_niitv_tsan_types
include Board_niitv_tsan_widgets_tyxml.Service_overview
module D = Make (Tyxml_js.Xml) (Tyxml_js.Svg) (Tyxml_js.Html)

let ( >>= ) = Lwt.bind

type event =
  [ `State of [Topology.state | `No_sync]
  | `Bitrate of Bitrate.ext option
  | `PIDs of (int * PID.t) list ts
  | `Services of (int * Service.t) list ts ]

module Selector = struct
  let icon_button = "." ^ Icon_button.CSS.root
end

module Set = Set.Make (struct
  type t = int * Service.t

  let compare (a : t) (b : t) = compare (fst a) (fst b)
end)

let update_row_bitrate
    (table : 'a Gadt_data_table.t)
    (info : Service.t)
    (bitrate : Bitrate.ext)
    row =
  let bps =
    Util.(sum_bitrates @@ cur_bitrate_for_pids bitrate (Util.service_pids info))
  in
  let pct = Float.(100. *. (of_int bps /. of_int bitrate.total.cur)) in
  let min, max =
    Gadt_data_table.Fmt_d.(
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
    Gadt_data_table.Fmt_d.
      [None; None; None; None; Some (Some bps); Some (Some pct); min; max]
  in
  table#set_row_data_some data row

let update_row_info (table : 'a Gadt_data_table.t) row id (info : Service.t) =
  let data =
    Gadt_data_table.Fmt_d.
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
    val mutable service_info = None

    val mutable data =
      match init with
      | None -> []
      | Some {data; _} -> data

    inherit
      [int] Table_overview.with_details ~create_table_format:D.create_table_format elt () as super

    method! init () : unit =
      service_info <- Some (Service_info.make ~control ());
      super#init ()

    method services = data

    method service_info : Service_info.t = Option.get service_info

    method notify : event -> unit =
      function
      | `State x -> super#set_state x
      | `PIDs _ as x -> (self#service_info)#notify x
      | `Services x -> self#set_services x
      | `Bitrate rate as x ->
          (self#service_info)#notify x;
          self#set_bitrate rate

    method set_hex (hex : bool) =
      let id_fmt = D.id_fmt ~hex in
      let (format : _ D.Fmt.data_format) =
        match table#data_format with
        | _ :: name :: _ :: _ :: tl -> id_fmt :: name :: id_fmt :: id_fmt :: tl
      in
      table#set_data_format ~redraw:true format;
      (self#service_info)#set_hex hex

    method set_bitrate : Bitrate.ext option -> unit =
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
      Set.iter (super#remove_row % fst) @@ Set.diff old cur;
      (* Handle found PIDs *)
      Set.iter self#add_service @@ Set.diff cur old;
      (* Update existing PIDs *)
      Set.iter self#update_service @@ Set.inter cur old;
      self#update_empty_state ()

    method private update_service (id, info) =
      match self#find_row id with
      | None -> ()
      | Some row -> update_row_info table row id info

    method private add_service service =
      let (data : _ D.Fmt.data) = D.data_of_service_info service in
      let row = table#insert_row (-1) data in
      ignore row

    method private find_row (pid : int) =
      let find row =
        let pid' =
          Gadt_data_table.Fmt_d.(
            match table#get_row_data_lazy row with
            | pid :: _ -> pid ())
        in
        pid = pid'
      in
      List.find_opt find table#rows

    method private get_row_title (row : Dom_html.tableRowElement Js.t) =
      match table#get_row_data_lazy row with
      | _ :: name :: _ -> name ()

    method private handle_row_action (row : Dom_html.tableRowElement Js.t) =
      let id =
        match table#get_row_data_lazy row with
        | id :: _ -> id ()
      in
      (self#service_info)#notify
        (`Service (List.find_opt (fun (id', _) -> id' = id) data));
      Element.remove_child_safe super#root table#root;
      Dom.appendChild super#root (self#service_info)#root;
      Js_of_ocaml_lwt.Lwt_js_events.click back_action#root
      >>= fun _ ->
      Js_of_ocaml_lwt.Lwt_js_events.request_animation_frame ()
      >>= fun () ->
      Dom.removeChild super#root (self#service_info)#root;
      Dom.appendChild super#root table#root;
      Lwt.return_unit
  end

let attach ?init elt : t = new t ?init (elt : Dom_html.element Js.t) ()

let make ?classes ?a ?dense ?init ~control () =
  D.create ?classes ?a ?dense ?init ~control () |> Tyxml_js.To_dom.of_div |> attach ?init
