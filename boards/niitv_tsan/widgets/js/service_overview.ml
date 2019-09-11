open Js_of_ocaml
open Js_of_ocaml_tyxml
open Components
open Application_types
open Board_niitv_tsan_types
include Board_niitv_tsan_widgets_tyxml.Service_overview
module Markup_js = Make (Tyxml_js.Xml) (Tyxml_js.Svg) (Tyxml_js.Html)

type event =
  [ `State of [Topology.state | `No_sync]
  | `Bitrate of Bitrate.t option
  | `Services of (int * Service_info.t) list ts ]

module Selector = struct
  let table = "." ^ CSS.table

  let placeholder = "." ^ Components_lab.Placeholder.CSS.root
end

module Set = Set.Make (struct
  type t = int * Service_info.t

  let compare (a : t) (b : t) = compare (fst a) (fst b)
end)

let get_service_bitrate (br : Bitrate.t) (info : Service_info.t) =
  let elts =
    List.fold_left
      (fun acc pid ->
        match List.assoc_opt pid br.pids with
        | None -> acc
        | Some b -> (pid, b) :: acc)
      []
      info.elements
  in
  let pmt =
    match info.has_pmt with
    | false -> None
    | true ->
        Some
          (info.pmt_pid, Option.value ~default:0 @@ List.assoc_opt info.pmt_pid br.pids)
  in
  match pmt with
  | None -> elts
  | Some x -> x :: elts

let sum_bitrate rate = List.fold_left (fun acc x -> acc + snd x) 0 rate

let acc_bitrate total rate =
  let bps = sum_bitrate rate in
  let pct = Float.(100. *. (of_int bps /. of_int total)) in
  let mbps = Float.(of_int bps /. 1_000_000.) in
  bps, mbps, pct

let update_row_bitrate
    (table : 'a Gadt_data_table.t)
    (info : Service_info.t)
    (bitrate : Bitrate.t)
    row =
  let _, br, pct = acc_bitrate bitrate.total (get_service_bitrate bitrate info) in
  let min, max =
    Gadt_data_table.Fmt_js.(
      match table#get_row_data_lazy row with
      | [_; _; _; _; _; _; min; max] -> min, max)
  in
  let min =
    match min () with
    | None -> Some (Some br)
    | Some v -> if br < v then Some (Some br) else None
  in
  let max =
    match max () with
    | None -> Some (Some br)
    | Some v -> if br > v then Some (Some br) else None
  in
  let data =
    Gadt_data_table.Fmt_js.
      [None; None; None; None; Some (Some br); Some (Some pct); min; max]
  in
  table#set_row_data_some data row

let update_row_info (table : 'a Gadt_data_table.t) row id (info : Service_info.t) =
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

class t ?(init : (int * Service_info.t) list ts option) elt () =
  object (self)
    val placeholder =
      match Element.query_selector elt Selector.placeholder with
      | Some x -> x
      | None -> Tyxml_js.To_dom.of_div @@ Markup_js.create_empty_placeholder ()

    val table : _ Gadt_data_table.t =
      Gadt_data_table.attach ~fmt:Markup_js.table_fmt
      @@ Element.query_selector_exn elt Selector.table

    val mutable data : Set.t =
      Set.of_list
        (match init with
        | None -> []
        | Some {data; _} -> data)

    inherit Widget.t elt () as super

    method! init () : unit =
      self#update_empty_state ();
      super#init ()

    method! destroy () : unit =
      table#destroy ();
      super#destroy ()

    method pids = Set.to_seq data

    method notify : event -> unit =
      function
      | `State x -> self#set_state x
      | `Services x -> self#set_services x
      | `Bitrate x -> self#set_bitrate x

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
              let info = List.assoc_opt id @@ Set.elements data in
              match info with
              | None ->
                  print_endline @@ Printf.sprintf "id %d, info not found" id;
                  ()
              | Some info -> update_row_bitrate table info bitrate row)
            table#rows
    (** Updates bitrate values *)

    method set_services (services : (int * Service_info.t) list ts) =
      (* Manage found, lost and updated items *)
      let old = data in
      let cur = Set.of_list services.data in
      data <- cur;
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
  end

let attach ?init elt : t = new t ?init (elt : Dom_html.element Js.t) ()

let make ?classes ?attrs ?dense ?init () =
  Markup_js.create ?classes ?attrs ?dense ?init ()
  |> Tyxml_js.To_dom.of_div
  |> attach ?init
