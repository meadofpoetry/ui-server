open Js_of_ocaml
open Js_of_ocaml_tyxml
open Application_types
open Components
open Board_niitv_tsan_types
include Board_niitv_tsan_widgets_tyxml.Si_psi_overview
module D = Make (Tyxml_js.Xml) (Tyxml_js.Svg) (Tyxml_js.Html)

type event =
  [ `State of [Topology.state | `No_sync]
  | `Bitrate of Bitrate.ext option
  | `Tables of (SI_PSI_table.id * SI_PSI_table.t) list ts ]

module Set = Set.Make (struct
  type t = SI_PSI_table.id * SI_PSI_table.t

  let compare (a : t) (b : t) : int = SI_PSI_table.compare_id (fst a) (fst b)
end)

let get_attribute elt attr =
  Element.get_attribute (Tyxml_js.To_dom.of_element @@ Tyxml_js.Html.tot elt) attr

let update_row_bitrate
    (table : 'a Gadt_data_table.t)
    (total : Bitrate.value)
    (br : Bitrate.value)
    row =
  let pct = 100. *. float_of_int br.cur /. float_of_int total.cur in
  let mbps = float_of_int br.cur /. 1_000_000. in
  let data =
    Gadt_data_table.Fmt_d.
      [ None
      ; None
      ; None
      ; None
      ; None
      ; None
      ; None
      ; None
      ; Some (Some mbps)
      ; Some (Some pct)
      ; Some (Some (float_of_int br.min /. 1_000_000.))
      ; Some (Some (float_of_int br.max /. 1_000_000.)) ]
  in
  table#set_row_data_some data row

let update_row_info
    (table : 'a Gadt_data_table.t)
    row
    (id : SI_PSI_table.id)
    (info : SI_PSI_table.t) =
  let data =
    Gadt_data_table.Fmt_d.
      [ Some id.table_id
      ; Some info.pid
      ; Some ""
      ; Some id
      ; Some info.version
      ; Some info.service_name
      ; Some (List.length info.sections)
      ; Some info.last_section
      ; None
      ; None
      ; None
      ; None ]
  in
  table#set_row_data_some data row

class t ?(init : (SI_PSI_table.id * SI_PSI_table.t) list ts option) elt () =
  object (self)
    val mutable data : Set.t =
      Set.of_list
        (match init with
        | None -> []
        | Some {data; _} -> data)

    inherit
      [SI_PSI_table.id] Table_overview.with_details
        ~create_table_format:(D.create_table_format ~get_attribute)
        elt () as super

    method notify : event -> unit =
      function
      | `State x -> super#set_state x
      | `Tables x -> self#set_tables x
      | `Bitrate x -> self#set_bitrate x

    method set_hex (hex : bool) : unit =
      let id_ext_fmt = D.id_ext_fmt ~get_attribute ~hex () in
      let pid_fmt = D.pid_fmt ~hex in
      let (format : _ D.Fmt.data_format) =
        match table#data_format with
        | _ :: _ :: name :: _ :: tl -> pid_fmt :: pid_fmt :: name :: id_ext_fmt :: tl
      in
      table#set_data_format ~redraw:false format;
      List.iter
        (fun row ->
          let cells = row##.cells in
          let set_cell_value fmt n =
            Js.Opt.iter
              (cells##item n)
              (fun cell ->
                Gadt_data_table.(set_cell_value fmt (get_cell_value fmt cell) cell))
          in
          set_cell_value pid_fmt 0;
          set_cell_value pid_fmt 1;
          set_cell_value id_ext_fmt 3)
        table#rows

    method set_bitrate : Bitrate.ext option -> unit =
      function
      | None -> () (* FIXME do smth *)
      | Some ({total; tables; _} : Bitrate.ext) ->
          List.iter
            (fun (id, (br : Bitrate.value)) ->
              let row = self#find_row id in
              Option.iter (update_row_bitrate table total br) row)
            tables
    (** Updates bitrate values *)

    method set_tables (pids : (SI_PSI_table.id * SI_PSI_table.t) list ts) =
      (* Manage found, lost and updated items *)
      let old = data in
      let cur = Set.of_list pids.data in
      data <- cur;
      (* Handle lost tables *)
      Set.iter (self#remove_row % fst) @@ Set.diff old cur;
      (* Handle found tables *)
      Set.iter self#add_table @@ Set.diff cur old;
      (* Update existing tables *)
      Set.iter self#update_table @@ Set.inter cur old;
      super#update_empty_state ()

    method private update_table (id, info) =
      match self#find_row id with
      | None -> ()
      | Some row -> update_row_info table row id info

    method private add_table x =
      let (data : _ D.Fmt.data) = D.data_of_si_psi_info x in
      let _row = table#insert_row (-1) data in
      ()

    method private find_row (id : SI_PSI_table.id) =
      let find row =
        let id' =
          Gadt_data_table.Fmt_d.(
            match table#get_row_data_lazy row with
            | id :: _ -> id ())
        in
        id.table_id = id'
      in
      List.find_opt find table#rows

    method private get_row_title _ = ""

    method private handle_row_action _ = Lwt.return_unit
  end

let attach elt : t = new t (elt :> Dom_html.element Js.t) ()
