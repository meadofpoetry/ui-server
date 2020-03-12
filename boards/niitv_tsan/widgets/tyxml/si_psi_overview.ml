open Components_tyxml
open Application_types
open Board_niitv_tsan_types

module CSS = struct
  include Table_overview.CSS

  let si_psi = BEM.add_modifier root "si-psi"
end

module Make
    (Xml : Intf.Xml)
    (Svg : Svg_sigs.T with module Xml := Xml)
    (Html : Html_sigs.T with module Xml := Xml and module Svg := Svg) =
struct
  open Xml.W
  open Html
  include Table_overview.Make (Xml) (Svg) (Html)
  module Fmt = Data_table.Make_fmt (Xml) (Svg) (Html)

  let dec_pid_fmt = Fmt.Int

  let hex_pid_fmt =
    Fmt.Custom
      {
        to_string = Util.pid_to_hex_string;
        of_string = int_of_string;
        compare;
        is_numeric = true;
      }

  let pid_fmt ~hex = if hex then hex_pid_fmt else dec_pid_fmt

  let pct_fmt =
    Fmt.Custom
      {
        to_string = (fun x -> Printf.sprintf "%.2f" x);
        of_string = float_of_string;
        compare;
        is_numeric = true;
      }

  (** Returns HTML element to insert into 'Extra' table column *)
  let create_table_id_ext ?(hex = false) (id : SI_PSI_table.id) =
    let to_id_string =
      match hex with
      | true -> Printf.sprintf "0x%02X"
      | false -> Printf.sprintf "%d"
    in
    let specific =
      match MPEG_TS.SI_PSI.of_table_id id.table_id with
      | `PAT -> [ ("tsid", id.table_id_ext) ]
      | `PMT -> [ ("program", id.table_id_ext) ]
      | `NIT _ -> [ ("network_id", id.table_id_ext) ]
      | `SDT _ -> [ ("tsid", id.table_id_ext); ("onid", id.id_ext_1) ]
      | `BAT -> [ ("bid", id.table_id_ext) ]
      | `EIT _ ->
          [
            ("onid", id.id_ext_1);
            ("tsid", id.id_ext_2);
            ("sid", id.table_id_ext);
          ]
      | _ -> []
    in
    let data = Yojson.Safe.to_string (SI_PSI_table.id_to_yojson id) in
    let length = List.length specific in
    span
      ~a:[ a_user_data "id" (return data) ]
      Xml.Wutils.(
        const
          (List.mapi
             (fun i (s, v) ->
               let v = to_id_string v in
               let v = if i = pred length then v else v ^ ", " in
               span
                 (const
                    [
                      span
                        ~a:[ a_class (return [ Typography.CSS.subtitle2 ]) ]
                        (const [ txt (return (s ^ ": ")) ]);
                      txt (return v);
                    ]))
             specific))

  let id_ext_fmt ?(get_attribute = fun _ _ -> None) ~hex () =
    Fmt.Custom_elt
      {
        is_numeric = false;
        compare = SI_PSI_table.compare_id;
        to_elt = (fun x -> create_table_id_ext ~hex x);
        of_elt =
          (fun elt ->
            match get_attribute elt "data-id" with
            | None -> failwith "no `data-id` attribute found"
            | Some x -> (
                let res =
                  SI_PSI_table.id_of_yojson @@ Yojson.Safe.from_string x
                in
                match res with Error e -> failwith e | Ok x -> x ));
      }

  let br_fmt =
    Fmt.Custom
      {
        to_string =
          (fun x -> Printf.sprintf "%f" (float_of_int x /. 1_000_000.));
        of_string = (fun x -> int_of_float (float_of_string x *. 1_000_000.));
        compare;
        is_numeric = true;
      }

  let create_table_format ?get_attribute ?(hex = return false) () : _ Fmt.format
      =
    let pid_fmt = fmap (fun hex -> pid_fmt ~hex) hex in
    let pct_fmt = return (Fmt.Option (pct_fmt, "-")) in
    let br_fmt = return (Fmt.Option (br_fmt, "-")) in
    (* FIXME here react fails with `compare: functional value` *)
    (* let id_ext_fmt = fmap (fun hex -> id_ext_fmt ?get_attribute ~hex ()) hex in *)
    Fmt.
      [
        make_column ~sortable:true ~title:(return "ID") pid_fmt;
        make_column ~sortable:true ~title:(return "PID") pid_fmt;
        make_column ~sortable:true ~title:(return "Имя") (return String)
        (* ; make_column ~title:(return "Доп. инфо") id_ext_fmt *);
        make_column ~sortable:true ~title:(return "Версия") (return Int);
        make_column ~sortable:true ~title:(return "Сервис")
          (return (Option (String, "")));
        make_column ~title:(return "Кол-во секций") (return Int);
        make_column ~title:(return "LSN") (return Int);
        make_column ~title:(return "Битрейт, Мбит/с") br_fmt;
        make_column ~title:(return "%") pct_fmt;
        make_column ~title:(return "Min, Мбит/с") br_fmt;
        make_column ~title:(return "Max, Мбит/с") br_fmt;
      ]

  let data_of_si_psi_info ?(bitrate = return None)
      ((id, info) : SI_PSI_table.id * SI_PSI_table.t) : _ Fmt.data =
    Fmt.
      [
        return id.table_id;
        return info.pid;
        return MPEG_TS.SI_PSI.(name (of_table_id id.table_id)) (* ; return id *);
        return info.version;
        return info.service_name;
        return (List.length info.sections);
        return info.last_section;
        fmap
          (function None -> None | Some (_, { Bitrate.cur; _ }) -> Some cur)
          bitrate;
        fmap
          (function
            | None -> None
            | Some ({ Bitrate.cur = tot; _ }, { Bitrate.cur; _ }) ->
                Some (100. *. float_of_int cur /. float_of_int tot))
          bitrate;
        fmap
          (function None -> None | Some (_, { Bitrate.cur; _ }) -> Some cur)
          bitrate;
        fmap
          (function None -> None | Some (_, { Bitrate.cur; _ }) -> Some cur)
          bitrate;
      ]

  let row_of_si_psi_info ?bitrate ~format (id, (info : SI_PSI_table.t)) =
    let data = data_of_si_psi_info ?bitrate (id, info) in
    let cells = Data_table_markup.data_table_cells_of_fmt format data in
    let value = Yojson.Safe.to_string (SI_PSI_table.to_yojson info) in
    Data_table_markup.data_table_row
      ~a:
        [
          Html.a_user_data "value" (return value);
          Html.a_user_data "id"
            (return (Yojson.Safe.to_string @@ SI_PSI_table.id_to_yojson id));
        ]
      ~children:cells ()

  let create ?a ?dense ?get_attribute ?(hex = return false)
      ?(bitrate = return None) ?(init = nil ()) ~control =
    let format = create_table_format ?get_attribute ~hex () in
    let rows =
      Xml.W.map
        (fun ((id, _) as x) ->
          let bitrate =
            fmap
              (function
                | None -> None
                | Some (rate : Bitrate.ext) -> (
                    match List.assoc_opt id rate.tables with
                    | None -> None
                    | Some x -> Some (rate.total, x) ))
              bitrate
          in
          row_of_si_psi_info ~bitrate ~format x)
        init
    in
    create
      ~classes:(return [ CSS.si_psi ])
      ?a ?dense ~hex
      ~title:(return "Список таблиц SI/PSI")
      ~format ~rows ~control
end

module F = Make (Impl.Xml) (Impl.Svg) (Impl.Html)
