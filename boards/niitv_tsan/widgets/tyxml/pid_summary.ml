open Components_tyxml
open Board_niitv_tsan_types

module CSS = struct
  let root = Util.CSS.root ^ "-pid-summary"

  let title = BEM.add_element root "title"

  let pids = BEM.add_element root "pids"

  let pid = BEM.add_element root "pid"
end

module Make
    (Xml : Xml_sigs.NoWrap)
    (Svg : Svg_sigs.NoWrap with module Xml := Xml)
    (Html : Html_sigs.NoWrap with module Xml := Xml and module Svg := Svg) =
struct
  open Html

  let create_pid_attrs (pid, (info : PID.t)) =
    let open Application_types.MPEG_TS.PID in
    [ a_user_data "type" (Yojson.Safe.to_string @@ Type.to_yojson info.typ)
    ; a_user_data "pid" (string_of_int pid) ]
    |> Utils.cons_if_lazy info.has_pts (fun () -> a_user_data "has-pts" "")
    |> Utils.cons_if_lazy info.has_pcr (fun () -> a_user_data "has-pcr" "")
    |> Utils.cons_if_lazy info.scrambled (fun () -> a_user_data "scrambled" "")
    |> Utils.cons_if_lazy (not info.present) (fun () -> a_user_data "lost" "")
    |> Utils.map_cons_option (a_user_data "service-id" % string_of_int) info.service_id
    |> Utils.map_cons_option (a_user_data "service-name") info.service_name

  let create_title ?(classes = []) ?(a = []) ?total () =
    let text =
      match total with
      | None -> "PIDs"
      | Some x -> Printf.sprintf "PIDs (%d)" x
    in
    let classes = CSS.title :: classes in
    span ~a:(a_class classes :: a) [txt text]

  let create_pid ?(classes = []) ?(a = []) ?(hex = false) ~pid () =
    let classes = CSS.pid :: classes in
    let text =
      if hex then Util.pid_to_hex_string (fst pid) else Util.pid_to_dec_string (fst pid)
    in
    span ~a:((a_class classes :: create_pid_attrs pid) @ a) [txt text]

  let create_pids ?(classes = []) ?(a = []) ?hex ?pids ?children () =
    let children =
      match children with
      | Some x -> x
      | None -> (
        match pids with
        | None -> []
        | Some pids -> List.map (fun pid -> create_pid ?hex ~pid ()) pids)
    in
    let classes = CSS.pids :: classes in
    div ~a:(a_class classes :: a) children

  let create ?(classes = []) ?(a = []) ?hex ?children ?pids () =
    let children =
      match children with
      | Some x -> x
      | None ->
          [ create_title ?total:(Option.map List.length pids) ()
          ; create_pids ?hex ?pids () ]
    in
    let classes = CSS.root :: classes in
    div ~a:(a_class classes :: a) children
end

module F = Make (Tyxml.Xml) (Tyxml.Svg) (Tyxml.Html)
