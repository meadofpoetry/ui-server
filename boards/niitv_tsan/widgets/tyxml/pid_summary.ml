open Components_tyxml
open Board_niitv_tsan_types

module CSS = struct
  let root = Util.CSS.root ^ "-pid-summary"

  let title = BEM.add_element root "title"

  let pids = BEM.add_element root "pids"

  let pid = BEM.add_element root "pid"
end

module Make(Xml : Xml_sigs.NoWrap)
    (Svg : Svg_sigs.NoWrap with module Xml := Xml)
    (Html : Html_sigs.NoWrap with module Xml := Xml
                              and module Svg := Svg) = struct
  open Html

  let make_pid_attrs ({ has_pts
                      ; has_pcr
                      ; scrambled
                      ; present
                      ; service_id
                      ; service_name
                      ; typ
                      } : PID_info.t) =
    let open Application_types.MPEG_TS.PID in
    [a_user_data "type" (Yojson.Safe.to_string @@ Type.to_yojson typ)]
    |> Utils.cons_if_lazy has_pts (fun () -> a_user_data "has-pts" "")
    |> Utils.cons_if_lazy has_pcr (fun () -> a_user_data "has-pcr" "")
    |> Utils.cons_if_lazy scrambled (fun () -> a_user_data "scrambled" "")
    |> Utils.cons_if_lazy (not present) (fun () -> a_user_data "lost" "")
    |> Utils.map_cons_option (a_user_data "service-id" % string_of_int) service_id
    |> Utils.map_cons_option (a_user_data "service-name") service_name

  let make_title ?(classes = []) ?(attrs = []) ?total () =
    let text = match total with
      | None -> "PIDs"
      | Some x -> Printf.sprintf "PIDs (%d)" x in
    let classes = CSS.title :: classes in
    span ~a:([a_class classes] @ attrs) [txt text]

  let make_pid ?(classes = []) ?(attrs = [])
      ?(hex = false) pid =
    let classes = CSS.pid :: classes in
    let text =
      if hex then Util.pid_to_hex_string (fst pid)
      else Util.pid_to_dec_string (fst pid) in
    span ~a:([a_class classes] @ make_pid_attrs (snd pid) @ attrs)
      [txt text]

  let make_pids ?(classes = []) ?(attrs = []) ?hex pids =
    let pids = match pids with
      | `Info x -> List.map (make_pid ?hex) x
      | `Html x -> x in
    let classes = CSS.pids :: classes in
    div ~a:([a_class classes] @ attrs) pids

  let make ?(classes = []) ?(attrs = []) ?hex ?content ?pids () =
    let content = match content with
      | Some x -> x
      | None ->
        [ make_title ?total:(Option.map List.length pids) ()
        ; make_pids ?hex (`Info (Option.value ~default:[] pids))
        ] in
    let classes = CSS.root :: classes in
    div ~a:([a_class classes] @ attrs) content
end
