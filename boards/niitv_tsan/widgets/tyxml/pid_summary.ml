open Components_tyxml
open Board_niitv_tsan_types

module CSS = struct
  let root = Util.CSS.root ^ "-pid-summary"

  let title = BEM.add_element root "title"

  let pids = BEM.add_element root "pids"

  let pid = BEM.add_element root "pid"
end

module Make
    (Xml : Intf.Xml)
    (Svg : Svg_sigs.T with module Xml := Xml)
    (Html : Html_sigs.T with module Xml := Xml and module Svg := Svg) =
struct
  open Xml.W
  open Html

  let create_pid_attrs (pid, (info : PID.t)) =
    let open Application_types.MPEG_TS.PID in
    [
      a_user_data "type"
        (return (Yojson.Safe.to_string @@ Type.to_yojson info.typ));
      a_user_data "pid" (return (string_of_int pid));
    ]
    |> Utils.cons_if_lazy info.has_pts (fun () ->
           a_user_data "has-pts" (return ""))
    |> Utils.cons_if_lazy info.has_pcr (fun () ->
           a_user_data "has-pcr" (return ""))
    |> Utils.cons_if_lazy info.scrambled (fun () ->
           a_user_data "scrambled" (return ""))
    |> Utils.cons_if_lazy (not info.present) (fun () ->
           a_user_data "lost" (return ""))
    |> Utils.map_cons_option
         (a_user_data "service-id" % return % string_of_int)
         info.service_id
    |> Utils.map_cons_option
         (a_user_data "service-name" % return)
         info.service_name

  let create_title ?(classes = return []) ?(a = []) ~total () =
    let text = fmap (Printf.sprintf "PIDs (%d)") total in
    let classes = fmap (fun x -> CSS.title :: x) classes in
    span ~a:(a_class classes :: a) (singleton (return (txt text)))

  let create_pid ?(a = []) ?(hex = return false) ~pid () =
    let text =
      fmap
        (fun hex ->
          if hex then Util.pid_to_hex_string (fst pid)
          else Util.pid_to_dec_string (fst pid))
        hex
    in
    span
      ~a:((a_class (return [ CSS.pid ]) :: create_pid_attrs pid) @ a)
      (singleton (return (txt text)))

  let create ?(classes = return []) ?(a = []) ?hex ?(pids = nil ()) () =
    let pid_elts = Xml.W.map (fun pid -> create_pid ?hex ~pid ()) pids in
    let children =
      [
        create_title ~total:(fmap List.length (Xml.Wutils.tot pids)) ();
        div ~a:[ a_class (return [ CSS.pids ]) ] pid_elts;
      ]
    in
    let classes = fmap (fun x -> CSS.root :: x) classes in
    div ~a:(a_class classes :: a) (Xml.Wutils.const children)
end

module F = Make (Impl.Xml) (Impl.Svg) (Impl.Html)
