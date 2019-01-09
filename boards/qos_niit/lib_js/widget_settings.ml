open Board_types

type config = unit [@@deriving yojson]

let name = "Настройки"
let settings = None

let make ~(state : Common.Topology.state React.signal)
      ~(t2mi_mode : t2mi_mode option React.signal)
      ~(jitter_mode : jitter_mode option React.signal)
      ~(streams : Common.Stream.t list React.signal)
      (_ : config option)
      (control : int) =
  ignore jitter_mode;
  let t2mi = Widget_t2mi_settings.make ~state ~streams ~mode:t2mi_mode None control in
  (* let jitter = Widget_jitter_settings.make ~state ~mode:jitter_mode None control in *)
  t2mi
  (* let tabs   =
   *   [ new Tab.t ~content:(Text "T2-MI") ~value:t2mi ()
   *   ; new Tab.t ~content:(Text "Джиттер") ~value:jitter () ] in
   * Ui_templates.Tabs.(create_simple tabs |> wrap_simple) *)
