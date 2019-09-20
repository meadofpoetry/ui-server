open Components
open Application_types
open Topo_types

module CSS = struct
  let root = "topology-cpu"

  let header = BEM.add_element root "header"
end

let get_cpu_name (cpu : Topology.topo_cpu) =
  match cpu.process with
  | "pipeline" -> "Анализатор QoE"
  | s -> Printf.sprintf "Неизвестный модуль: %s" s

let make_cpu_page socket (cpu : Topology.topo_cpu) =
  match cpu.process with
  | "pipeline" ->
      let getter () =
        Topo_pipeline.make cpu socket
        |> Components_lab.Loader.make_widget_loader
        |> Widget.create
      in
      Some getter
  | _ -> None

module Header = struct
  class t (has_settings_button : bool) (cpu : Topology.topo_cpu) () =
    let title = get_cpu_name cpu in
    let settings =
      match has_settings_button with
      | false -> None
      | true ->
          let button =
            Icon_button.make
              ~classes:[Topo_block.CSS.header_action_settings]
              ~icon:Icon.SVG.(D.icon ~d:Path.settings ())
              ()
          in
          Some button
    in
    object
      inherit
        Topo_block.Header.t ?action:(Option.map Widget.markup settings) ~title () as super

      method settings_icon = settings

      method! init () : unit =
        super#add_class CSS.header;
        super#init ()

      method! layout () : unit =
        Option.iter Widget.layout settings;
        super#layout ()

      method! destroy () : unit =
        Option.iter Widget.destroy settings;
        super#destroy ()
    end

  let create (has_settings_button : bool) (cpu : Topology.topo_cpu) =
    new t has_settings_button cpu ()
end

module Body = struct
  class t (cpu : Topology.topo_cpu) () =
    object
      inherit Topo_block.Body.t (List.length cpu.ifaces) ()
    end

  let create (cpu : Topology.topo_cpu) = new t cpu ()
end

class t
  ~(connections : (#Topo_node.t * connection_point) list)
  (socket : Api_js.Websocket.JSON.t)
  (cpu : Topology.topo_cpu) =
  let e_settings, push_settings = React.E.create () in
  let make_settings = make_cpu_page socket cpu in
  let header = Header.create (Option.is_some make_settings) cpu in
  let body = Body.create cpu in
  let port_setter _ _ = Lwt_result.fail "CPU ports are not switchable" in
  object (self)
    val mutable _click_listener = None

    inherit
      Topo_block.t ~port_setter ~node:(`CPU cpu) ~connections ~header ~body () as super

    method! init () : unit =
      super#init ();
      List.iter (fun p -> p#set_state `Active) self#paths;
      self#add_class CSS.root;
      self#set_attribute "data-cpu" cpu.process;
      Option.iter
        (fun (w : #Widget.t) ->
          let listener =
            Js_of_ocaml_lwt.Lwt_js_events.clicks w#root (fun _ _ ->
                let name = get_cpu_name self#cpu in
                let widget = self#make_settings_widget () in
                push_settings (widget, name);
                Lwt.return_unit)
          in
          _click_listener <- Some listener)
        header#settings_icon

    method! destroy () : unit =
      super#destroy ();
      Option.iter Lwt.cancel _click_listener;
      _click_listener <- None

    method! layout () : unit =
      super#layout ();
      header#layout ()

    method settings_event : (Widget.t * string) React.event = e_settings

    method cpu = cpu

    (* Private methods *)
    method private make_settings_widget () : Widget.t =
      match make_settings with
      | Some make -> make ()
      | None ->
          let ph =
            Components_lab.Placeholder.make
              ~icon:Icon.SVG.(D.icon ~d:Path.stop ())
              ~text:
                (`Text "Нет доступных настроек для модуля")
              ()
          in
          ph#widget
  end

let create ~connections socket (cpu : Topology.topo_cpu) = new t ~connections socket cpu
