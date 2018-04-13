open Containers
open Components
open Topo_types

let base_class = "topology__cpu"

let get_cpu_name (cpu:Common.Topology.topo_cpu) = match cpu.process with
  | "pipeline" -> "Анализатор QoE"
  | s          -> Printf.sprintf "Неизвестный модуль: %s" s

module Header = struct

  class t (cpu:Common.Topology.topo_cpu) () =
    let _class   = Markup.CSS.add_element base_class "header" in
    let title    = get_cpu_name cpu in
    let settings = new Icon.Button.Font.t ~icon:"settings" () in
    object(self)
      inherit Topo_block.Header.t ~action:settings#widget ~title ()
      method settings_icon = settings
      method layout        = self#settings_icon#layout
      initializer
        self#add_class _class;
    end

  let create (cpu:Common.Topology.topo_cpu) =
    new t cpu ()

end

module Body = struct

  class t (cpu:Common.Topology.topo_cpu) () =
  object
    inherit Topo_block.Body.t (List.length cpu.ifaces) ()
  end

  let create (cpu:Common.Topology.topo_cpu) =
    new t cpu ()

end

let make_cpu_page ?error_prefix (cpu:Common.Topology.topo_cpu) =
  match cpu.process with
  | "pipeline" -> Topo_pipeline.make ?error_prefix ()
  | s          -> Lwt_result.fail (Printf.sprintf "Неизвестный процесс: %s" s)

class t ~(connections:(#Topo_node.t * connection_point) list)
        (cpu:Common.Topology.topo_cpu)
        () =
  let header     = Header.create cpu in
  let body       = Body.create cpu in
  let e_settings = React.E.map (fun _ -> cpu) header#settings_icon#e_click in
  object(self)
    inherit Topo_block.t ~node:(`CPU cpu) ~connections ~header ~body () as super
    method cpu        = cpu
    method layout     = super#layout; header#layout
    method e_settings = e_settings
    initializer
      List.iter (fun p -> p#set_state `Active) self#paths;
      self#add_class base_class;
      self#set_attribute "data-cpu" cpu.process;
  end

let create ~connections (cpu:Common.Topology.topo_cpu) =
  new t ~connections cpu ()
