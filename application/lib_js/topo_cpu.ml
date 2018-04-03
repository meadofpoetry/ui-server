open Containers
open Components

let base_class = "topology__cpu"

let get_cpu_name (cpu:Common.Topology.topo_cpu) = match cpu.process with
  | "pipeline" -> "Анализатор QoE"
  | s          -> Printf.sprintf "Неизвестный модуль: %s" s

module Header = struct

  class t (cpu:Common.Topology.topo_cpu) () =
    let _class   = Markup.CSS.add_element base_class "header" in
    let title    = get_cpu_name cpu in
    object(self)
      inherit Topo_block.Header.t ~title ()
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

class t (cpu:Common.Topology.topo_cpu) () =
  let header = Header.create cpu in
  let body   = Body.create cpu in
  object(self)
    inherit Topo_block.t ~header ~body ()
    method cpu = cpu
    initializer
      self#add_class base_class;
      self#set_attribute "data-cpu" cpu.process;
  end

let create (cpu:Common.Topology.topo_cpu) =
  new t cpu ()
