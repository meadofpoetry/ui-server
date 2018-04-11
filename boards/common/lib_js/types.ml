open Components

type ('a,'b) settings_block = Widget.widget * 'a option React.signal * ('a -> ('b,string) Lwt_result.t)

type topo_settings        = Widget.widget * (unit -> unit)
type topo_settings_result = (topo_settings,string) Lwt_result.t
