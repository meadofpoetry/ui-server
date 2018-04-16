open Components

type ('a,'b) settings_block = Widget.widget * 'a option React.signal * ('a -> ('b,string) Lwt_result.t)

type settings_section     = Widget.widget * (unit -> unit)
type settings_section_lwt = (settings_section,string) Lwt_result.t
