open Components
open Api_js.Requests

type ('a,'b) settings_block = Widget.t * 'a option React.signal * ('a -> (unit,'b err) Lwt_result.t)
