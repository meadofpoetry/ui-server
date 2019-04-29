open Components
open Api_js.Requests

type 'a settings_block =
  Widget.t * 'a option React.signal * ('a -> (unit, error) Lwt_result.t)
