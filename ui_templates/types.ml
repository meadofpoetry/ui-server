open Components

type 'a settings_block =
  Widget.t * 'a option React.signal * ('a -> (unit, Api_js.Http.error) Lwt_result.t)
