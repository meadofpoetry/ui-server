open Board_protocol

module Api_handler = Api.Handler.Make(Common.User)

let handle api (ev:events) _ meth path uri sock_data _ body =
  let scheme = Api_common.meth_of_uri uri in
  match Api_utils.req_of_path path with
  | Some (`Device x)  -> Board_api_device.handle  api ev.device  scheme meth x uri sock_data body ()
  | Some (`Errors x)  -> Board_api_errors.handle  api ev.errors  scheme meth x uri sock_data body ()
  | Some (`Streams x) -> Board_api_streams.handle api ev.streams scheme meth x uri sock_data body ()
  | Some (`Jitter x)  -> Board_api_jitter.handle  api ev.jitter  scheme meth x uri sock_data body ()
  | None              -> Api.Redirect.not_found ()

let handlers id api events =
  [ (module struct
       let domain = Common.Topology.get_api_path id
       let handle = handle api events
     end : Api_handler.HANDLER) ]
