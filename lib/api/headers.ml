let is_ws headers = match Cohttp.Header.get headers "upgrade" with
  | Some "websocket" -> true
  | _                -> false
