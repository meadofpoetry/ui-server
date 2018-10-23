open Api_js.Requests.Json_request

module WS = struct

  open Common.Uri

  let get () =
    WS.get ?secure:None ?host:None ?port:None
      ~path:Path.Format.("api/pipeline/status" @/ empty)
      ~query:Query.empty
      ~from:Qoe_status.status_list_of_yojson

end

module HTTP = struct

  open Common.Uri

  let get () =
    get_result ?scheme:None ?from_err:None ?host:None ?port:None
      ~path:Path.Format.("api/pipeline/status" @/ empty)
      ~query:Query.empty
      ~from:Qoe_status.status_list_of_yojson

end
