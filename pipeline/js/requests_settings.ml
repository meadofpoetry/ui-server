open Api_js.Requests.Json_request

module WS = struct

  open Common.Uri

  let get () =
    WS.get ?secure:None ?host:None ?port:None
      ~path:Path.Format.("api/pipeline/settings" @/ empty)
      ~query:Query.empty
      ~from:Settings.of_yojson

end

module HTTP = struct

  open Common.Uri

  let set s =
    post_result ?scheme:None ?from_err:None ?host:None ?port:None
      ~path:Path.Format.("api/pipeline/settings" @/ empty)
      ~query:Query.empty
      ~contents:(Settings.to_yojson s)
      ~from:(fun _ -> Ok ())

  let get () =
    get_result ?scheme:None ?from_err:None ?host:None ?port:None
      ~path:Path.Format.("api/pipeline/settings" @/ empty)
      ~query:Query.empty
      ~from:Settings.of_yojson

end
