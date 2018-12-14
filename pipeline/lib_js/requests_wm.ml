open Api_js.Requests.Json_request

module WS = struct

  open Common.Uri

  let get () =
    WS.get ?secure:None ?host:None ?port:None
      ~path:Path.Format.("api/pipeline/wm" @/ empty)
      ~query:Query.empty
      ~from:Wm.of_yojson

end

module HTTP = struct

  open Common.Uri

  let apply_layout wm =
    post_result ?scheme:None ?from_err:None ?host:None ?port:None
      ~path:Path.Format.("api/pipeline/wm" @/ empty)
      ~query:Query.empty
      ~contents:(Wm.to_yojson wm)
      ~from:(fun _ -> Ok ())

  let get_layout () =
    get_result ?scheme:None ?from_err:None ?host:None ?port:None
      ~path:Path.Format.("api/pipeline/wm" @/ empty)
      ~query:Query.empty
      ~from:Wm.of_yojson

end
