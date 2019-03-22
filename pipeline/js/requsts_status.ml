open Api_js.Requests.Json_request
open Common

module WS = struct

  let get () =
    WS.get ?secure:None ?host:None ?port:None
      ~path:Uri.Path.Format.("api/pipeline/status" @/ empty)
      ~query:Uri.Query.["id", (module List(Stream.ID))]
      ~from:Qoe_status.status_list_of_yojson

end

module HTTP = struct

  let get ?(ids = []) () =
    get_result ?scheme:None ?from_err:None ?host:None ?port:None
      ~path:Uri.Path.Format.("api/pipeline/status" @/ empty)
      ~query:Uri.Query.["id", (module List(Stream.ID))]
      ~from:Qoe_status.status_list_of_yojson
      ids

end
