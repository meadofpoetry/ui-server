open Containers
open Board_types
open Api_js.Requests.Json_request
open Common

let make_path control path = Boards_js.Requests.make_path control ("errors"::path)

module WS = struct

  module TS = struct

    open Errors

    let get_errors ?stream control =
      let query = Api_js.Query.Stream.make stream in
      let path  = make_path control ["ts"] in
      WS.get ~query ~path t_list_of_yojson ()

  end

  module T2MI = struct

    open Errors

    let get_errors ?stream control =
      let query = Api_js.Query.Stream.make stream in
      let path = make_path control ["t2mi"] in
      WS.get ~query ~path t_list_of_yojson ()

  end

end

module HTTP = struct

  let to_errors_query ?errors ?level ?limit ?thin ?total ?stream time =
    let strm = Api_js.Query.Stream.make stream in
    let coll = Api_js.Query.Collection.make ?limit ?total ?thin () in
    let time = Api_js.Query.Time.make time in
    List.fold_left Uri.Query.merge [] [strm;coll;time]

  let to_percent_uri ?errors ?level ?stream time =
    let strm = Api_js.Query.Stream.make stream in
    let time = Api_js.Query.Time.make time in
    List.fold_left Uri.Query.merge [] [strm;time]

  let to_has_any_uri ?errors ?level ?stream time =
    let strm = Api_js.Query.Stream.make stream in
    let time = Api_js.Query.Time.make time in
    List.fold_left Uri.Query.merge [] [strm;time]

  module TS = struct

    module Archive = struct

      let get_errors ?errors ?priority ?limit ?thin ?total time control =
        let query = to_errors_query ?errors ?level:priority ?limit ?thin ?total ?stream:None time in
        let path  = make_path control ["ts"] in
        get_result ~query ~path (fun _ -> Error "not implemented") ()

      let get_errors_for_stream ?errors ?priority ?limit ?thin ?total time id control =
        let query = to_errors_query ?errors ?level:priority ?limit ?thin ?total ~stream:id time in
        let path  = make_path control ["ts"] in
        get_result ~query ~path (fun _ -> Error "not implemented") ()

      let get_percent ?errors ?priority time control =
        let query = to_errors_query ?errors ?level:priority ?stream:None time in
        let path  = make_path control ["ts";"percent"] in
        get_result ~query ~path (fun _ -> Error "not implemented") ()

      let get_percent_for_stream ?errors ?priority time id control =
        let query = to_errors_query ?errors ?level:priority ~stream:id time in
        let path  = make_path control ["ts";"percent"] in
        get_result ~query ~path (fun _ -> Error "not implemented") ()

      let get_has_any ?errors ?priority time control =
        let query = to_errors_query ?errors ?level:priority ?stream:None time in
        let path  = make_path control ["ts";"has-any"] in
        get_result ~query ~path (fun _ -> Error "not implemented") ()

      let get_has_any_for_stream ?errors ?priority time id control =
        let query = to_errors_query ?errors ?level:priority ~stream:id time in
        let path  = make_path control ["ts";"has-any"] in
        get_result ~query ~path (fun _ -> Error "not implemented") ()

    end

  end

  module T2MI = struct

    module Archive = struct

      let get_errors ?errors ?priority ?limit ?thin ?total time control =
        let query = to_errors_query ?errors ?level:priority ?limit ?thin ?total ?stream:None time in
        let path  = make_path control ["t2mi"] in
        get_result ~query ~path (fun _ -> Error "not implemented") ()

      let get_errors_for_stream ?errors ?priority ?limit ?thin ?total time id control =
        let query = to_errors_query ?errors ?level:priority ?limit ?thin ?total ~stream:id time in
        let path  = make_path control ["t2mi"] in
        get_result ~query ~path (fun _ -> Error "not implemented") ()

      let get_percent ?errors ?priority time control =
        let query = to_errors_query ?errors ?level:priority ?stream:None time in
        let path  = make_path control ["t2mi";"percent"] in
        get_result ~query ~path (fun _ -> Error "not implemented") ()

      let get_percent_for_stream ?errors ?priority time id control =
        let query = to_errors_query ?errors ?level:priority ~stream:id time in
        let path  = make_path control ["t2mi";"percent"] in
        get_result ~query ~path (fun _ -> Error "not implemented") ()

      let get_has_any ?errors ?priority time control =
        let query = to_errors_query ?errors ?level:priority ?stream:None time in
        let path  = make_path control ["t2mi";"has-any"] in
        get_result ~query ~path (fun _ -> Error "not implemented") ()

      let get_has_any_for_stream ?errors ?priority time id control =
        let query = to_errors_query ?errors ?level:priority ~stream:id time in
        let path  = make_path control ["t2mi";"has-any"] in
        get_result ~query ~path (fun _ -> Error "not implemented") ()

    end

  end

end
