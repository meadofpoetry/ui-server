open Containers
open Common.Uri.Query

module Collection = struct

  let k_limit = "limit"
  let k_total = "total"
  let k_thin  = "thin"

  let get (q:t) f ef =
    parse_query [ k_limit, (module Option(Int))
                ; k_total, (module Option(Bool))
                ; k_thin,  (module Option(Bool))
                ]
                (fun lim tot thn -> lim,tot,thn) q
    |> function
      | Ok (limit,total,thin) -> f ?limit ?total ?thin ()
      | Error e -> ef e

end

module Time = struct

  open Common.Time

  type t = Common.Uri.Query.t

  let k_start = "start"
  let k_end   = "end"
  let k_dur   = "duration"

  module Show_time = Either(Show_RFC3339)(Show_float)

  module Show = struct
    type t = Common.Time.t
    let of_string s = match Show_time.of_string s with
      | `Left x -> x | `Right x -> x
    let to_string t = Show_time.to_string (`Left t)
  end

  let get_start (q:t) =
    parse_query' [ k_start, (module Option(Show)) ] (fun x -> x) q

  let get_end (q:t) =
    parse_query' [ k_end, (module Option(Show)) ] (fun x -> x) q

  let get_duration (q:t) =
    parse_query' [ k_dur, (module Option(Relative)) ] (fun x -> x) q

  let get (q:t) =
    let err s = Result.fail (Parser_error s) in
    let (>>=) = Result.Infix.(>>=) in
    get_start q
    >>= fun (start,q) -> get_end q
    >>= fun (end',q)  -> get_duration q
    >>= fun (dur,q)   -> let ok v = Result.return (v,q) in
                         match start,end',dur with
                         | Some _,Some _,Some _ -> err "excessive duration query"
                         | Some s,Some e,None   -> ok (Some (`Range (s,e)))
                         | Some s,None,Some d   -> (match add_span s d with
                                                    | Some e -> ok (Some (`Range (s,e)))
                                                    | None   -> err "time range exceeded")
                         | Some s,None,None     -> ok (Some (`From s))
                         | None,Some e,Some d   -> (match sub_span e d with
                                                    | Some s -> ok (Some (`Range (s,e)))
                                                    | None   -> err "time range exceeded")
                         | None,Some e,None     -> ok (Some (`Till e))
                         | None,None,Some d     -> let e = Clock.now () in
                                                   (match sub_span e d with
                                                    | Some s -> ok (Some (`Range (s,e)))
                                                    | None   -> err "time range exceeded")
                         | None,None,None       -> ok None

end
