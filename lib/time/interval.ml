module type Time_now = sig
  val now : unit -> Ptime.t
end

module Make (M : Time_now) = struct
  let make ?(from : Ptime.t option) ?(till : Ptime.t option)
      ?(duration : Ptime.span option) () =
    match (from, till, duration) with
    | Some _, Some _, Some _ -> Error "excessive duration query"
    | Some s, Some e, None -> Ok (`Range (s, e))
    | Some s, None, Some d -> (
        match Ptime.add_span s d with
        | Some e -> Ok (`Range (s, e))
        | None -> Error "time range exceeded" )
    | Some s, None, None -> Ok (`Range (s, Ptime.max))
    | None, Some e, Some d -> (
        match Ptime.sub_span e d with
        | Some s -> Ok (`Range (s, e))
        | None -> Error "time range exceeded" )
    | None, Some e, None -> Ok (`Range (Ptime.epoch, e))
    | None, None, Some d -> (
        let now = M.now () in
        match Ptime.sub_span now d with
        | Some s -> Ok (`Range (s, now))
        | None -> Error "time range exceeded" )
    | None, None, None -> Ok (`Range (Ptime.epoch, Ptime.max))
end
