module Queue = struct

  type 'a t =
    { m : Lwt_mutex.t
    ; c : unit Lwt_condition.t
    ; q : 'a Queue.t
    }

  let create () =
    { m = Lwt_mutex.create ()
    ; c = Lwt_condition.create ()
    ; q = Queue.create ()
    }

  let ( >>= ) = Lwt.( >>= )

  let add e t =
    Queue.add e t.q;
    Lwt_condition.signal t.c ()

  let take t =
    Lwt_mutex.lock t.m
    >>= fun () ->
    (if Queue.is_empty t.q
     then Lwt_condition.wait ~mutex:t.m t.c
     else Lwt.return ())
    >>= fun () ->
    let e = Queue.take t.q in
    Lwt_mutex.unlock t.m;
    Lwt.return e

end
