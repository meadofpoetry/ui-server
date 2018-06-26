open Containers
open Common.Uri.Query

module Stream = struct

  let k_id = "id"

  module Show = struct
    type t = Common.Stream.id
    let typ = "int32"
    let of_string s = Int32.of_string_opt s
                      |> CCOpt.map Common.Stream.id_of_int32
                      |> CCOpt.get_exn
    let to_string x = Common.Stream.id_to_int32 x |> Int32.to_string
  end

  let make x =
    make_query [ k_id, (module Option(Show)) ] x

  let get' (q:t) =
    parse_query' [ k_id, (module Option(Show)) ] (fun x -> x) q
  let get  (q:t) =
    get' q |> Result.map fst

  let map' (q:t) f ef =
    get' q |> function Ok x -> f x | Error e -> ef e
  let map (q:t) f ef =
    get  q |> function Ok x -> f x | Error e -> ef e
end

module Collection = struct

  let k_limit = "limit"
  let k_total = "total"
  let k_thin  = "thin"

  let make ?limit ?total ?thin () =
    make_query [ k_limit, (module Option(Int))
               ; k_total, (module Option(Bool))
               ; k_thin,  (module Option(Bool))
               ]
               limit total thin

  let get' (q:t) =
    parse_query' [ k_limit, (module Option(Int))
                 ; k_total, (module Option(Bool))
                 ; k_thin,  (module Option(Bool))
                 ]
                 (fun lim tot thn -> lim,tot,thn) q
  let get  (q:t) =
    get' q |> Result.map fst


  let map' (q:t) f ef =
    get' q |> function Ok ((limit,total,thin),rest) -> f ?limit ?total ?thin rest () | Error e -> ef e
  let map  (q:t) f ef =
    get  q |> function Ok (limit,total,thin) -> f ?limit ?total ?thin () | Error e -> ef e

end

module Time = struct

  open Common.Time

  type t = Common.Uri.Query.t

  let k_start = "start"
  let k_end   = "end"
  let k_dur   = "duration"

  module Show_duration = Relative

  let make time =
    let aux ?from ?till ?duration () =
      make_query [ k_start, (module Option(Show))
                 ; k_end,   (module Option(Show))
                 ; k_dur,   (module Option(Relative))
                 ]
                 from till duration
    in match time with
       | `From (t,d)  -> aux ?duration:d ~from:t ()
       | `Till (t,d)  -> aux ?duration:d ~till:t ()
       | `Range (f,t) -> aux ~from:f ~till:t ()
       | `Duration d  -> aux ~duration:d ()

  let get' _ = Ok (None,([]:Common.Uri.Query.t))

end
