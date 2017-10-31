open Common.Topology
open Common.Stream

let ( >>= ) o f = match o with
  | Some x -> f x
  | None   -> None
   
let rec dummy_cont _ = `Continue dummy_cont
                     
let topo = { typ   = TS
           ; model = ""
           ; manufacturer = ""
           ; version = 1
           ; control = 1
           ; connection = `Fine
           ; ports = [ { port = 1
                       ; listening = true
                       ; child     = Board ({ typ   = DVB
                                            ; model = ""
                                            ; manufacturer = ""
                                            ; version = 1
                                            ; control = 2
                                            ; connection = `Fine
                                            ; ports = [ { port = 1
                                                        ; listening = true
                                                        ; child = Input ( { input = RF; id = 0 } ) } ]
                       } ) }
                     ; { port = 2
                       ; listening = true
                       ; child     = Board ( { typ = IP2TS
                                             ; model = ""
                                             ; manufacturer = ""
                                             ; version = 1
                                             ; control = 3
                                             ; connection = `Fine
                                             ; ports = [ { port = 1
                                                         ; listening = true
                                                         ; child = Board ( { typ = TS2IP
                                                                           ; model = ""
                                                                           ; manufacturer = ""
                                                                           ; version = 1
                                                                           ; control = 4
                                                                           ; connection = `Fine
                                                                           ; ports = [ { port = 1
                                                                                       ; listening = true
                                                                                       ; child = Input ( { input = ASI
                                                                                                         ; id = 1
                                                                                     } ) } ]
                                                       } ) }]
                       } ) }
                     ; { port = 3
                       ; listening = true
                       ; child = Input ( { input = TSOIP; id = 2 } ) } ]
           }

let make_board i : (Meta_board.board * _) =
  let s, push = React.S.create [] in
  { handlers = []
  ; control  = i
  ; streams_signal = s
  ; step = `Continue dummy_cont
  ; connection = React.S.const `Fine
  ; ports_active = Meta_board.Ports.empty
  ; state = object end
  },
  push

let pack_boards (blist : Meta_board.board list) =
  List.fold_left (fun acc (b : Meta_board.board) -> Meta_board.Map.add b.control b acc)
                  Meta_board.Map.empty
                  blist

let gen_stream_ts src id ?(desc = None) : Common.Stream.t =
  { source = src
  ; id     = `Ts id
  ; description = desc
  }

let gen_stream_ip src ip ?(desc = None) : Common.Stream.t =
  { source = src
  ; id     = `Ip ip
  ; description = desc
  }

let gen_raw_stream_ts src id ?(desc = None) : Common.Stream.stream =
  { source = src
  ; id     = `Ts id
  ; description = desc
  }

let init_topo () =
  let streams, p1 = React.S.create [] in
  let b2, p2 = make_board 2 in
  let b3, p3 = make_board 3 in
  let b4, p4 = make_board 4 in
  let boards = pack_boards [b2;b3;b4] in
  boards, streams, topo, p1, p2, p3, p4
  
(** Regular cases *)

let test1 () =
  let boards, raw_streams, topo, p1, p2, _, _ = init_topo () in
  let streams = Meta_board.merge_streams boards raw_streams topo in
  p2 [(gen_stream_ts (Input { input = RF; id = 0 }) Single ~desc:(Some "this stream"))];
  p1 [(gen_raw_stream_ts (Port 1) Single ~desc:None)];
  let reslt =
    try 
      React.S.value streams
      |> List.find (fun s -> s.id = `Ts Single)
      |> (fun s -> s.source)
      |> function Parent p -> p.description
                | _        -> None
    with _ -> None
  in
  Alcotest.(check (option string)) "Trivial case 1"
    (Some "this stream")
    reslt
  
let test_set =
  [
    "Find single" , `Slow, test1;
  ]

let () =
  Alcotest.run "Meta_board test" [
      "test", test_set
    ]
