open Common.Topology
open Common.Stream

let ( >>= ) o f = match o with
  | Some x -> f x
  | None   -> None

let opt_strcmp x y =
  match x, y with
  | None, None -> 0
  | None, Some _ -> -1
  | Some _, None -> 1
  | Some s1, Some s2 -> String.compare s1 s2

let strmtst = Alcotest.testable Common.Stream.pp (=)
            
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
  
(** Trivial cases *)

(* Find stream on port 1 *)
let stream_on_port1 () =
  let boards, raw_streams, topo, p1, p2, _, _ = init_topo () in
  let streams = Meta_board.merge_streams boards raw_streams topo in
  let parent  = gen_stream_ts (Input { input = RF; id = 0 }) Single ~desc:(Some "this stream") in
  p2 [parent];
  p1 [(gen_raw_stream_ts (Port 1) Single ~desc:None)];
  let reslt =
    try 
      React.S.value streams
      |> List.find (fun s -> s.id = `Ts Single)
      |> (fun s -> Some s)
    with _ -> None
  in
  let expected = Some parent in
  Alcotest.(check (option strmtst)) "stream_on_port1"
    expected
    reslt

(* No parent stream exists *)
let no_parent_streams () =
  let boards, raw_streams, topo, p1, _, p3, p4 = init_topo () in
  let streams = Meta_board.merge_streams boards raw_streams topo in
  let plpstrm = gen_stream_ts (Input { input = ASI; id = 1 }) (T2mi_plp 1) ~desc:(Some "this stream") in
  p4 [ plpstrm ];
  p3 [(gen_stream_ts (Parent plpstrm) Single ~desc:(Some "this stream"));
       plpstrm ];
  p1 [(gen_raw_stream_ts (Port 1) Single ~desc:None)];
  let reslt =
    try 
      React.S.value streams
      |> List.find (fun s -> s.id = `Ts Single)
      |> (fun s -> Some s)
    with _ -> None
  in
  let expected = None in
  Alcotest.(check (option strmtst)) "no_parent_streams"
    expected
    reslt

(* Deeper stream hierarchy *)
let hierarchy () =
  let boards, raw_streams, topo, p1, _, p3, p4 = init_topo () in
  let streams = Meta_board.merge_streams boards raw_streams topo in
  let plpstrm = gen_stream_ts (Input { input = ASI; id = 1 }) (T2mi_plp 1) ~desc:(Some "this stream") in
  let child   = gen_stream_ts (Parent plpstrm) (Unknown 42l) ~desc:(Some "this stream") in
  p4 [plpstrm];
  p3 [child];
  p1 [(gen_raw_stream_ts (Port 2) (Unknown 42l) ~desc:None)];
  let reslt =
    try 
      React.S.value streams
      |> List.find (fun s -> s.id = `Ts (Unknown 42l))
      |> (fun s -> Some s)
    with _ -> None
  in
  let expected = Some (child) in
  Alcotest.(check (option strmtst)) "hierarchy"
    expected
    reslt

(* Bad id *)
let bad_id () =
  let boards, raw_streams, topo, p1, _, p3, p4 = init_topo () in
  let streams = Meta_board.merge_streams boards raw_streams topo in
  let plpstrm = gen_stream_ts (Input { input = ASI; id = 1 }) (T2mi_plp 1) ~desc:(Some "this stream") in
  let child   = gen_stream_ts (Parent plpstrm) (Unknown 42l) ~desc:(Some "this stream") in
  p4 [plpstrm];
  p3 [child];
  p1 [(gen_raw_stream_ts (Port 2) (Unknown 43l) ~desc:None)];
  let reslt =
    try 
      React.S.value streams
      |> List.find (fun s -> s.id = `Ts (Unknown 43l))
      |> (fun s -> Some s)
    with _ -> None
  in
  let expected = None in
  Alcotest.(check (option strmtst)) "bad_id"
    expected
    reslt

(* Locally unpacked plp *)
let unpacked_plp () =
  let boards, raw_streams, topo, p1, p2, _, _ = init_topo () in
  let streams = Meta_board.merge_streams boards raw_streams topo in
  let parent  = gen_stream_ts (Input { input = RF; id = 0 }) (T2mi_plp 42) ~desc:(Some "plp parent") in
  p2 [parent];
  p1 [(gen_raw_stream_ts (Stream (T2mi_plp 42)) (Unknown 43l) ~desc:(Some "43rd stream"));
      (gen_raw_stream_ts (Stream (T2mi_plp 42)) (Unknown 44l) ~desc:(Some "44th stream"));
      (gen_raw_stream_ts (Port 1) (T2mi_plp 42) ~desc:None);
      (gen_raw_stream_ts (Port 1) (Unknown 13l) ~desc:None)];
  let reslt =
    try 
      let sms = React.S.value streams in
      let a = List.find (fun s -> s.id = `Ts (Unknown 43l)) sms in
      let b = List.find (fun s -> s.id = `Ts (Unknown 44l)) sms in
      let p = List.find (fun s -> s.id = `Ts (T2mi_plp 42)) sms in
      [a;b;p]
    with _ -> []
  in
  let expected_a = gen_stream_ts (Parent parent) (Unknown 43l) ~desc:(Some "43rd stream") in
  let expected_b = gen_stream_ts (Parent parent) (Unknown 44l) ~desc:(Some "44th stream") in
  let expected   = [expected_a; expected_b; parent] in
  Alcotest.(check (list strmtst)) "unpacked_plp"
    expected
    reslt

(* Locally unpacked plp + additional stream *)
let unpacked_plp_plus_single () =
  let boards, raw_streams, topo, p1, p2, p3, p4 = init_topo () in
  let streams = Meta_board.merge_streams boards raw_streams topo in
  let additional = gen_stream_ts (Input { input = ASI; id = 1 }) Single ~desc:(Some "additional") in
  let parent     = gen_stream_ts (Input { input = RF; id = 0 }) (T2mi_plp 42) ~desc:(Some "plp parent") in
  p4 [additional];
  p3 [additional];
  p2 [parent];
  p1 [(gen_raw_stream_ts (Stream (T2mi_plp 42)) (Unknown 43l) ~desc:(Some "43rd stream"));
      (gen_raw_stream_ts (Stream (T2mi_plp 42)) (Unknown 44l) ~desc:(Some "44th stream"));
      (gen_raw_stream_ts (Port 1) (T2mi_plp 42) ~desc:None);
      (gen_raw_stream_ts (Port 2) Single ~desc:None)];
  let reslt =
    try 
      let sms = React.S.value streams in
      let a = List.find (fun s -> s.id = `Ts (Unknown 43l)) sms in
      let b = List.find (fun s -> s.id = `Ts (Unknown 44l)) sms in
      let p = List.find (fun s -> s.id = `Ts (T2mi_plp 42)) sms in
      let c = List.find (fun s -> s.id = `Ts Single) sms in
      [a;b;c;p]
    with _ -> []
  in
  let expected_a = gen_stream_ts (Parent parent) (Unknown 43l) ~desc:(Some "43rd stream") in
  let expected_b = gen_stream_ts (Parent parent) (Unknown 44l) ~desc:(Some "44th stream") in
  let expected   = [expected_a; expected_b; additional; parent] in
  Alcotest.(check (list strmtst)) "unpacked_plp_plus_single"
    expected
    reslt

(* Lost stream  *)
let lost_stream () =
  let boards, raw_streams, topo, p1, p2, _, _ = init_topo () in
  let streams = Meta_board.merge_streams boards raw_streams topo in
  let parent  = gen_stream_ts (Input { input = RF; id = 0 }) Single ~desc:(Some "this stream") in
  p2 [parent];
  p1 [(gen_raw_stream_ts (Port 1) Single ~desc:None)];
  let reslt =
    try 
      let sms = React.S.value streams in
      List.find (fun s -> s.id = `Ts Single) sms |> ignore; (* exists *)
      p2 []; (* remove parent stream *)
      List.find (fun s -> s.id = `Ts Single) sms
      |> (fun s -> Some s)
    with _ -> None
  in
  let expected = Some parent in
  Alcotest.(check (option strmtst)) "lost_stream"
    expected
    reslt

(* removed stream *)
let removed_stream () =
  let boards, raw_streams, topo, p1, p2, _, _ = init_topo () in
  let streams = Meta_board.merge_streams boards raw_streams topo in
  let parent  = gen_stream_ts (Input { input = RF; id = 0 }) (T2mi_plp 42) ~desc:(Some "plp parent") in
  p2 [parent];
  p1 [(gen_raw_stream_ts (Stream (T2mi_plp 42)) (Unknown 43l) ~desc:(Some "43rd stream"));
      (gen_raw_stream_ts (Stream (T2mi_plp 42)) (Unknown 44l) ~desc:(Some "44th stream"));
      (gen_raw_stream_ts (Port 1) (T2mi_plp 42) ~desc:None);
      (gen_raw_stream_ts (Port 1) (Unknown 13l) ~desc:None)];
  let reslt =
    try 
      let sms = React.S.value streams in
      List.find (fun s -> s.id = `Ts (Unknown 43l)) sms |> ignore; (* exists *)
      List.find (fun s -> s.id = `Ts (Unknown 44l)) sms |> ignore; (* exists *)
      List.find (fun s -> s.id = `Ts (T2mi_plp 42)) sms |> ignore; (* exists *)
      p1 [(gen_raw_stream_ts (Port 1) (Unknown 13l) ~desc:None)];
      try
        let sms = React.S.value streams in
        let a = List.find (fun s -> s.id = `Ts (Unknown 43l)) sms in
        let b = List.find (fun s -> s.id = `Ts (Unknown 44l)) sms in
        let p = List.find (fun s -> s.id = `Ts (T2mi_plp 42)) sms in
        Some [a;b;p]
      with _ -> Some []
    with _ -> None
  in
  let expected   = Some [] in
  Alcotest.(check (option (list strmtst))) "removed_stream"
    expected
    reslt

(* removed stream  *)
let removed_stream_v2 () =
  let boards, raw_streams, topo, p1, p2, _, _ = init_topo () in
  let streams = Meta_board.merge_streams boards raw_streams topo in
  let addit   = gen_stream_ts (Input { input = RF; id = 0 }) (Unknown 13l) ~desc:(Some "additional") in
  let parent  = gen_stream_ts (Input { input = RF; id = 0 }) (T2mi_plp 42) ~desc:(Some "plp parent") in
  p2 [parent; addit];
  p1 [(gen_raw_stream_ts (Stream (T2mi_plp 42)) (Unknown 43l) ~desc:(Some "43rd stream"));
      (gen_raw_stream_ts (Stream (T2mi_plp 42)) (Unknown 44l) ~desc:(Some "44th stream"));
      (gen_raw_stream_ts (Port 1) (T2mi_plp 42) ~desc:None);
      (gen_raw_stream_ts (Port 1) (Unknown 13l) ~desc:None)];
  let reslt =
    try 
      let sms = React.S.value streams in
      List.find (fun s -> s.id = `Ts (Unknown 43l)) sms |> ignore; (* exists *)
      List.find (fun s -> s.id = `Ts (Unknown 44l)) sms |> ignore; (* exists *)
      List.find (fun s -> s.id = `Ts (T2mi_plp 42)) sms |> ignore; (* exists *)
      p1 [(gen_raw_stream_ts (Port 1) (Unknown 13l) ~desc:None)];
      try
        let sms = React.S.value streams in
        let a = List.find (fun s -> s.id = `Ts (Unknown 13l)) sms in
        Some [a]
      with _ -> Some []
    with _ -> None
  in
  let expected   = Some [addit] in
  Alcotest.(check (option (list strmtst))) "removed_stream_v2"
    expected
    reslt

(* Lost and removed stream  *)
let removed_lost_stream () =
  let boards, raw_streams, topo, p1, p2, _, _ = init_topo () in
  let streams = Meta_board.merge_streams boards raw_streams topo in
  let parent  = gen_stream_ts (Input { input = RF; id = 0 }) Single ~desc:(Some "this stream") in
  p2 [parent];
  p1 [(gen_raw_stream_ts (Port 1) Single ~desc:None)];
  let reslt =
    try 
      let sms = React.S.value streams in
      List.find (fun s -> s.id = `Ts Single) sms |> ignore; (* exists *)
      p2 []; (* remove parent stream *)
      let sms = React.S.value streams in
      List.find (fun s -> s.id = `Ts Single) sms |> ignore; (* still exists *)
      try
        p1 [];
        let sms = React.S.value streams in
        List.find (fun s -> s.id = `Ts Single) sms
        |> (fun s -> Some (Some s))
      with _ -> Some None
    with _ -> None
  in
  let expected = Some None in
  Alcotest.(check (option (option strmtst))) "removed_lost_stream"
    expected
    reslt

(* Local stream  *)
let local_stream () =
  let boards, raw_streams, topo, p1, _, _, _ = init_topo () in
  let streams = Meta_board.merge_streams boards raw_streams topo in
  p1 [(gen_raw_stream_ts (Port 3) (Unknown 13l) ~desc:(Some "Lucky 13th"))];
  let reslt =
    try 
      let sms = React.S.value streams in
      let a = List.find (fun s -> s.id = `Ts (Unknown 13l)) sms in
      Some a
    with _ -> None
  in
  let expected   = Some (gen_stream_ts (Input { input = TSOIP; id = 2 }) (Unknown 13l) ~desc:(Some "Lucky 13th")) in
  Alcotest.(check (option strmtst)) "local_stream"
    expected
    reslt

(* A lot of streams *)
let alot_streams () =
  let boards, raw_streams, topo, p1, p2, p3, p4 = init_topo () in
  let streams = Meta_board.merge_streams boards raw_streams topo in
  let additional = gen_stream_ts (Input { input = ASI; id = 1 }) Single ~desc:(Some "additional") in
  let parent     = gen_stream_ts (Input { input = RF; id = 0 }) (T2mi_plp 42) ~desc:(Some "plp parent") in
  p4 [additional];
  p3 [additional];
  p2 [parent];
  p1 [(gen_raw_stream_ts (Stream (T2mi_plp 42)) (Unknown 43l) ~desc:(Some "43rd stream"));
      (gen_raw_stream_ts (Stream (T2mi_plp 42)) (Unknown 44l) ~desc:(Some "44th stream"));
      (gen_raw_stream_ts (Port 1) (T2mi_plp 42) ~desc:None);
      (gen_raw_stream_ts (Port 2) Single ~desc:None);
      (gen_raw_stream_ts (Port 3) (Unknown 13l) ~desc:(Some "Lucky 13th"))];
  let reslt =
    try 
      let sms = React.S.value streams in
      let a = List.find (fun s -> s.id = `Ts (Unknown 43l)) sms in
      let b = List.find (fun s -> s.id = `Ts (Unknown 44l)) sms in
      let p = List.find (fun s -> s.id = `Ts (T2mi_plp 42)) sms in
      let c = List.find (fun s -> s.id = `Ts Single) sms in
      let l = List.find (fun s -> s.id = `Ts (Unknown 13l)) sms in
      [a;b;c;p;l]
    with _ -> []
  in
  let expected_a = gen_stream_ts (Parent parent) (Unknown 43l) ~desc:(Some "43rd stream") in
  let expected_b = gen_stream_ts (Parent parent) (Unknown 44l) ~desc:(Some "44th stream") in
  let local      = gen_stream_ts (Input { input = TSOIP; id = 2 }) (Unknown 13l) ~desc:(Some "Lucky 13th") in
  let expected   = [expected_a; expected_b; additional; parent; local] in
  Alcotest.(check (list strmtst)) "unpacked_plp_plus_single"
    expected
    reslt

(* Appeared stream *)
let appeared_stream () =
  let boards, raw_streams, topo, p1, p2, _, _ = init_topo () in
  let streams = Meta_board.merge_streams boards raw_streams topo in
  let parent  = gen_stream_ts (Input { input = RF; id = 0 }) Single ~desc:(Some "this stream") in
  p1 [(gen_raw_stream_ts (Port 1) Single ~desc:None)];
  p2 [parent];
  let reslt =
    try 
      React.S.value streams
      |> List.find (fun s -> s.id = `Ts Single)
      |> (fun s -> Some s)
    with _ -> None
  in
  let expected = Some parent in
  Alcotest.(check (option strmtst)) "appeared_stream"
    expected
    reslt
  
let test_set =
  [
    "Case 1: stream_on_port1"          , `Slow, stream_on_port1;
    "Case 2: no_parent_streams"        , `Slow, no_parent_streams;
    "Case 3: hierarchy"                , `Slow, hierarchy;
    "Case 4: bad_id"                   , `Slow, bad_id;
    "Case 5: unpacked_plp"             , `Slow, unpacked_plp;
    "Case 6: unpacked_plp_plus_single" , `Slow, unpacked_plp_plus_single;
    "Case 7: lost_stream"              , `Slow, lost_stream;
    "Case 8: removed_stream"           , `Slow, removed_stream;
    "Case 9: removed_stream_v2"        , `Slow, removed_stream_v2;
    "Case 10: removed_lost_stream"     , `Slow, removed_lost_stream;
    "Case 11: local_stream"            , `Slow, local_stream;
    "Case 12: alot_streams"            , `Slow, alot_streams;
    "Case 13: appeared_stream"         , `Slow, appeared_stream;
  ]

let () =
  Alcotest.run "Meta_board test" [
      "test", test_set
    ]
