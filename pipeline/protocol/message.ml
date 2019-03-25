open Pipeline_types

type 'a res = ('a, string) result

(*
let res_to_yojson ok = function
  | Ok v -> `Assoc ["Ok", ok v]
  | Error s -> `Assoc ["Err", `String s]
 *)
let res_of_yojson ok = function
  | `Assoc ["Ok", v] -> Ok(ok v)
  | `Assoc ["Err", `String s] -> Ok(Error s)
  | _ -> Error "res_to_yojson"

type 'a message = { name    : string
                  ; meth    : string [@key "method"]
                  ; counter : int32
                  ; content : 'a
                  } [@@deriving yojson { strict = false }]

(* TODO remove after 4.08 *)

let is_ok = function Ok _ -> true | _ -> false

type chan =
  { send : Yojson.Safe.json -> (Yojson.Safe.json, exn) result Lwt.t }
  
type 'a t = ('a, string) Lwt_result.t

let (>>=) = Lwt_result.(>>=)

let return = Lwt_result.return

let _counter = ref 0l
             
let message name meth content =
  let msg = { name; meth; counter = !_counter; content } in
  _counter := Int32.add !_counter 1l;
  msg

let call ~name ~meth
      (content_to : 'b -> Yojson.Safe.json)
      (content_from : Yojson.Safe.json -> 'a res) chan
      ?(options : 'b Kv_v.rw option)
      content =
  (* TODO fix err messages and err evaluation logic *)
  let open Lwt_result.Infix in
  let msg = message name meth content in
  message_to_yojson content_to msg
  |> chan.send
  |> Lwt_result.map_err Printexc.to_string
  |> (fun js -> Lwt_result.bind_result js (message_of_yojson (res_of_yojson content_from)))
  >>= fun res ->
  if res.counter <> msg.counter
  then Lwt.return_error "msg order mismatch"
  else
    (if not @@ is_ok res.content
     then Lwt.return res.content
     else
       match options with
       | None ->
          Lwt.return res.content
       | Some options ->
          Lwt.bind
            (options#set content)
            (fun () -> Lwt.return res.content))
    
let create_channel mutex send =
  let send x =
    Lwt_mutex.with_lock mutex (fun () -> send x)
  in { send }

module Protocol = struct
  let unit_to_yojson = fun () -> `Null
  let unit_of_yojson = fun _ -> Ok ()
  let established_of_yojson = function
    | `String "established" -> Ok ()
    | _ -> Error "established_of_yojson"

  let ready = call ~name:"connection" ~meth:"accept" ?options:None
                unit_to_yojson established_of_yojson

  let stream_parser_get =  call ~name:"stream_parser" ~meth:"get" ?options:None
                             unit_to_yojson
                             (Util_json.List.of_yojson Structure.of_yojson)

  let graph_get_structure = call ~name:"graph" ~meth:"get_structure" ?options:None
                              unit_to_yojson
                              (Util_json.List.of_yojson Structure.of_yojson)

  let graph_apply_structure = call ~name:"graph" ~meth:"apply_structure"
                                (Util_json.List.to_yojson Structure.to_yojson)
                                unit_of_yojson

  let wm_get_layout = call ~name:"wm" ~meth:"get_layout" ?options:None
                        unit_to_yojson Wm.of_yojson

  let wm_apply_layout = call ~name:"wm" ~meth:"apply_layout"
                          Wm.to_yojson unit_of_yojson

end
