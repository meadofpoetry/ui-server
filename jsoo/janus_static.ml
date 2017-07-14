let (>|=) x f = Js.Optdef.map x f
let (>>=) = Lwt.(>>=)

let wrap_js_optdef x f = Js.Optdef.option x >|= f |> Js.Unsafe.inject

(** Janus plugin handler **)
module Plugin = struct
  (* Handler *)
  type t = Janus.plugin Js.t

  (* Mountpoint info *)
  type mp_info = { video_age_ms : int
                 ; audio_age_ms : int
                 ; id           : int
                 ; _type        : string
                 ; description  : string
                 }

   type media_video = Bool of bool
                   | Resolution of ([`Lowres | `Stdres | `Hires] * [`Wide | `Square ])
                   | Screen
                   | Device of (int * int * int)

  type media_audio = Bool of bool
                   | Device of int

  type media_props = { audio_send : bool option
                     ; audio_recv : bool option
                     ; audio : media_audio option
                     ; video_send : bool option
                     ; video_recv : bool option
                     ; video : media_video option
                     ; data : bool option
                     ; fail_if_no_video : bool option
                     ; fail_if_no_audio : bool option
                     ; screen_rate : int option
                     }

  (* Plugin request types *)
  type _ request =
    | List      : mp_info list request
    | Info      : int -> mp_info request
    | Create    : int -> string request
    | Destroy   : int -> string request
    | Recording : int -> string request
    | Enable    : int -> string request
    | Disable   : int -> string request
    | Watch     : int -> string request
    | Start     : unit request
    | Pause     : unit request
    | Stop      : unit request
    | Switch    : int -> unit request

  (* Get plugin id *)
  let get_id plugin = (plugin##getId ())
                      |> Js.float_of_number
                      |> Int64.of_float

  (* Get plugin name *)
  let get_name plugin = plugin##getPlugin ()
                        |> Js.to_string

  let request_to_string : type a. a request -> string = function
    | List        -> "list"
    | Info _      -> "info"
    | Create _    -> "create"
    | Destroy _   -> "destroy"
    | Recording _ -> "recording"
    | Enable _    -> "enable"
    | Disable _   -> "disable"
    | Watch _     -> "watch"
    | Start       -> "start"
    | Pause       -> "pause"
    | Stop        -> "stop"
    | Switch _    -> "switch"

  let request_to_params : type a. a request -> (string * Js.Unsafe.any) array = function
    | List        -> [| |]
    | Info x      -> [| ("id", Js.Unsafe.inject x) |]
    | Create x    -> [| ("id", Js.Unsafe.inject x) |]
    | Destroy x   -> [| ("id", Js.Unsafe.inject x) |]
    | Recording x -> [| ("id", Js.Unsafe.inject x) |]
    | Enable x    -> [| ("id", Js.Unsafe.inject x) |]
    | Disable x   -> [| ("id", Js.Unsafe.inject x) |]
    | Watch x     -> [| ("id", Js.Unsafe.inject x) |]
    | Start       -> [| |]
    | Pause       -> [| |]
    | Stop        -> [| |]
    | Switch x    -> [| ("id", Js.Unsafe.inject x) |]

  let make_response (type a) (response:Js.json Js.t) (request:a request) : a =
    Printf.printf "Response: %s\n" (Js.to_string (Json.output response));
    match request with
    | List -> [{ video_age_ms = 1
               ; audio_age_ms = 2
               ; id           = 1
               ; _type        = "live"
               ; description  = "Abc"
               }]
    | Info _ -> { video_age_ms = 1
                ; audio_age_ms = 2
                ; id           = 1
                ; _type        = "live"
                ; description  = "Abc"
                }
    | Create _ -> "create"
    | Destroy _   -> "destroy"
    | Recording _ -> "recording"
    | Enable _    -> "enable"
    | Disable _   -> "disable"
    | Watch _     -> "watch"
    | Start       -> ()
    | Pause       -> ()
    | Stop        -> ()
    | Switch _    -> ()

  (* Send message to plugin *)
  let send (type a) ?jsep (plugin:t) (request: a request) : a Lwt.t =
    let t,w      = Lwt.wait () in
    let request' = ("request",
                    request_to_string request |> Js.string |> Js.Unsafe.inject) in
    let params   = request_to_params request in
    let message  = ("message",
                    Js.Unsafe.inject @@ Js.Unsafe.obj @@ Array.append [| request'; |] params) in
    let jsep'    = ("jsep", Js.Optdef.option jsep |> Js.Unsafe.inject) in
    let success  = ("success",
                    Js.wrap_callback (fun (result:Js.json Js.t) -> Lwt.wakeup w result)
                    |> Js.Unsafe.inject) in
    let error    = ("error",
                    Js.wrap_callback (fun (s:Js.json Js.t) ->
                        Lwt.wakeup_exn w (Failure (Js.to_string (Json.output s))))
                    |> Js.Unsafe.inject) in
    plugin##send (Js.Unsafe.obj [| request'; message; success; error; jsep' |]);
    t >>= (fun result->
        Lwt.return (make_response result request))


  let get_resolution_string quality aspect =
    begin match quality with
      | `Lowres -> "lowres"
      | `Stdres -> "stdres"
      | `Hires -> "hires"
    end
    ^
    begin match aspect with
      | `Wide -> "-16:9"
      | `Square -> ""
    end

  (* Create WebRTC answer *)
  (* FIXME includes undefined values in the resulting obj *)
  let create_answer (plugin:t) media trickle jsep =
    let media' = ("media",
                  [ ("audioSend", wrap_js_optdef media.audio_send Js.bool);
                    ("audioRecv", wrap_js_optdef media.audio_recv Js.bool);
                    ("audio", match media.audio with
                      | Some x -> begin match x with
                          | Bool x -> Js.bool x |> Js.Unsafe.inject
                          | Device id -> Js.Unsafe.obj [| ("deviceId", Js.Unsafe.inject id) |]
                                         |> Js.Unsafe.inject
                        end
                      | None -> Js.Unsafe.inject Js.Optdef.empty);
                    ("videoSend", wrap_js_optdef media.video_send Js.bool);
                    ("videoRecv", wrap_js_optdef media.video_recv Js.bool);
                    ("video", match media.video with
                      | Some x -> begin match x with
                          | Bool x -> Js.bool x |> Js.Unsafe.inject
                          | Resolution (quality, aspect) ->
                            Js.string (get_resolution_string quality aspect) |> Js.Unsafe.inject
                          | Screen -> Js.string "screen" |> Js.Unsafe.inject
                          | Device (id,w,h) -> Js.Unsafe.obj [| ("deviceId", Js.Unsafe.inject id);
                                                                ("width", Js.Unsafe.inject w);
                                                                ("height", Js.Unsafe.inject h) |]
                                               |> Js.Unsafe.inject
                        end
                      | None -> Js.Unsafe.inject Js.Optdef.empty);
                    ("data", wrap_js_optdef media.data Js.bool);
                    ("failIfNoVideo", wrap_js_optdef media.fail_if_no_video Js.bool);
                    ("failIfNoAudio", wrap_js_optdef media.fail_if_no_audio Js.bool);
                    ("screenShareFrameRate", wrap_js_optdef media.screen_rate (fun x -> x)) ]
                  |> Array.of_list
                  |> Js.Unsafe.obj |> Js.Unsafe.inject ) in
    let trickle' = ("trickle", wrap_js_optdef trickle Js.bool) in
    let jsep' = ("jsep", Js.Unsafe.inject jsep) in
    let success = ("success", Js.wrap_callback (fun jsep'' ->
        Printf.printf "Success answer!\n";
        send ~jsep:jsep'' plugin Start) |> Js.Unsafe.inject) in
    let error = ("error", Js.wrap_callback (fun _ -> ()) |> Js.Unsafe.inject) in
    plugin##createAnswer (Js.Unsafe.obj [| media'; trickle'; jsep';
                                           success; error|])

end

(* Janus instance *)
type janus = Janus.janus Js.t

(* Available Janus debuggers *)
type debug_token = Trace
                 | Debug
                 | Log
                 | Warn
                 | Error

type create_result = { error   : string Lwt.t
                     ; success : janus Lwt.t
                     ; destroy : unit Lwt.t
                     }

type attach_result = { error : string Lwt.t
                     ; success : Plugin.t Lwt.t
                     }

type session_props = { server : [`One of string | `Many of string list]
                     ; ice_servers : string list option
                     ; ipv6 : bool option
                     ; with_credentials : bool option
                     ; max_poll_events : int option
                     ; destroy_on_unload : bool option
                     ; token : string option
                     ; apisecret : string option
                     }

type 'a react_push = ('a -> unit)

type plugin_props = { name : string
                    ; opaque_id : string option
                    ; consent_dialog : bool react_push option
                    ; webrtc_state : bool react_push option
                    ; ice_state : string react_push option
                    ; media_state : (string * bool) react_push option
                    ; slow_link : bool react_push option
                    ; on_cleanup : unit react_push option
                    ; detached : unit react_push option
                    }
let debug_token_to_string = function
  | Trace -> "trace" | Debug -> "debug" | Log -> "log"
  | Warn  -> "warn"  | Error -> "error"

(* Initialize Janus *)
let init debug =
  let thread, wakener = Lwt.wait () in
  let debug' = ("debug",
                match debug with
                | `All x -> Js.bool x |> Js.Unsafe.inject
                | `Several x -> (List.map (fun l -> debug_token_to_string l |> Js.string) x
                                 |> Array.of_list
                                 |> Js.array
                                 |> Js.Unsafe.inject)) in
  let cb = ("callback", (fun () -> (if (Janus.isWebrtcSupported ())
                                    then Lwt.wakeup wakener ()
                                    else Lwt.wakeup_exn wakener (Failure "WebRTC is not supported")))
                        |> Js.wrap_callback
                        |> Js.Unsafe.inject) in
  Janus.init [| debug'; cb |];
  thread

(* Create Janus session *)
let create props =
  let ok_t, ok_w = Lwt.wait () in
  let err_t, err_w = Lwt.wait () in
  let destr_t, destr_w = Lwt.wait () in
  let server = ("server", (match props.server with
      | `One x -> Js.string x |> Js.Unsafe.inject
      | `Many x -> (List.map Js.string x
                    |> Array.of_list
                    |> Js.array
                    |> Js.Unsafe.inject))) in
  let ice_servers       = ("iceServers", wrap_js_optdef props.ice_servers (fun l -> List.map Js.string l
                                                                                    |> Array.of_list)) in
  let ipv6              = ("ipv6", wrap_js_optdef props.ipv6 Js.bool) in
  let with_credentials  = ("withCredentials", wrap_js_optdef props.with_credentials Js.bool) in
  let max_poll_events   = ("max_poll_events", wrap_js_optdef props.max_poll_events (fun x -> x)) in
  let destroy_on_unload = ("destroyOnUnload", wrap_js_optdef props.destroy_on_unload Js.bool) in
  let token             = ("token", wrap_js_optdef props.token Js.string) in
  let apisecret         = ("apisecret", wrap_js_optdef props.apisecret Js.string) in
  let success           = ("success", Js.wrap_callback (fun () -> Lwt.wakeup ok_w ())
                                      |> Js.Unsafe.inject) in
  let error             = ("error", Js.wrap_callback (fun s -> Lwt.wakeup err_w (Js.to_string s))
                                    |> Js.Unsafe.inject) in
  let destroy           = ("destroy", Js.wrap_callback (fun () -> Lwt.wakeup destr_w ())
                                      |> Js.Unsafe.inject) in

  let j = Janus.create [| server; ice_servers; ipv6; with_credentials;
                          max_poll_events; destroy_on_unload; token; apisecret;
                          success; error; destroy |] in

  { success = ok_t >>= (fun () -> Lwt.return j)
  ; error   = err_t
  ; destroy = destr_t
  }

let handle_message (handle,msg,jsep) =
  Printf.printf "Got a message on plugin: %s\n" @@ Plugin.get_name handle;
  Printf.printf "Got a message: %s\n" (Js.to_string @@ Json.output msg); (* Ignore message for now FIXME*)
  let open CCOpt in
  Js.Optdef.to_option jsep
  >>= Js.Opt.to_option
  >>= fun jsep' -> (Printf.printf "Got a jsep: %s\n" (Js.to_string @@ Json.output jsep');
                    return
                    @@ Plugin.create_answer
                      handle
                      { audio_send = Some false
                      ; audio_recv = None
                      ; audio = None
                      ; video_send = Some false
                      ; video_recv = None
                      ; video = None
                      ; data = None
                      ; fail_if_no_video = None
                      ; fail_if_no_audio = None
                      ; screen_rate = None
                      }
                      None
                      jsep')
  
(* Attach plugin to Janus instance *)
let attach session props =
  let ok_t, ok_w = Lwt.wait () in
  let err_t, err_w = Lwt.wait () in
  let e, push = Lwt_react.E.create () in
  let _ = Lwt_react.E.map handle_message e in
  let wrap_cb s push = (s, (Js.Optdef.option push
                            >|= (fun push -> Js.wrap_callback (fun data -> push data)))
                           |> Js.Unsafe.inject) in
  let name           = ("plugin", Js.string props.name |> Js.Unsafe.inject) in
  let opaque_id      = ("opaqueId", wrap_js_optdef props.opaque_id Js.string) in
  let success        = ("success", Js.wrap_callback (fun plugin -> Lwt.wakeup ok_w plugin)
                                   |> Js.Unsafe.inject) in
  let error          = ("error", Js.wrap_callback (fun s -> Lwt.wakeup err_w (Js.to_string s))
                                 |> Js.Unsafe.inject) in
  let consent_dialog = wrap_cb "consentDialog" props.consent_dialog in
  let webrtc_state   = wrap_cb "webrtcState" props.webrtc_state in
  let ice_state      = wrap_cb "iceState" props.ice_state in
  let media_state    = wrap_cb "mediaState" props.media_state in
  let slow_link      = wrap_cb "slowLink" props.slow_link in
  let on_cleanup     = wrap_cb "oncleanup" props.on_cleanup in
  let detached       = wrap_cb "detached" props.detached in
  let on_remote_stream = ("onremotestream",
                          (Js.wrap_callback (fun stream -> Janus.attachMediaStream "remotevideo" stream))
                          |> Js.Unsafe.inject) in
  let on_message     = ("onmessage",
                        (Js.wrap_callback (fun m j -> ok_t >>= (fun handle -> Lwt.return @@ push (handle,m,j))))
                        |> Js.Unsafe.inject) in

  session##attach (Js.Unsafe.obj [| name; opaque_id; success; error; on_message;
                                    consent_dialog; webrtc_state; ice_state;
                                    media_state; slow_link; on_cleanup;
                                    detached; on_remote_stream |]);

  { success = ok_t >>= (fun plugin -> Plugin.send plugin (Watch 1) |> ignore;
                         Lwt.return plugin)
  ; error   = err_t
  }
