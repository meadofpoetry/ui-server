let (>|=) x f = Js.Optdef.map x f
let (>>=) = Lwt.(>>=)

let int_of_number x = int_of_float @@ Js.float_of_number x
let wrap_js_optdef x f = Js.Optdef.option x >|= f |> Js.Unsafe.inject
let bind_undef_or_null x f =
  let open CCOpt in
  Js.Optdef.to_option x
  >>= Js.Opt.to_option
  >>= f

(** Janus plugin handler **)
module Plugin = struct

  (* Types *)

  type t = Janus.plugin Js.t

  type plugin_type = Audiobridge
                   | Echotest
                   | Recordplay
                   | Sip
                   | Streaming
                   | Textroom
                   | Videocall
                   | Videoroom
                   | Voicemail

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

  type 'a send_response = Error of int option * string
                        | Empty
                        | Data of 'a

  (* Helper functions *)

  let plugin_type_to_string type_ =
    let name = match type_ with
      | Audiobridge -> "audiobridge" | Echotest  -> "echotest"  | Recordplay -> "recordplay"
      | Sip         -> "sip"         | Streaming -> "streaming" | Textroom   -> "textroom"
      | Videocall   -> "videocall"   | Videoroom -> "videoroom" | Voicemail  -> "voicemail" in
    "janus.plugin." ^ name

  let get_resolution_string quality aspect =
    let q = match quality with
      | `Lowres -> "lowres"
      | `Stdres -> "stdres"
      | `Hires -> "hires" in
    let a = match aspect with
      | `Wide -> "-16:9"
      | `Square -> "" in
    q ^ a

  let parse_sync_response name f = function
    | Empty         -> Result.Error "Empty message"
    | Data d        -> (let prop = Js.Unsafe.get d name in
                        Js.Optdef.bind prop (fun x -> Js.Unsafe.get d x)
                        |> Js.Optdef.to_option
                        |> (function
                            | Some x -> f x
                            | None   -> Result.Error "Bad response"))
    | Error (_,msg) -> Result.Error msg

  let parse_async_response = function
    | Empty         -> Ok ()
    | Error (_,msg) -> Result.Error msg (* FIXME add error code to error string? *)
    | Data _        -> Result.Error "Data returned in async request"

  let data_or_error response =
    let response = Js.Optdef.to_option response in
    match response with
    | Some response ->
      if (Js.Optdef.test ((Js.Unsafe.coerce response)##.error_code)) ||
         (Js.Optdef.test ((Js.Unsafe.coerce response)##.error))
      then
        let error = match Js.Optdef.to_option (Js.Unsafe.coerce response)##.error with
          | Some x -> Js.to_string x
          | None   -> "" in
        let code  = match Js.Optdef.to_option (Js.Unsafe.coerce response)##.error_code with
          | Some x -> Some (int_of_number x)
          | None   -> None in
        Error (code, error)
      else Data response
    | None -> Empty

  (* Plugin functions *)

  let get_id plugin   = plugin##getId () |> Js.float_of_number |> Int64.of_float

  let get_name plugin = plugin##getPlugin () |> Js.to_string

  let send ?jsep (plugin:t) request request_to_string request_to_params parse_response =
    let open Js.Unsafe in
    let t,w      = Lwt.wait () in
    let request' = ("request", request_to_string request |> Js.string |> inject) in
    let params   = request_to_params request in
    let message  = ("message", inject @@ obj @@ Array.append [| request' |] params) in
    let jsep'    = ("jsep", inject @@ Js.Optdef.option jsep) in
    let success  = ("success", inject @@ Js.wrap_callback (fun resp -> Lwt.wakeup w resp)) in
    let error    = ("error", inject @@ Js.wrap_callback (fun err ->
        Lwt.wakeup_exn w (Failure (Js.to_string (Json.output err))))) in
    plugin##send (obj [| request'; message; success; error; jsep' |]);
    Lwt.catch
      (fun () -> t >>= (fun response ->
           Lwt.return @@ parse_response response request))
      (function
        | Failure e -> Lwt.return @@ Result.Error e
        | e -> Lwt.return @@ Result.Error (Printexc.to_string e))

  (* FIXME includes undefined values in the resulting obj *)
  (* FIXME maybe make sending of Start message optional? *)
  (* FIXME return value *)
  let create_answer (plugin:t) media trickle jsep =
    let open Js.Unsafe in
    let media' = ("media",
                  [ ("audioSend", wrap_js_optdef media.audio_send Js.bool);
                    ("audioRecv", wrap_js_optdef media.audio_recv Js.bool);
                    ("audio", match media.audio with
                      | Some x -> begin match x with
                          | Bool x -> Js.bool x |> inject
                          | Device id -> obj [| ("deviceId", Js.Unsafe.inject id) |] |> inject
                        end
                      | None -> inject Js.Optdef.empty);
                    ("videoSend", wrap_js_optdef media.video_send Js.bool);
                    ("videoRecv", wrap_js_optdef media.video_recv Js.bool);
                    ("video", match media.video with
                      | Some x -> begin match x with
                          | Bool x -> Js.bool x |> inject
                          | Resolution (quality, aspect) ->
                            get_resolution_string quality aspect |> Js.string |> inject
                          | Screen -> Js.string "screen" |> Js.Unsafe.inject
                          | Device (id,w,h) -> obj [| ("deviceId", inject id);
                                                      ("width", inject w);
                                                      ("height", inject h) |] |> inject
                        end
                      | None -> Js.Unsafe.inject Js.Optdef.empty);
                    ("data", wrap_js_optdef media.data Js.bool);
                    ("failIfNoVideo", wrap_js_optdef media.fail_if_no_video Js.bool);
                    ("failIfNoAudio", wrap_js_optdef media.fail_if_no_audio Js.bool);
                    ("screenShareFrameRate", wrap_js_optdef media.screen_rate (fun x -> x)) ]
                  |> Array.of_list |> obj |> inject ) in
    let trickle' = ("trickle", wrap_js_optdef trickle Js.bool) in
    let jsep' = ("jsep", inject jsep) in
    let success = ("success", Js.wrap_callback (fun _ -> () (* send ~jsep:jsep'' plugin Start *)) |> inject) in
    let error = ("error", Js.wrap_callback (fun _ -> ()) |> inject) in (* FIXME handle error properly *)
    plugin##createAnswer (obj [| media'; trickle'; jsep'; success; error|])

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

type plugin_props = { name : Plugin.plugin_type
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
  let open Js.Unsafe in
  let thread, wakener = Lwt.wait () in
  let debug' = ("debug",
                match debug with
                | `All x -> Js.bool x |> inject
                | `Several x -> (List.map (fun l -> debug_token_to_string l |> Js.string) x
                                 |> Array.of_list |> Js.array |> inject)) in
  let cb     = ("callback", (fun () -> (if Janus.isWebrtcSupported ()
                                        then Lwt.wakeup wakener ()
                                        else Lwt.wakeup_exn wakener (Failure "WebRTC is not supported")))
                            |> Js.wrap_callback |> inject) in
  Janus.init [| debug'; cb |];
  thread

(* Create Janus session *)
let create props =
  let open Js.Unsafe in
  let ok_t, ok_w = Lwt.wait () in
  let err_t, err_w = Lwt.wait () in
  let destr_t, destr_w = Lwt.wait () in
  let server = ("server", (match props.server with
      | `One x -> Js.string x |> inject
      | `Many x -> (List.map Js.string x |> Array.of_list |> Js.array |> inject))) in
  let ice_servers       = ("iceServers", wrap_js_optdef props.ice_servers
                             (fun l -> List.map Js.string l |> Array.of_list)) in
  let ipv6              = ("ipv6", wrap_js_optdef props.ipv6 Js.bool) in
  let with_credentials  = ("withCredentials", wrap_js_optdef props.with_credentials Js.bool) in
  let max_poll_events   = ("max_poll_events", wrap_js_optdef props.max_poll_events (fun x -> x)) in
  let destroy_on_unload = ("destroyOnUnload", wrap_js_optdef props.destroy_on_unload Js.bool) in
  let token             = ("token", wrap_js_optdef props.token Js.string) in
  let apisecret         = ("apisecret", wrap_js_optdef props.apisecret Js.string) in
  let success           = ("success", Js.wrap_callback (fun () -> Lwt.wakeup ok_w ()) |> inject) in
  let error             = ("error", Js.wrap_callback (fun s -> Lwt.wakeup err_w (Js.to_string s)) |> inject) in
  let destroy           = ("destroy", Js.wrap_callback (fun () -> Lwt.wakeup destr_w ()) |> inject) in

  let j = Janus.create [| server; ice_servers; ipv6; with_credentials;
                          max_poll_events; destroy_on_unload; token; apisecret;
                          success; error; destroy |] in

  { success = ok_t >>= (fun () -> Lwt.return j)
  ; error   = err_t
  ; destroy = destr_t
  }

let handle_message (handle,msg,jsep) =
  Printf.printf "Got a message: %s\n" (Js.to_string @@ Json.output msg); (* Ignore message for now FIXME*)
  (* Handle jsep if some *)
  bind_undef_or_null jsep (fun jsep' -> (CCOpt.return
                                         @@ Plugin.create_answer
                                           handle
                                           { audio_send       = Some false
                                           ; audio_recv       = None
                                           ; audio            = None
                                           ; video_send       = Some false
                                           ; video_recv       = None
                                           ; video            = None
                                           ; data             = None
                                           ; fail_if_no_video = None
                                           ; fail_if_no_audio = None
                                           ; screen_rate      = None
                                           }
                                           None
                                           jsep'))

(* Attach plugin to Janus instance *)
let attach session props =
  let ok_t, ok_w = Lwt.wait () in
  let err_t, err_w = Lwt.wait () in
  let e, push = Lwt_react.E.create () in
  let _ = Lwt_react.E.map handle_message e in

  let open Js.Unsafe in
  let wrap_cb name push = (name, wrap_js_optdef push (fun push -> Js.wrap_callback (fun data -> push data))) in
  let name              = ("plugin", Plugin.plugin_type_to_string props.name|> Js.string |> inject) in
  let opaque_id         = ("opaqueId", wrap_js_optdef props.opaque_id Js.string) in
  let success           = ("success", Js.wrap_callback (fun plugin -> Lwt.wakeup ok_w plugin) |> inject) in
  let error             = ("error", Js.wrap_callback (fun s -> Lwt.wakeup err_w (Js.to_string s)) |> inject) in

  let consent_dialog    = wrap_cb "consentDialog" props.consent_dialog in
  let webrtc_state      = wrap_cb "webrtcState" props.webrtc_state in
  let ice_state         = wrap_cb "iceState" props.ice_state in
  let media_state       = wrap_cb "mediaState" props.media_state in
  let slow_link         = wrap_cb "slowLink" props.slow_link in
  let on_cleanup        = wrap_cb "oncleanup" props.on_cleanup in
  let detached          = wrap_cb "detached" props.detached in

  let on_remote_stream  = ("onremotestream",
                           (Js.wrap_callback (fun stream -> Janus.attachMediaStream "remotevideo" stream))
                           |> inject) in
  let on_message        = ("onmessage",
                           (Js.wrap_callback (fun m j -> ok_t >>= (fun handle -> Lwt.return @@ push (handle,m,j))))
                           |> inject) in

  session##attach (obj [| name; opaque_id; success; error; on_message;
                          consent_dialog; webrtc_state; ice_state;
                          media_state; slow_link; on_cleanup;
                          detached; on_remote_stream |]);
  { success = ok_t; error = err_t }
