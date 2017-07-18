let (>|=) x f = Js.Optdef.map x f
let (>>=) = Lwt.(>>=)

let wrap_js_optdef x f = Js.Optdef.option x >|= f |> Js.Unsafe.inject
let bind_undef_or_null x f =
  let open CCOpt in
  Js.Optdef.to_option x
  >>= Js.Opt.to_option
  >>= f

module Mp_base = struct

  type t =
    { id          : int option
    ; name        : string option
    ; description : string option
    ; is_private  : bool option
    ; audio       : bool option
    ; video       : bool option
    ; data        : bool option
    }

  let to_js_obj x =
    let id          = ("id", wrap_js_optdef x.id (fun x -> x)) in
    let name        = ("name", wrap_js_optdef x.name Js.string) in
    let description = ("description", wrap_js_optdef x.description Js.string) in
    let is_private  = ("is_private", wrap_js_optdef x.is_private Js.bool) in
    let audio       = ("audio", wrap_js_optdef x.audio Js.bool) in
    let video       = ("video", wrap_js_optdef x.video Js.bool) in
    let data        = ("data", wrap_js_optdef x.data Js.bool) in
    [| id; name; description; is_private; audio; video; data |]

end

(* Parameters for rtp mountpoint*)
module Mp_rtp = struct

  type audio =
    { audiomcast  : string option
    ; audioport   : int
    ; audiopt     : int
    ; audiortpmap : string
    ; audiofmtp   : string option
    ; audioiface  : string option
    }

  type video =
    { videomcast    : string option
    ; videoport     : int
    ; videopt       : int
    ; videortpmap   : string
    ; videofmtp     : string option
    ; videoiface    : string option
    ; videobufferkf : bool option
    }

  type data =
    { dataport      : int
    ; databuffermsg : bool option
    ; dataiface     : string option
    }

  type t = { base  : Mp_base.t option
           ; audio : audio option
           ; video : video option
           ; data  : data option
           }

  let to_js_obj x =
    let base  = match x.base with
      | Some x -> Mp_base.to_js_obj x
      | None   -> [| |] in
    let audio = match x.audio with
      | Some aud -> [| ("audiomcast", wrap_js_optdef aud.audiomcast Js.string);
                       ("audioport", Js.Unsafe.inject aud.audioport);
                       ("audiopt", Js.Unsafe.inject @@ (fun x -> x) aud.audiopt);
                       ("audiortpmap", Js.Unsafe.inject @@ Js.string aud.audiortpmap);
                       ("audiofmtp", wrap_js_optdef aud.audiofmtp Js.string);
                       ("audioiface", wrap_js_optdef aud.audioiface Js.string) |]
      | None -> [| |] in
    let video = match x.video with
      | Some vid -> [| ("videomcast", wrap_js_optdef vid.videomcast Js.string);
                       ("videoport", Js.Unsafe.inject vid.videoport);
                       ("videopt", Js.Unsafe.inject @@ (fun x -> x) vid.videopt);
                       ("videortpmap", Js.Unsafe.inject @@ Js.string vid.videortpmap);
                       ("videofmtp", wrap_js_optdef vid.videofmtp Js.string);
                       ("videoiface", wrap_js_optdef vid.videoiface Js.string);
                       ("videobufferkf", wrap_js_optdef vid.videobufferkf Js.bool) |]
      | None -> [| |] in
    let data = match x.data with
      | Some data -> [| ("dataport", Js.Unsafe.inject data.dataport);
                        ("databuffermsg", wrap_js_optdef data.databuffermsg Js.bool);
                        ("dataiface", wrap_js_optdef data.dataiface Js.string) |]
      | None -> [| |] in
    Array.concat [ base; audio; video; data ]

end

(* Parameters for live mountpoint *)
module Mp_live = struct

  type t =
    { base : Mp_base.t option
    ; filename : string
    }

  let to_js_obj x =
    let base     = match x.base with
      | Some x -> Mp_base.to_js_obj x
      | None   -> [| |] in
    let filename = [| ("filename", Js.Unsafe.inject @@ Js.string x.filename) |] in
    Array.append base filename

end

(* Parameters for ondemand mountpoint *)
module Mp_ondemand = struct

  type t = Mp_live.t

  let to_js_obj x = Mp_live.to_js_obj x

end

(* Parameters for rtps mountpoint *)
module Mp_rtsp = struct

  type t =
    { base      : Mp_base.t option
    ; url       : string option
    ; rtsp_user : string option
    ; rtsp_pwd  : string option
    ; rtspiface : string option
    }

  let to_js_obj x =
    let base = match x.base with
      | Some x -> Mp_base.to_js_obj x
      | None   -> [| |] in
    let specific = [| ("url", wrap_js_optdef x.url Js.string);
                      ("rtsp_user", wrap_js_optdef x.rtsp_user Js.string);
                      ("rtsp_pwd", wrap_js_optdef x.rtsp_pwd Js.string);
                      ("rtspiface", wrap_js_optdef x.rtspiface Js.string) |] in
    Array.append base specific

end

(* Parameters to start recording from a mountpoint *)
module Mp_recording = struct

  type recording_action =
    | Start of (string option * string option * string option) (* filenames *)
    | Stop of (bool option * bool option * bool option) (* flags *)

  type t =
    { id     : int
    ; action : recording_action
    ; secret : string option
    }

  let action_to_string = function
    | Start _ -> "start" | Stop _ -> "stop"

  let to_js_obj x =
    let id     = ("id", Js.Unsafe.inject x.id) in
    let action = ("action", Js.Unsafe.inject @@ Js.string @@ action_to_string x.action) in
    let secret = ("secret", wrap_js_optdef x.secret Js.string) in
    let params = match x.action with
      | Start (a,v,d) -> [| ("audio", wrap_js_optdef a Js.string);
                            ("video", wrap_js_optdef v Js.string);
                            ("data", wrap_js_optdef d Js.string) |]
      | Stop (a,v,d)  -> [| ("audio", wrap_js_optdef a Js.bool);
                            ("video", wrap_js_optdef v Js.bool);
                            ("data", wrap_js_optdef d Js.bool) |] in
    Array.append [| id; action; secret |] params

end

module Mp_create = struct

  type mp_type = Rtp of Mp_rtp.t
               | Live of Mp_live.t
               | Ondemand of Mp_ondemand.t
               | Rtsp of Mp_rtsp.t

  type t =
    { type_     : mp_type
    ; admin_key : string option
    ; secret    : string option
    ; pin       : string option
    ; permanent : bool option
    }

  let type_to_string = function
    | Rtp _ -> "rtp" | Live _ -> "live" | Ondemand _ -> "ondemand" | Rtsp _ -> "rtsp"

  let to_js_obj x =
    let type_    = match x.type_ with
      | Rtp rtp           -> Mp_rtp.to_js_obj rtp
      | Live live         -> Mp_live.to_js_obj live
      | Ondemand ondemand -> Mp_ondemand.to_js_obj ondemand
      | Rtsp rtsp         -> Mp_rtsp.to_js_obj rtsp in
    let specific = [| ("type", Js.Unsafe.inject @@ Js.string @@ type_to_string x.type_);
                      ("admin_key", wrap_js_optdef x.admin_key Js.string);
                      ("secret", wrap_js_optdef x.secret Js.string);
                      ("pin", wrap_js_optdef x.pin Js.string);
                      ("permanent", wrap_js_optdef x.permanent Js.bool) |] in
    Array.append type_ specific

end

(* Parameters to destroy a mountpoint *)
module Mp_destroy = struct

  type t =
    { id        : int
    ; secret    : string option
    ; permanent : bool option
    }

  let to_js_obj x =
    let id        = ("id", Js.Unsafe.inject x.id) in
    let secret    = ("secret", wrap_js_optdef x.secret Js.string) in
    let permanent = ("permanent", wrap_js_optdef x.permanent Js.bool) in
    [| id; secret; permanent |]

end

(** Janus plugin handler **)
module Plugin = struct

  type t = Janus.plugin Js.t

  type mp_info = { video_age_ms : int option
                 ; audio_age_ms : int option
                 ; id           : int option
                 ; type_        : string option
                 ; description  : string option
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

  type _ request =
    (* sync requests, have response *)
    | List      : mp_info list request
    | Info      : int                   -> mp_info request
    | Create    : Mp_create.t           -> string request
    | Destroy   : Mp_destroy.t          -> string request
    | Recording : Mp_recording.t        -> string request
    | Enable    : (int * string option) -> string request
    | Disable   : (int * string option) -> string request
    (* async requests, have no response *)
    | Watch     : (int * string option) -> unit request
    | Start     : unit request
    | Pause     : unit request
    | Stop      : unit request
    | Switch    : int                   -> unit request

  let default_streaming_media_props = { audio_send       = Some false
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

  let get_id plugin   = plugin##getId () |> Js.float_of_number |> Int64.of_float

  let get_name plugin = plugin##getPlugin () |> Js.to_string

  let request_to_string : type a. a request -> string = function
    | List        -> "list"    | Info _      -> "info"      | Create _    -> "create"
    | Destroy _   -> "destroy" | Recording _ -> "recording" | Enable _    -> "enable"
    | Disable _   -> "disable" | Watch _     -> "watch"     | Start       -> "start"
    | Pause       -> "pause"   | Stop        -> "stop"      | Switch _    -> "switch"

  let request_to_params : type a. a request -> (string * Js.Unsafe.any) array = function
    | List                -> [| |]
    | Info x              -> [| ("id", Js.Unsafe.inject x) |]
    | Create x            -> Mp_create.to_js_obj x
    | Destroy x           -> Mp_destroy.to_js_obj x
    | Recording x         -> Mp_recording.to_js_obj x
    | Enable (id,secret)  -> [| ("id", Js.Unsafe.inject id);
                                ("secret", wrap_js_optdef secret Js.string) |]
    | Disable (id,secret) -> [| ("id", Js.Unsafe.inject id);
                                ("secret", wrap_js_optdef secret Js.string) |]
    | Watch (id,secret)   -> [| ("id", Js.Unsafe.inject id);
                                ("secret", wrap_js_optdef secret Js.string)|]
    | Start               -> [| |]
    | Pause               -> [| |]
    | Stop                -> [| |]
    | Switch x            -> [| ("id", Js.Unsafe.inject x) |]

  let obj_to_mp_info obj =
    let open CCOpt in
    { id           = (bind_undef_or_null
                        obj##.id
                        (fun x -> return @@ int_of_float @@ Js.float_of_number x))
    ; video_age_ms = (bind_undef_or_null
                        obj##.video_age_ms
                        (fun x -> return @@ int_of_float @@ Js.float_of_number x))
    ; audio_age_ms = (bind_undef_or_null
                        obj##.audio_age_ms
                        (fun x -> return @@ int_of_float @@ Js.float_of_number x))
    ; type_        = (bind_undef_or_null
                        obj##.type_
                        (fun x -> return @@ Js.to_string x))
    ; description  = (bind_undef_or_null
                        obj##.description
                        (fun x -> return @@ Js.to_string x))
    }

  let parse_mp_info response = 
    CCOpt.(bind_undef_or_null response (fun resp ->
        bind_undef_or_null resp##.info
          (fun info -> return @@ obj_to_mp_info info)))

  let parse_mp_list response =
    CCOpt.(bind_undef_or_null response (fun resp ->
        bind_undef_or_null resp##.list
          (fun arr -> return @@ Array.to_list @@ Array.map obj_to_mp_info arr)))

  let make_response (type a) response (request:a request) : (a,string) Result.result =
    match request with
    | List         ->
      begin match (parse_mp_list response) with
        | Some x -> Ok x
        | None   -> Error "Unable to parse mountpoint list"
      end
    | Info _       ->
      begin match (parse_mp_info response) with
        | Some x -> Ok x
        | None   -> Error "Unable to parse mounpoint info"
      end
    | Create _    -> Ok (Js.to_string @@ Json.output response)
    | Destroy _   -> Ok (Js.to_string @@ Json.output response)
    | Recording _ -> Ok (Js.to_string @@ Json.output response)
    | Enable _    -> Ok (Js.to_string @@ Json.output response)
    | Disable _   -> Ok (Js.to_string @@ Json.output response)
    | Watch _     -> Ok ()
    | Start       -> Ok ()
    | Pause       -> Ok ()
    | Stop        -> Ok ()
    | Switch _    -> Ok ()

  (* Send message to plugin *)
  let send (type a) ?jsep (plugin:t) (request: a request) : (a,string) Result.result Lwt.t =
    let open Js.Unsafe in
    let t,w      = Lwt.wait () in
    let request' = ("request",
                    request_to_string request |> Js.string |> inject) in
    let params   = request_to_params request in
    let message  = ("message", inject @@ obj @@ Array.append [| request'; |] params) in
    let jsep'    = ("jsep", inject @@ Js.Optdef.option jsep) in
    let success  = ("success", inject @@ Js.wrap_callback (fun resp -> Lwt.wakeup w resp)) in
    let error    = ("error", inject @@ Js.wrap_callback (fun err ->
        Lwt.wakeup_exn w (Failure (Js.to_string (Json.output err))))) in
    plugin##send (obj [| request'; message; success; error; jsep' |]);
    Lwt.catch
      (fun () -> t >>= (fun result -> Lwt.return @@ make_response result request))
      (function
        | Failure e -> Lwt.return @@ Error e
        | _ -> Lwt.return @@ Error "Unknown exception")


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
    let success = ("success", Js.wrap_callback (fun jsep'' -> send ~jsep:jsep'' plugin Start) |> inject) in
    let error = ("error", Js.wrap_callback (fun _ -> ()) |> inject) in
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
                                           Plugin.default_streaming_media_props
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
  let name              = ("plugin", Js.string props.name |> inject) in
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
