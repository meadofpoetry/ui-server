let (>|=) x f = Js.Optdef.map x f
let int_of_number x = int_of_float @@ Js.float_of_number x
let wrap_js_optdef x f = Js.Optdef.option x >|= f |> Js.Unsafe.inject

let parse_ok_response ok =
  if String.equal (Js.to_string ok) "ok"
  then Ok ()
  else Error "Bad ok response"

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

module Mp_list = struct

  type t = unit

  type r = { id           : int option
           ; type_        : string option
           ; description  : string option
           ; video_age_ms : int option
           ; audio_age_ms : int option
           }

  let to_js_obj _ = [| |]

  let of_js_obj o =
    Array.map (fun el ->
        let el = Js.Unsafe.coerce el in
        { id           = Js.Optdef.to_option el##.id           |> CCOpt.map int_of_number
        ; type_        = Js.Optdef.to_option el##.type_        |> CCOpt.map Js.to_string
        ; description  = Js.Optdef.to_option el##.description  |> CCOpt.map Js.to_string
        ; video_age_ms = Js.Optdef.to_option el##.video_age_ms |> CCOpt.map int_of_number
        ; audio_age_ms = Js.Optdef.to_option el##.audio_age_ms |> CCOpt.map int_of_number
        }) o
    |> Array.to_list
    |> (fun x -> Ok x)

end

module Mp_info = struct

  type t = int

  type r = { id           : int option
           ; type_        : string option
           ; description  : string option
           ; video_age_ms : int option
           ; audio_age_ms : int option
           ; data_age_ms  : int option
           ; video        : string option
           ; audio        : string option
           ; data         : string option
           }

  let to_js_obj (id:int) = [| ("id", Js.Unsafe.inject id) |]

  let of_js_obj o =
    let o = Js.Unsafe.coerce o in
    Ok { id           = Js.Optdef.to_option o##.id           |> CCOpt.map int_of_number
       ; type_        = Js.Optdef.to_option o##.type_        |> CCOpt.map Js.to_string
       ; description  = Js.Optdef.to_option o##.description  |> CCOpt.map Js.to_string
       ; video_age_ms = Js.Optdef.to_option o##.video_age_ms |> CCOpt.map int_of_number
       ; audio_age_ms = Js.Optdef.to_option o##.audio_age_ms |> CCOpt.map int_of_number
       ; data_age_ms  = Js.Optdef.to_option o##.data_age_msg  |> CCOpt.map int_of_number
       ; video        = Js.Optdef.to_option o##.video         |> CCOpt.map Js.to_string
       ; audio        = Js.Optdef.to_option o##.audio         |> CCOpt.map Js.to_string
       ; data         = Js.Optdef.to_option o##.data          |> CCOpt.map Js.to_string
       }

end

module Mp_create = struct

  type mp_type = Rtp of Mp_rtp.t
               | Live of Mp_live.t
               | Ondemand of Mp_ondemand.t
               | Rtsp of Mp_rtsp.t

  type t = { type_     : mp_type
           ; admin_key : string option
           ; secret    : string option
           ; pin       : string option
           ; permanent : bool option
           }

  type r = { id          : int option
           ; type_       : string option
           ; description : string option
           ; is_private  : bool option
           ; audio_port  : int option
           ; video_port  : int option
           ; data_port   : int option
           }

  let type_to_string = function
    | Rtp _ -> "rtp" | Live _ -> "live" | Ondemand _ -> "ondemand" | Rtsp _ -> "rtsp"

  let to_js_obj (x:t) =
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

  let of_js_obj o =
    let o = Js.Unsafe.coerce o in
    Ok { id          = Js.Optdef.to_option o##.id          |> CCOpt.map int_of_number
       ; type_       = Js.Optdef.to_option o##.type_       |> CCOpt.map Js.to_string
       ; description = Js.Optdef.to_option o##.description |> CCOpt.map Js.to_string
       ; is_private  = Js.Optdef.to_option o##.is_private  |> CCOpt.map Js.to_bool
       ; audio_port  = Js.Optdef.to_option o##.audio_port  |> CCOpt.map int_of_number
       ; video_port  = Js.Optdef.to_option o##.video_port  |> CCOpt.map int_of_number
       ; data_port   = Js.Optdef.to_option o##.data_port   |> CCOpt.map int_of_number
       }

end

module Mp_destroy = struct

  type t =
    { id        : int
    ; secret    : string option
    ; permanent : bool option
    }

  type r = int

  let to_js_obj (x:t) =
    let id        = ("id", Js.Unsafe.inject x.id) in
    let secret    = ("secret", wrap_js_optdef x.secret Js.string) in
    let permanent = ("permanent", wrap_js_optdef x.permanent Js.bool) in
    [| id; secret; permanent |]

  let of_js_obj o = Ok (int_of_number o)

end

module Mp_recording = struct

  type recording_action =
    | Start of (string option * string option * string option) (* filenames *)
    | Stop of (bool option * bool option * bool option) (* flags *)

  type t =
    { id     : int
    ; action : recording_action
    ; secret : string option
    }

  type r = unit

  let action_to_string = function
    | Start _ -> "start" | Stop _ -> "stop"

  let to_js_obj (x:t) =
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

  let of_js_obj o = parse_ok_response o

end

module Mp_enable = struct

  type t = { id     : int
           ; secret : string option
           }

  type r = unit

  let to_js_obj (x:t) = [| ("id", Js.Unsafe.inject x.id);
                           ("secret", wrap_js_optdef x.secret Js.string) |]
  let of_js_obj o = parse_ok_response o

end

module Mp_disable = struct

  include Mp_enable

end

module Mp_watch = struct

  type t = { id     : int
           ; secret : string option
           }

  let to_js_obj (x:t) = [| ("id", Js.Unsafe.inject x.id);
                           ("secret", wrap_js_optdef x.secret Js.string) |]

end

module Mp_start = struct

  type t = unit

  let to_js_obj _ = [| |]

end

module Mp_pause = struct

  include Mp_start

end

module Mp_stop = struct

  include Mp_start

end

module Mp_switch = struct

  type t = int

  let to_js_obj (id:int) = [| ("id", Js.Unsafe.inject id) |]

end

type _ request =
  | List      : Mp_list.r list request
  | Info      : Mp_info.t      -> Mp_info.r request
  | Create    : Mp_create.t    -> Mp_create.r request
  | Destroy   : Mp_destroy.t   -> Mp_destroy.r request
  | Recording : Mp_recording.t -> Mp_recording.r request
  | Enable    : Mp_enable.t    -> Mp_enable.r request
  | Disable   : Mp_disable.t   -> Mp_disable.r request
  | Watch     : Mp_watch.t     -> unit request
  | Start     : unit request
  | Pause     : unit request
  | Stop      : unit request
  | Switch    : Mp_switch.t    -> unit request

let request_to_string : type a. a request -> string = function
  | List        -> "list"    | Info _      -> "info"      | Create _    -> "create"
  | Destroy _   -> "destroy" | Recording _ -> "recording" | Enable _    -> "enable"
  | Disable _   -> "disable" | Watch _     -> "watch"     | Start       -> "start"
  | Pause       -> "pause"   | Stop        -> "stop"      | Switch _    -> "switch"

let request_to_params : type a. a request -> (string * Js.Unsafe.any) array = function
  | List                -> Mp_list.to_js_obj ()
  | Info x              -> Mp_list.to_js_obj x
  | Create x            -> Mp_create.to_js_obj x
  | Destroy x           -> Mp_destroy.to_js_obj x
  | Recording x         -> Mp_recording.to_js_obj x
  | Enable x            -> Mp_enable.to_js_obj x
  | Disable x           -> Mp_disable.to_js_obj x
  | Watch x             -> Mp_watch.to_js_obj x
  | Start               -> Mp_start.to_js_obj ()
  | Pause               -> Mp_pause.to_js_obj ()
  | Stop                -> Mp_stop.to_js_obj ()
  | Switch x            -> Mp_switch.to_js_obj x

let parse_response (type a) response (request:a request) : (a,string) Result.result =
  let open Janus_static.Plugin in
  let plugin_name = "streaming" in
  let typed_response = data_or_error response in
  begin match request with
    (* sync responses *)
    | List        -> parse_sync_response plugin_name Mp_list.of_js_obj typed_response
    | Info _      -> parse_sync_response plugin_name Mp_info.of_js_obj typed_response
    | Create _    -> parse_sync_response plugin_name Mp_create.of_js_obj typed_response
    | Destroy _   -> parse_sync_response plugin_name Mp_destroy.of_js_obj typed_response
    | Recording _ -> parse_sync_response plugin_name parse_ok_response typed_response
    | Enable _    -> parse_sync_response plugin_name parse_ok_response typed_response
    | Disable _   -> parse_sync_response plugin_name parse_ok_response typed_response
    (* async responses *)
    | Watch _     -> parse_async_response typed_response
    | Start       -> parse_async_response typed_response
    | Pause       -> parse_async_response typed_response
    | Stop        -> parse_async_response typed_response
    | Switch _    -> parse_async_response typed_response
  end

let send ?jsep plugin request =
  Janus_static.Plugin.send
    ?jsep
    plugin
    request
    request_to_string
    request_to_params
    parse_response
