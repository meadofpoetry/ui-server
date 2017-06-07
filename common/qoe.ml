open Yojson

let (<+>) a b =
  match a,b with
  | _, Some _ -> b
  | Some _, None -> a
  | _ -> None

let opt_update f a b =
  match a,b with
  | Some a, Some b -> Some (f a b)
  | Some x, None | None, Some x -> Some x
  | _ -> None

(* -------------- Channel settings  ---------------- *)

module Error_overlay = struct

  type t =
    { enabled     : bool option [@default None]
    ; error_color : int option [@default None]
    } [@@deriving yojson]

  let update a b =
    { enabled     = a.enabled <+> b.enabled
    ; error_color = a.error_color <+> b.error_color
    }

end

module Channel_name = struct

  type t =
    { enabled   : bool option [@default None]
    ; font_size : int option [@default None]
    ; fmt       : string option [@default None]
    } [@@deriving yojson]

  let update a b =
    { enabled   = a.enabled <+> b.enabled
    ; font_size = a.font_size <+> b.font_size
    ; fmt       = a.fmt <+> b.fmt
    }

end

module Audio_meter = struct

  type audio_meter_pos = Left
                       | Right
                       [@@deriving yojson]

  type t =
    { enabled  : bool option [@default None]
    ; position : audio_meter_pos option [@default None]
    } [@@deriving yojson]

  let update a b =
    { enabled  = a.enabled <+> b.enabled
    ; position = a.position <+> b.position
    }

end

module Status_bar = struct

  type status_bar_pos = Top_left
                      | Top_right
                      | Left
                      | Right
                      | Bottom_left
                      | Bottom_right
  let status_bar_pos_of_yojson = function
    | `String "top_left"     -> Ok Top_left
    | `String "top_right"    -> Ok Top_right
    | `String "left"         -> Ok Left
    | `String "right"        -> Ok Right
    | `String "bottom_left"  -> Ok Bottom_left
    | `String "bottom_right" -> Ok Bottom_right
    | err -> Error ("status_bar_pos_of_yojson: wrang data " ^ (Yojson.Safe.to_string err))
  let status_bar_pos_to_yojson = function
    | Top_left     -> `String "top_left"
    | Top_right    -> `String "top_right"
    | Left         -> `String "left"
    | Right        -> `String "right"
    | Bottom_left  -> `String "bottom_left"
    | Bottom_right -> `String "bottom_right"

  type t =
    { enabled   : bool option [@default None]
    ; position  : status_bar_pos option [@default None]
    ; aspect    : bool option [@default None]
    ; subtitles : bool option [@default None]
    ; teletext  : bool option [@default None]
    ; eit       : bool option [@default None]
    ; qos       : bool option [@default None]
    ; scte35    : bool option [@default None]
    } [@@deriving yojson]

  let update a b =
    { enabled   = a.enabled <+> b.enabled
    ; position  = a.position <+> b.position
    ; aspect    = a.aspect <+> b.aspect
    ; subtitles = a.subtitles <+> b.subtitles
    ; teletext  = a.teletext <+> b.teletext
    ; eit       = a.eit <+> b.eit
    ; qos       = a.qos <+> b.qos
    ; scte35    = a.scte35 <+> b.scte35
    }

end

module Channel_settings = struct

  type t =
    { show_border         : bool option [@default None]
    ; border_color        : int option [@default None]
    ; show_aspect_border  : bool option [@default None]
    ; aspect_border_color : int option [@default None]
    ; error_overlay       : Error_overlay.t option [@default None]
    ; channel_name        : Channel_name.t option [@default None]
    ; audio_meter         : Audio_meter.t option [@default None]
    ; status_bar          : Status_bar.t option [@default None]
    } [@@deriving yojson]

  let update a b =
    { show_border         = a.show_border <+> b.show_border
    ; border_color        = a.border_color <+> b.border_color
    ; show_aspect_border  = a.show_aspect_border <+> b.show_aspect_border
    ; aspect_border_color = a.aspect_border_color <+> b.aspect_border_color
    ; error_overlay       = opt_update Error_overlay.update a.error_overlay b.error_overlay
    ; channel_name        = opt_update Channel_name.update a.channel_name b.channel_name
    ; audio_meter         = opt_update Audio_meter.update a.audio_meter b.audio_meter
    ; status_bar          = opt_update Status_bar.update a.status_bar b.status_bar
    }

end

(* -------------- QoE settings  ---------------- *)

module Qoe_settings = struct

  type setting =
    { peak_en : bool option [@default None]
    ; peak    : float option [@default None]
    ; cont_en : bool option [@default None]
    ; cont    : float option [@default None]
    } [@@deriving yojson]

  let setting_update a b =
    { peak_en = a.peak_en <+> b.peak_en
    ; peak    = a.peak <+> b.peak
    ; cont_en = a.cont_en <+> b.cont_en
    ; cont    = a.cont <+> b.cont
    }

  type loss =
    { vloss : float option [@default None]
    ; aloss : float option [@default None]
    } [@@deriving yojson]

  let loss_update a b =
    { vloss = a.vloss <+> b.vloss
    ; aloss = a.aloss <+> b.vloss
    }

  type black =
    { black       : setting option [@default None]
    ; luma        : setting option [@default None]
    ; black_pixel : int option [@default None]
    ; time        : float option [@default None]
    } [@@deriving yojson]

  let black_update a b =
    { black       = opt_update setting_update a.black b.black
    ; luma        = opt_update setting_update a.luma b.luma
    ; black_pixel = a.black_pixel <+> b.black_pixel
    ; time        = a.time <+> b.time
    }

  type freeze =
    { freeze     : setting option [@default None]
    ; diff       : setting option [@default None]
    ; pixel_diff : int option [@default None]
    ; time       : float option [@default None]
    } [@@deriving yojson]

  let freeze_update a b =
    { freeze     = opt_update setting_update a.freeze b.freeze
    ; diff       = opt_update setting_update a.diff b.diff
    ; pixel_diff = a.pixel_diff <+> b.pixel_diff
    ; time       = a.time <+> b.time
    }

  type blocky =
    { blocky      : setting option [@default None]
    ; mark_blocks : bool option [@default None]
    ; time        : float option [@default None]
    } [@@deriving yojson]

  let blocky_update a b =
    { blocky      = opt_update setting_update a.blocky b.blocky
    ; mark_blocks = a.mark_blocks <+> b.mark_blocks
    ; time        = a.time <+> b.time
    }

  type silence =
    { silence : setting option [@default None]
    ; time    : float option [@default None]
    } [@@deriving yojson]

  let silence_update a b =
    { silence = opt_update setting_update a.silence b.silence
    ; time    = a.time <+> b.time
    }

  type loudness =
    { loudness : setting option [@default None]
    ; time     : float option [@default None]
    } [@@deriving yojson]

  let loudness_update a b =
    { loudness = opt_update setting_update a.loudness b.loudness
    ; time     = a.time <+> b.time
    }

  type adv =
    { adv_diff : float option [@default None]
    ; adv_buf  : int option [@default None]
    } [@@deriving yojson]

  let adv_update a b =
    { adv_diff = a.adv_diff <+> b.adv_diff
    ; adv_buf  = a.adv_buf <+> b.adv_buf
    }

  type t =
    { loss      : loss option [@default None]
    ; black     : black option [@default None]
    ; freeze    : freeze option [@default None]
    ; blocky    : blocky option [@default None]
    ; silence   : silence option [@default None]
    ; loudness  : loudness option [@default None]
    ; adv       : adv option [@default None]
    } [@@deriving yojson]

  let update a b =
    { loss     = opt_update loss_update a.loss b.loss
    ; black    = opt_update black_update a.black b.black
    ; freeze   = opt_update freeze_update a.freeze b.freeze
    ; blocky   = opt_update blocky_update a.blocky b.blocky
    ; silence  = opt_update silence_update a.silence b.silence
    ; loudness = opt_update loudness_update a.loudness b.loudness
    ; adv      = opt_update adv_update a.adv b.adv
    }

end

(* -------------- Settings  ---------------- *)

module Settings = struct

  type t =
    { channel_settings : Channel_settings.t option [@default None]
    ; qoe_settings     : Qoe_settings.t option [@default None]
    } [@@deriving yojson]

  let update a b =
    { channel_settings = opt_update Channel_settings.update a.channel_settings b.channel_settings
    ; qoe_settings     = opt_update Qoe_settings.update a.qoe_settings b.qoe_settings
    }

end

(* ------------- Prog list (metadata) --------- *)

module Position = struct

  type t =
    { x      : int
    ; y      : int
    ; width  : int
    ; height : int
    } [@@deriving yojson]

  let update a b =
    { x       = b.x
    ; y       = b.y
    ; width   = b.width
    ; height  = b.height
    }

end


module Meta_pid = struct

  type video_pid =
    { codec      : string
    ; resolution : (int * int) option [@default None]
    ; aspect     : (int * int) option [@default None]
    ; interlaced : string option [@default None]
    ; frame_rate : float option [@default None]
    } [@@deriving yojson]

  type audio_pid =
    { codec       : string
    ; bitrate     : string option [@default None]
    ; sample_rate : int option [@default None]
    } [@@deriving yojson]

  type pid_content = Video of video_pid
                   | Audio of audio_pid
                   [@@deriving yojson]

  type t =
    { pid              : int
    ; to_be_analyzed   : bool option [@default None]
    ; pid_content      : pid_content option [@default None]
    ; stream_type      : int option [@default None]
    ; stream_type_name : string option [@default None]
    ; position         : Position.t option [@default None]
    } [@@deriving yojson]

  let update a b = b

end

module Meta_channel = struct

  type t =
    { number        : int
    ; service_name  : string option [@default None]
    ; provider_name : string option [@default None]
    ; pids          : Meta_pid.t list
    } [@@deriving yojson]

  let update a b =
    { number        = b.number
    ; service_name  = a.service_name <+> b.service_name
    ; provider_name = a.provider_name <+> b.provider_name
    ; pids          = b.pids
    }

end

(* ------------- Options ---------------------- *)

module Options = struct

  type t =
    { prog_list         : Meta_channel.t list option [@default None]
    ; mosaic_resolution : (int * int) option [@default None]
    ; mosaic_bg_color   : int option [@default None]
    } [@@deriving yojson]

  let update a b =
    { prog_list         = b.prog_list
    ; mosaic_resolution = a.mosaic_resolution <+> b.mosaic_resolution
    ; mosaic_bg_color   = a.mosaic_bg_color <+> b.mosaic_bg_color
    }

end

(* ------------- Graph ------------------------ *)

module Graph = struct

  type graph_state = Null
                   | Pause
                   | Play
                   | Stop
  let graph_state_of_yojson = function
    | `String "Null"  -> Ok Null
    | `String "Pause" -> Ok Pause
    | `String "Play"  -> Ok Play
    | `String "Stop"  -> Ok Stop
    | err -> Error ("graph_state_of_yojson: wrang data" ^ (Yojson.Safe.to_string err))
  let graph_state_to_yojson = function
    | Null  -> `String "Null"
    | Pause -> `String "Pause"
    | Play  -> `String "Play"
    | Stop  -> `String "Stop"

  type t =
    { state : graph_state option [@default None] } [@@deriving yojson]

  let update a b =
    { state = a.state <+> b.state }

end

(* ------------- Root ------------------------- *)

module Qoe_root = struct

  type t =
    { options  : Options.t option [@default None]
    ; settings : Settings.t option [@default None]
    ; graph    : Graph.t option [@default None]
    } [@@deriving yojson]

  let update a b =
    { options  = opt_update Options.update a.options b.options
    ; settings = opt_update Settings.update a.settings b.settings
    ; graph    = opt_update Graph.update a.graph b.graph
    }

end

let rec filter_none : Yojson.Safe.json -> Yojson.Safe.json = function
  | `Assoc tl  -> let vk = List.filter (function (_, `Null) -> false | _ -> true) tl
                  in `Assoc (List.map (fun (s,o) -> s, filter_none o) vk)
  | `List lst  -> `List (List.map filter_none lst)
  | `Tuple lst -> `Tuple (List.map filter_none lst)
  | `Variant (s, Some o) -> `Variant (s,Some (filter_none o))
  | o -> o

let default : Qoe_root.t =
  { options  = None
  ; settings = None
  ; graph    =
      (Some { state = (Some Play) })}
