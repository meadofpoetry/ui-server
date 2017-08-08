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

(* -------------- QoE settings  ---------------- *)

module Qoe_settings = struct

  type setting =
    { peak_en : bool option [@default None]
    ; peak    : float option [@default None]
    ; cont_en : bool option [@default None]
    ; cont    : float option [@default None]
    } [@@deriving yojson, lens]

  let setting_update a b =
    { peak_en = a.peak_en <+> b.peak_en
    ; peak    = a.peak <+> b.peak
    ; cont_en = a.cont_en <+> b.cont_en
    ; cont    = a.cont <+> b.cont
    }

  type loss =
    { vloss : float option [@default None]
    ; aloss : float option [@default None]
    } [@@deriving yojson, lens]

  let loss_update a b =
    { vloss = a.vloss <+> b.vloss
    ; aloss = a.aloss <+> b.vloss
    }

  type black =
    { black       : setting option [@default None]
    ; luma        : setting option [@default None]
    ; black_pixel : int option [@default None]
    ; time        : float option [@default None]
    } [@@deriving yojson, lens]

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
    } [@@deriving yojson, lens]

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
    } [@@deriving yojson, lens]

  let blocky_update a b =
    { blocky      = opt_update setting_update a.blocky b.blocky
    ; mark_blocks = a.mark_blocks <+> b.mark_blocks
    ; time        = a.time <+> b.time
    }

  type silence =
    { silence : setting option [@default None]
    ; time    : float option [@default None]
    } [@@deriving yojson, lens]

  let silence_update a b =
    { silence = opt_update setting_update a.silence b.silence
    ; time    = a.time <+> b.time
    }

  type loudness =
    { loudness : setting option [@default None]
    ; time     : float option [@default None]
    } [@@deriving yojson, lens]

  let loudness_update a b =
    { loudness = opt_update setting_update a.loudness b.loudness
    ; time     = a.time <+> b.time
    }

  type adv =
    { adv_diff : float option [@default None]
    ; adv_buf  : int option [@default None]
    } [@@deriving yojson, lens]

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
    } [@@deriving yojson, lens]

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
  module Qoe_settings = Qoe_settings

  type t =
    { qoe_settings     : Qoe_settings.t option [@default None]
    } [@@deriving yojson, lens]

  let update a b =
    { qoe_settings     = opt_update Qoe_settings.update a.qoe_settings b.qoe_settings
    }

end

(* ------------- Prog list (metadata) --------- *)

module Meta_pid = struct

  type video_pid =
    { codec      : string
    ; resolution : (int * int) option [@default None]
    ; aspect     : (int * int) option [@default None]
    ; interlaced : string option [@default None]
    ; frame_rate : float option [@default None]
    } [@@deriving yojson, lens]

  type audio_pid =
    { codec       : string
    ; bitrate     : string option [@default None]
    ; sample_rate : int option [@default None]
    } [@@deriving yojson, lens]

  type pid_content = Video of video_pid
                   | Audio of audio_pid
                   [@@deriving yojson]

  type t =
    { pid              : int
    ; to_be_analyzed   : bool option [@default None]
    ; pid_content      : pid_content option [@default None]
    ; stream_type      : int option [@default None]
    ; stream_type_name : string option [@default None]
    } [@@deriving yojson, lens]

  let update _ b = b

end

module Meta_channel = struct
  module Meta_pid = Meta_pid

  type t =
    { number        : int
    ; service_name  : string option [@default None]
    ; provider_name : string option [@default None]
    ; pids          : Meta_pid.t list
    } [@@deriving yojson, lens]

  let update a b =
    { number        = b.number
    ; service_name  = a.service_name <+> b.service_name
    ; provider_name = a.provider_name <+> b.provider_name
    ; pids          = b.pids
    }

end

module Meta_stream = struct
  module Meta_channel = Meta_channel

  type t =
    { stream   : int
    ; uri      : string
    ; channels : Meta_channel.t list
    } [@@deriving yojson, lens]

  let update _ b = b

end

(* ------------- Options ---------------------- *)

module Options = struct
  module Meta_stream = Meta_stream

  type t =
    { prog_list         : Meta_stream.t list option [@default None]
    } [@@deriving yojson, lens]

  let update _ b =
    { prog_list         = b.prog_list
    }

end

(* ------------- Graph ------------------------ *)

module Graph = struct

  type graph_state = Null
                   | Pause
                   | Play
                   | Stop
  let graph_state_of_yojson = function
    | `String "null"  -> Ok Null
    | `String "pause" -> Ok Pause
    | `String "play"  -> Ok Play
    | `String "stop"  -> Ok Stop
    | err -> Error ("graph_state_of_yojson: wrong data" ^ (Yojson.Safe.to_string err))
  let graph_state_to_yojson = function
    | Null  -> `String "null"
    | Pause -> `String "pause"
    | Play  -> `String "play"
    | Stop  -> `String "stop"

  type t =
    { state : graph_state option [@default None] } [@@deriving yojson]

  let update a b =
    { state = a.state <+> b.state }

end

(* ------------- WM --------------------------- *)

module Wm = struct

  (* NOTE: just an assumption *)
  type widget_type = Video
                   | Audio
                   | Subtitles
                   | Teletext
                   | Qos          (* qos errors/events *)
                   | Qoe          (* qoe errors/events *)
                   | Clock        (* digital or analog clock *)
                   | Text         (* static text *)
                   | Image        (* image (from file or from url, maybe) *)
                   | Source       (* text with channel(stream,input,etc) name*)
                   | Eit          (* text with EIT info *)
                   | Service_info (* text with service desrciption (resolution,codec,etc) *)
                   | Icons_bar    (* status bar with availability indication of eit, scte35, teletext etc  *)
  [@@deriving yojson]

  type background = (* NOTE incomplete *)
    { color : int } [@@deriving yojson]

  type position =
    { left   : int
    ; top    : int
    ; right  : int
    ; bottom : int
    } [@@deriving yojson]

  type widget =
    { type_    : widget_type [@key "type"]
    ; position : position
    } [@@deriving yojson]

  type window =
    { position : position
    ; widgets  : (string * widget) list option
    } [@@deriving yojson]

  type t =
    { background : background
    ; resolution : int * int
    ; windows    : (string * window) list
    ; widgets    : (string * widget) list
    ; layout     : (string * window) list
    } [@@deriving yojson]

  let update _ b = b

end

(* ------------- Root ------------------------- *)

module State = struct
  module Graph    = Graph
  module Settings = Settings
  module Options  = Options
  module Wm       = Wm
  
  type t =
    { options  : Options.t option [@default None]
    ; settings : Settings.t option [@default None]
    ; graph    : Graph.t option [@default None]
    ; wm       : Wm.t option [@default None]
    } [@@deriving yojson, lens]

  let update a b =
    { options  = opt_update Options.update a.options b.options
    ; settings = opt_update Settings.update a.settings b.settings
    ; graph    = opt_update Graph.update a.graph b.graph
    ; wm       = opt_update Wm.update a.wm b.wm
    }

end

let rec filter_none : Yojson.Safe.json -> Yojson.Safe.json = function
  | `Assoc tl  -> let vk = List.filter (function (_, `Null) -> false | _ -> true) tl
                  in `Assoc (List.map (fun (s,o) -> s, filter_none o) vk)
  | `List lst  -> `List (List.map filter_none lst)
  | `Tuple lst -> `Tuple (List.map filter_none lst)
  | `Variant (s, Some o) -> `Variant (s,Some (filter_none o))
  | o -> o

let default : State.t =
  { options  =
      (Some { prog_list =
                (Some [ { stream   = 0
                        ; uri      = "udp://127.0.0.1:1234"
                        ; channels =
                            [ { number        = 0
                              ; service_name  = Some "Channel 1"
                              ; provider_name = Some "RTRN"
                              ; pids          =
                                  [ { pid              = 1024
                                    ; to_be_analyzed   = Some false
                                    ; pid_content      = None
                                    ; stream_type      = Some 4
                                    ; stream_type_name = Some "video-h264"
                                    }
                                  ]
                              }
                            ]
                        } ])
            })
  ; settings =
      (Some { qoe_settings =
                (Some { loss =
                          (Some { vloss = Some 2.0
                                ; aloss = Some 3.0
                                })
                      ; black =
                          (Some { black       =
                                    (Some { peak_en = Some true
                                          ; peak    = Some 100.0
                                          ; cont_en = Some false
                                          ; cont    = Some 90.0
                                          })
                                ; luma        =
                                    (Some { peak_en = Some false
                                          ; peak    = Some 20.0
                                          ; cont_en = Some true
                                          ; cont    = Some 17.0
                                          })
                                ; time        = Some 10.0
                                ; black_pixel = Some 16
                                })
                      ; freeze =
                          (Some { freeze      =
                                    (Some { peak_en = Some true
                                          ; peak    = Some 99.0
                                          ; cont_en = Some false
                                          ; cont    = Some 80.0
                                          })
                                ; diff        =
                                    (Some { peak_en = Some false
                                          ; peak    = Some 0.1
                                          ; cont_en = Some true
                                          ; cont    = Some 0.02
                                          })
                                ; time        = Some 14.0
                                ; pixel_diff  = Some 2
                                })
                      ; blocky =
                          (Some { blocky     =
                                    (Some { peak_en = Some true
                                          ; peak    = Some 7.0
                                          ; cont_en = Some false
                                          ; cont    = Some 4.0
                                          })
                                ; time       = Some 5.0
                                ; mark_blocks = Some false
                                })
                      ; silence =
                          (Some { silence     =
                                    (Some { peak_en = Some false
                                          ; peak    = Some (-35.0)
                                          ; cont_en = Some true
                                          ; cont    = Some (-33.0)
                                          })
                                ; time       = Some 3.0
                                })
                      ; loudness =
                          (Some { loudness   =
                                    (Some { peak_en = Some true
                                          ; peak    = Some (-15.0)
                                          ; cont_en = Some true
                                          ; cont    = Some (-22.0)
                                          })
                                ; time       = Some 4.0
                                })
                      ; adv =
                          (Some { adv_diff   = Some 1.5
                                ; adv_buf    = Some (3200)})
                      })
            })
  ; graph    =
      (Some { state = (Some Play) })
  ; wm       = None
  }

    (*
      type xy = { x : string; y : int } [@@deriving lens];;
      type st = { out : string; i : t option } [@@deriving lens];;
      let mod_opt f v o = Option.(o >>= fun s -> return ((f ^= v) s));;
      (st_i ^%= (mod_opt xy_y 17)) @@ { out = "12"; i = Some {x = "3"; y = 5} };;
     *)
