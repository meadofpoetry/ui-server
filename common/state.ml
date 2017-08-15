type t =
  { streams  : Streams.t  option [@default None]
  ; settings : Settings.t option [@default None]
  ; graph    : Graph.t option [@default None]
  ; wm       : Wm.t option [@default None]
  } [@@deriving yojson, lens]

open Opt_update
  
let update a b =
  { streams  = opt_update Streams.update a.streams b.streams
  ; settings = opt_update Settings.update a.settings b.settings
  ; graph    = opt_update Graph.update a.graph b.graph
  ; wm       = opt_update Wm.update a.wm b.wm
  }

let default : t =
  { streams  = None
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
