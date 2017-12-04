type setting =
  { peak_en  : bool
  ; peak     : float
  ; cont_en  : bool
  ; cont     : float
  ; duration : float
  } [@@deriving yojson]

type black =
  { black       : setting
  ; luma        : setting
  ; black_pixel : int
  } [@@deriving yojson]

type freeze =
  { freeze     : setting
  ; diff       : setting
  ; pixel_diff : int
  } [@@deriving yojson]

type blocky =
  { blocky      : setting
  ; mark_blocks : bool
  } [@@deriving yojson]

type silence =
  { silence : setting
  } [@@deriving yojson]

type loudness =
  { loudness : setting
  } [@@deriving yojson]

type adv =
  { adv_diff : float
  ; adv_buf  : int
  } [@@deriving yojson]

type video =
  { loss     : float
  ; black    : black
  ; freeze   : freeze
  ; blocky   : blocky
  } [@@deriving yojson]
  
type audio =
  { loss     : float
  ; silence  : silence
  ; loudness : loudness
  ; adv      : adv
  } [@@deriving yojson]
  
type t =
  { video  : video
  ; audio  : audio
  } [@@deriving yojson]

(* 
let default = { loss =
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
              }
 *)
