type setting =
  { peak_en : bool option [@default None]
  ; peak    : float option [@default None]
  ; cont_en : bool option [@default None]
  ; cont    : float option [@default None]
  } [@@deriving yojson, lens { optional = true } ]

type loss =
  { vloss : float option [@default None]
  ; aloss : float option [@default None]
  } [@@deriving yojson, lens { optional = true } ]

type black =
  { black       : setting option [@default None]
  ; luma        : setting option [@default None]
  ; black_pixel : int option [@default None]
  ; time        : float option [@default None]
  } [@@deriving yojson, lens { optional = true } ]

type freeze =
  { freeze     : setting option [@default None]
  ; diff       : setting option [@default None]
  ; pixel_diff : int option [@default None]
  ; time       : float option [@default None]
  } [@@deriving yojson, lens { optional = true } ]

type blocky =
  { blocky      : setting option [@default None]
  ; mark_blocks : bool option [@default None]
  ; time        : float option [@default None]
  } [@@deriving yojson, lens { optional = true } ]

type silence =
  { silence : setting option [@default None]
  ; time    : float option [@default None]
  } [@@deriving yojson, lens { optional = true } ]

type loudness =
  { loudness : setting option [@default None]
  ; time     : float option [@default None]
  } [@@deriving yojson, lens { optional = true } ]

type adv =
  { adv_diff : float option [@default None]
  ; adv_buf  : int option [@default None]
  } [@@deriving yojson, lens { optional = true } ]

type t =
  { loss      : loss option [@default None]
  ; black     : black option [@default None]
  ; freeze    : freeze option [@default None]
  ; blocky    : blocky option [@default None]
  ; silence   : silence option [@default None]
  ; loudness  : loudness option [@default None]
  ; adv       : adv option [@default None]
  } [@@deriving yojson, lens { optional = true } ]

open Common__opt_update

let setting_update a b =
  { peak_en = a.peak_en <+> b.peak_en
  ; peak    = a.peak <+> b.peak
  ; cont_en = a.cont_en <+> b.cont_en
  ; cont    = a.cont <+> b.cont
  }

let loss_update a b =
  { vloss = a.vloss <+> b.vloss
  ; aloss = a.aloss <+> b.vloss
  }

let black_update (a : black) (b : black) =
  { black       = opt_update setting_update a.black b.black
  ; luma        = opt_update setting_update a.luma b.luma
  ; black_pixel = a.black_pixel <+> b.black_pixel
  ; time        = a.time <+> b.time
  }

let freeze_update (a : freeze) (b : freeze) =
  { freeze     = opt_update setting_update a.freeze b.freeze
  ; diff       = opt_update setting_update a.diff b.diff
  ; pixel_diff = a.pixel_diff <+> b.pixel_diff
  ; time       = a.time <+> b.time
  }

  
let blocky_update (a : blocky) (b : blocky) =
  { blocky      = opt_update setting_update a.blocky b.blocky
  ; mark_blocks = a.mark_blocks <+> b.mark_blocks
  ; time        = a.time <+> b.time
  }

let silence_update (a : silence) (b : silence) =
  { silence = opt_update setting_update a.silence b.silence
  ; time    = a.time <+> b.time
  }

let loudness_update (a : loudness) (b : loudness) =
  { loudness = opt_update setting_update a.loudness b.loudness
  ; time     = a.time <+> b.time
  }

let adv_update a b =
  { adv_diff = a.adv_diff <+> b.adv_diff
  ; adv_buf  = a.adv_buf <+> b.adv_buf
  }

let update a b =
  { loss     = opt_update loss_update a.loss b.loss
  ; black    = opt_update black_update a.black b.black
  ; freeze   = opt_update freeze_update a.freeze b.freeze
  ; blocky   = opt_update blocky_update a.blocky b.blocky
  ; silence  = opt_update silence_update a.silence b.silence
  ; loudness = opt_update loudness_update a.loudness b.loudness
  ; adv      = opt_update adv_update a.adv b.adv
  }

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
