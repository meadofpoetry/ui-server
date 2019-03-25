
let name = "settings"

type setting =
  { peak_en  : bool
  ; peak     : float
  ; cont_en  : bool
  ; cont     : float
  ; duration : float
  } [@@deriving yojson,eq]

type black =
  { black       : setting
  ; luma        : setting
  ; black_pixel : int
  } [@@deriving yojson,eq]

type freeze =
  { freeze     : setting
  ; diff       : setting
  ; pixel_diff : int
  } [@@deriving yojson,eq]

type blocky =
  { blocky  : setting
  } [@@deriving yojson,eq]

type silence =
  { silence : setting
  } [@@deriving yojson,eq]

type loudness =
  { loudness : setting
  } [@@deriving yojson,eq]

type adv =
  { adv_diff : float
  ; adv_buf  : int
  } [@@deriving yojson,eq]

type video =
  { loss     : float
  ; black    : black
  ; freeze   : freeze
  ; blocky   : blocky
  } [@@deriving yojson,eq]
  
type audio =
  { loss     : float
  ; silence  : silence
  ; loudness : loudness
  ; adv      : adv
  } [@@deriving yojson,eq]
  
type t =
  { video  : video
  ; audio  : audio
  } [@@deriving yojson,eq]

let black_default = { black = { peak_en = true
                              ; peak    = 100.0
                              ; cont_en = false
                              ; cont    = 90.0
                              ; duration = 10.
                              }
                    ; luma  = { peak_en = false
                              ; peak    = 20.0
                              ; cont_en = true
                              ; cont    = 17.0
                              ; duration = 10.
                              }
                    ; black_pixel = 16
                    }

let freeze_default = { freeze  = { peak_en = true
                                 ; peak    = 99.0
                                 ; cont_en = false
                                 ; cont    = 80.0
                                 ; duration = 10.
                                 }
                     ; diff    = { peak_en = false
                                 ; peak    = 0.1
                                 ; cont_en = true
                                 ; cont    = 0.02
                                 ; duration = 10.
                                 }
                     ; pixel_diff  = 2
                     }

let blocky_default = { blocky = { peak_en = true
                                ; peak    = 7.0
                                ; cont_en = false
                                ; cont    = 4.0
                                ; duration = 10.
                                }
                     }

let silence_default = { silence = { peak_en = false
                                  ; peak    = (-35.0)
                                  ; cont_en = true
                                  ; cont    = (-33.0)
                                  ; duration = 3.
                                  }
                      }

let loudness_default = { loudness = { peak_en = true
                                    ; peak    = (-15.0)
                                    ; cont_en = true
                                    ; cont    = (-22.0)
                                    ; duration = 4.
                                    }
                       }

let adv_default = { adv_diff = 1.5
                  ; adv_buf = 3200
                  }
                    
let default = { video = { loss = 2.0
                        ; black = black_default
                        ; freeze = freeze_default
                        ; blocky = blocky_default
                        }
              ; audio = { loss = 3.0
                        ; silence = silence_default
                        ; loudness = loudness_default
                        ; adv = adv_default
                        }
              }

let to_string s = Yojson.Safe.to_string (to_yojson s)
let of_string s =
  match of_yojson (Yojson.Safe.from_string s) with
  | Ok v -> v
  | Error e -> failwith e

let combine ~set x = ignore set; `Kept x
