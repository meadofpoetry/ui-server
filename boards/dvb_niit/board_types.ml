open Common.Topology

type devinfo =
  { serial   : int
  ; hw_ver   : int
  ; fpga_ver : int
  ; soft_ver : int
  ; asi      : bool
  ; modules  : int list
  } [@@deriving yojson]

type mode =
  | T2
  | T
  | C [@@deriving yojson, eq]
  
type bw =
  | Bw8
  | Bw7
  | Bw6 [@@deriving yojson, eq]
  
type settings =
  { mode     : mode
  ; channel  : channel_settings
  }
and channel_settings =
  { bw   : bw
  ; freq : int32
  ; plp  : int
  }[@@deriving yojson, eq]

type rsp_settings =
  { settings   : settings
  ; hw_present : bool
  ; lock       : bool
  } [@@deriving yojson]

type plp_list =
  { lock    : bool
  ; plps    : int list
  } [@@deriving yojson]

type rsp_plp_set =
  { lock    : bool
  ; plp     : int
  } [@@deriving yojson]

type measure =
  { timestamp : float
  ; lock      : bool
  ; power     : float option
  ; mer       : float option
  ; ber       : float option
  ; freq      : int32 option
  ; bitrate   : int32 option
  } [@@deriving yojson]

type devinfo_response = devinfo option [@@deriving yojson]

type settings_request = (int * settings) [@@deriving yojson]

type settings_response = (int * rsp_settings) [@@deriving yojson]

type plp_setting_request = (int * int) [@@deriving yojson]

type plp_setting_response = (int * rsp_plp_set) [@@deriving yojson]

type plp_list_response = (int * plp_list) [@@deriving yojson]

type measure_response = (int * measure) [@@deriving yojson]

type config = (int * config_item) list
and config_item =
  { mode : mode
  ; t2   : channel_settings
  ; t    : channel_settings
  ; c    : channel_settings
  } [@@deriving yojson, eq]

let config_to_string c = Yojson.Safe.to_string @@ config_to_yojson c

let config_of_string s = config_of_yojson @@ Yojson.Safe.from_string s

let config_default = [ (0, { mode = T2
                           ; t2   = { bw   = Bw8
                                    ; freq = 586000000l
                                    ; plp  = 0 }
                           ; t    = { bw   = Bw8
                                    ; freq = 586000000l
                                    ; plp  = 0 }
                           ; c    = { bw   = Bw8
                                    ; freq = 586000000l
                                    ; plp  = 0 }})
                     ; (1, { mode = T2
                           ; t2   = { bw   = Bw8
                                    ; freq = 586000000l
                                    ; plp  = 0 }
                           ; t    = { bw   = Bw8
                                    ; freq = 586000000l
                                    ; plp  = 0 }
                           ; c    = { bw   = Bw8
                                    ; freq = 586000000l
                                    ; plp  = 0 }})
                     ; (2, { mode = T2
                           ; t2   = { bw   = Bw8
                                    ; freq = 586000000l
                                    ; plp  = 0 }
                           ; t    = { bw   = Bw8
                                    ; freq = 586000000l
                                    ; plp  = 0 }
                           ; c    = { bw   = Bw8
                                    ; freq = 586000000l
                                    ; plp  = 0 }})
                     ; (3, { mode = T2
                           ; t2   = { bw   = Bw8
                                    ; freq = 586000000l
                                    ; plp  = 0 }
                           ; t    = { bw   = Bw8
                                    ; freq = 586000000l
                                    ; plp  = 0 }
                           ; c    = { bw   = Bw8
                                    ; freq = 586000000l
                                    ; plp  = 0 }})
                     ]
