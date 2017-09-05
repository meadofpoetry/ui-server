type rsp_devinfo =
  { serial   : int
  ; hw_ver   : int
  ; fpga_ver : int
  ; soft_ver : int
  ; asi      : bool
  ; modules  : int list
  } [@@deriving to_yojson]

type mode =
  | T2
  | T
  | C [@@deriving yojson]
  
type bw =
  | Bw8
  | Bw7
  | Bw6 [@@deriving yojson]
  
type settings =
  { mode     : mode
  ; bw       : bw
  ; freq     : int32
  ; plp      : int
  } [@@deriving yojson]

type rsp_settings =
  { settings   : settings
  ; hw_present : bool
  ; lock       : bool
  } [@@deriving to_yojson]

type rsp_measure =
  { lock    : bool
  ; power   : float option
  ; mer     : float option
  ; ber     : float option
  ; freq    : int32 option
  ; bitrate : int32 option
  } [@@deriving to_yojson]

type rsp_plp_list =
  { lock    : bool
  ; plps    : int list
  } [@@deriving to_yojson]

type rsp_plp_set =
  { lock    : bool
  ; plp     : int
  } [@@deriving to_yojson]
 
