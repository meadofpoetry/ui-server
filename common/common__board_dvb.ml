type rsp_devinfo =
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
  } [@@deriving yojson]

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
  } [@@deriving yojson]

type rsp_plp_set =
  { lock    : bool
  ; plp     : int
  } [@@deriving yojson]

type devinfo_response = rsp_devinfo [@@deriving yojson]
  
type settings_request = (int * settings) [@@deriving yojson]

type settings_response = (int * rsp_settings) [@@deriving yojson]

type plp_setting_request = (int * int) [@@deriving yojson]

type plp_setting_response = (int * rsp_plp_set) [@@deriving yojson]

type plp_list_response = (int * rsp_plp_list) [@@deriving yojson]
