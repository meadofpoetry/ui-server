type err = Bad_tag_start of int
         | Bad_length of int
         | Bad_msg_code of int
         | Bad_module_addr of int
         | Bad_crc of (int * int)
         | Bad_tag_stop of int
         | Unknown_err of string

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

type init_conf = int list

type resp  = Ack
           | Devinfo     of rsp_devinfo
           | Settings    of (int * rsp_settings)
           | Measure     of (int * rsp_measure)
           | Plps        of (int * rsp_plp_list)
           | Plp_setting of (int * rsp_plp_set)

type req   = Devinfo     of bool
           | Settings    of int * settings
           | Measure     of int
           | Plps        of int
           | Plp_setting of int * int
  
let string_of_err = function
  | Bad_tag_start x   -> "incorrect start tag: "    ^ (string_of_int x)
  | Bad_length x      -> "incorrect length: "       ^ (string_of_int x)
  | Bad_msg_code x    -> "incorrect code: "         ^ (string_of_int x)
  | Bad_module_addr x -> "incorrect address: "      ^ (string_of_int x)
  | Bad_crc (x,y)     -> "incorrect crc: expected " ^ (string_of_int x) ^ ", got " ^ (string_of_int y)
  | Bad_tag_stop x    -> "incorrect stop tag: "     ^ (string_of_int x)
  | Unknown_err s     -> s
