open Common

type devinfo =
  { serial    : int
  ; hw_ver    : int
  ; fpga_ver  : int
  ; soft_ver  : int
  ; asi       : bool
  ; receivers : int list
  } [@@deriving yojson]

type devinfo_opt = devinfo option [@@deriving yojson]

type standard = T2
              | T
              | C [@@deriving yojson, eq]

type bw = Bw8
        | Bw7
        | Bw6 [@@deriving yojson, eq]

type channel =
  { bw   : bw
  ; freq : int
  ; plp  : int
  } [@@deriving yojson, eq]

type mode =
  { standard : standard
  ; channel  : channel
  } [@@deriving yojson, eq]

type mode_req =
  { id   : int
  ; mode : mode
  } [@@deriving yojson, eq]

type mode_rsp =
  { id         : int
  ; mode       : mode
  ; hw_present : bool
  ; lock       : bool
  } [@@deriving yojson]

type plp_list = int list [@@deriving yojson]

type plp_list_rsp =
  { id   : int
  ; lock : bool
  ; plps : plp_list
  } [@@deriving yojson]

type plp_set_req =
  { id  : int
  ; plp : int
  } [@@deriving yojson]

type plp_set_rsp =
  { id   : int
  ; lock : bool
  ; plp  : int
  } [@@deriving yojson]

type measures =
  { id        : int
  ; timestamp : Time.t
  ; lock      : bool
  ; power     : float option
  ; mer       : float option
  ; ber       : float option
  ; freq      : int option
  ; bitrate   : int option
  } [@@deriving yojson]

type config = (int * config_item) list
and config_item =
  { standard : standard
  ; t2       : channel
  ; t        : channel
  ; c        : channel
  } [@@deriving yojson, eq]

let config_to_string c = Yojson.Safe.to_string @@ config_to_yojson c

let config_of_string s = config_of_yojson @@ Yojson.Safe.from_string s

let default ?(freq=586000000) ?(plp=0) () = { bw = Bw8; freq; plp }

let config_default : config =
  [ 0, { standard = T2
       ; t2       = default ()
       ; t        = default ()
       ; c        = default () }
  ; 1, { standard = T2
       ; t2       = default ~plp:1 ()
       ; t        = default ()
       ; c        = default () }
  ; 2, { standard = T2
       ; t2       = default ~plp:2 ()
       ; t        = default ()
       ; c        = default () }
  ; 3, { standard = T2
       ; t2       = default ~freq:666_000_000 ()
       ; t        = default ()
       ; c        = default () }
  ]
