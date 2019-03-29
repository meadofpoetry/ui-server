type info =
  { serial : int
  ; hw_ver : int
  ; fpga_ver : int
  ; soft_ver : int
  ; asi : bool
  ; receivers : int list
  } [@@deriving yojson, eq]

type standard =
  | T2 [@value 1]
  | T
  | C [@@deriving yojson, eq, show, enum]

type bw =
  | Bw8 [@value 1]
  | Bw7
  | Bw6 [@@deriving yojson, eq, show, enum]

type channel =
  { bw : bw
  ; freq : int
  ; plp  : int
  } [@@deriving yojson, eq, show]

type mode =
  { standard : standard
  ; channel : channel
  } [@@deriving yojson, eq, show]

type mode_rsp =
  { mode : mode
  ; hw_present : bool
  ; lock : bool
  } [@@deriving yojson, show]

type config = (int * mode) list [@@deriving yojson, eq]
