(* Misc *)

module Stream = struct

  type t = int32 [@@deriving yojson]

  let src_single = Int32.zero

  type source = Single
              | Ip of int32
              | T2mi_plp of int
              | Dvb_plp of int * int [@@deriving yojson]

  let of_int32 x = x
  let to_int32 x = x

  let to_source : t -> source = function
    | x when x = src_single -> Single
    | x when (Int32.logand x (Int32.of_int 0xFFFF0000)) = Int32.zero
      -> let x'     = Int32.to_int x in
         let stream = (x' land 0x0000FF00) lsr 8 in
         let plp    = (x' land 0xFF) in
         (match stream with
          | 1             -> T2mi_plp plp
          | 2 | 3 | 4 | 5 -> Dvb_plp (stream - 2, plp)
          | _             -> Ip x)
    | _ as x -> Ip x

  let of_source : source -> t = function
    | Single                -> src_single
    | T2mi_plp plp          -> 1
                               |> (fun x -> x lsl 8)
                               |> Int32.of_int
                               |> Int32.logor (Int32.of_int plp)
    | Dvb_plp (stream,plp)  -> stream + 2
                               |> (fun x -> x lsl 8)
                               |> Int32.of_int
                               |> Int32.logor (Int32.of_int plp)
    | Ip x                  -> x

end

(* Board info *)

type info =
  { typ : int
  ; ver : int
  } [@@deriving to_yojson]

(* Board mode *)

type input =
   | SPI
   | ASI [@@deriving yojson]

type t2mi_mode =
  { enabled   : bool
  ; pid       : int
  ; stream_id : Stream.t
  } [@@deriving yojson]

type mode =
  { input : input
  ; t2mi  : t2mi_mode option
  } [@@deriving yojson]

(* Status *)

type packet_fmt = Ts188
                | Ts192
                | Ts204 [@@deriving to_yojson]

type status =
  { ts_num       : int
  ; load         : int
  ; bitrate      : int32
  ; mode         : mode
  ; has_stream   : bool
  ; has_sync     : bool
  ; packet_fmt   : packet_fmt
  ; services_num : int
  } [@@deriving to_yojson]

(* MPEG-TS errors *)

type ts_error =
  { count     : int
  ; err_code  : int
  ; err_ext   : int
  ; multi_pid : bool
  ; pid       : int
  ; packet    : int32
  ; param_1   : int32
  ; param_2   : int32
  } [@@deriving to_yojson]

type ts_errors =
  { stream_id : Stream.t
  ; errors    : ts_error list
  } [@@deriving to_yojson]

(* T2-MI errors *)

type t2mi_error =
  { count          : int option
  ; err_code       : int
  ; t2mi_stream_id : int
  ; param          : int option
  }

type t2mi_errors =
  { stream_id     : Stream.t
  ; t2mi_pid      : int
  ; sync          : (int * bool) list
  ; parser_errors : int
  ; errors        : t2mi_error list
  }

(* SI/PSI section *)

type section =
  { stream_id : Stream.t
  ; data      : string (* FIXME*)
  }

(* T2-MI frames sequence *)

type t2mi_seq_item =
  { typ         : int
  ; super_frame : int
  ; frame       : int
  ; frame_dyn_1 : int option
  ; frame_dyn_2 : int option
  ; plp         : int
  ; count       : int32
  }

(* TS struct *)

type pid =
  { pid       : int
  ; has_pts   : bool
  ; scrambled : bool
  ; present   : bool
  }

type es =
  { pid          : int
  ; has_pts      : bool
  ; es_type      : int
  ; es_stream_id : int
  }

type ecm =
  { pid       : int
  ; ca_sys_id : int
  }

type service =
  { id            : int
  ; name          : string
  ; provider_name : string
  ; pmt_pid       : int
  ; pcr_pid       : int
  ; bitrate       : int32
  ; has_pmt       : bool
  ; has_sdt       : bool
  ; dscr          : bool
  ; list_dscr     : bool
  ; eit_schedule  : bool
  ; eit_pf        : bool
  ; free_ca_mode  : bool
  ; es            : es list
  ; ecm           : ecm list
  }

type emm = ecm

type table =
  { version        : int
  ; id             : int
  ; pid            : int
  ; id_ext         : int
  ; lsn            : int
  ; eit_info       : (int * int * int * int) option
  ; section_syntax : bool
  ; sections       : bool * int (* analyzed, length*)
  }

type ts_struct =
  { stream_id    : Stream.t
  ; complete     : bool
  ; services_num : int
  ; bitrate      : int32
  ; has_cat      : bool
  ; nw_pid       : int
  ; ts_id        : int
  ; nw_id        : int
  ; orig_nw_id   : int
  ; bouquet_id   : int
  ; nw_info      : string (* FIXME *)
  ; nw_name      : string
  ; bouquet_name : string
  ; pids         : pid list
  ; services     : service list
  ; emm          : emm list
  ; tables       : table list
  }

(* Bitrate *)

type pid_bitrate =
  { pid     : int
  ; bitrate : int32
  }

type table_bitrate =
  { id             : int
  ; id_ext         : int
  ; fully_analyzed : bool
  ; section_syntax : bool
  ; eit_info       : (int * int) option
  ; bitrate        : int32
  }

type bitrate =
  { stream_id  : Stream.t
  ; ts_bitrate : int32
  ; pids       : (int * int32) list
  ; tables     : table_bitrate list
  }

(* T2-MI info *)
type t2mi_info =
  { stream_id : Stream.t
  ; packets   : int list
  ; t2mi_pid  : int
  ; l1_pre    : string (* FIXME *)
  ; l1_conf   : string (* FIXME *)
  }
