open Board_dektec_dtm3200_types
open Containers
open Components

module type M = sig
  type t
  val to_string : t -> string
end

type parameter_type =
  [ `FEC_delay
  | `FEC_cols
  | `FEC_rows
  | `Jitter_tolerance
  | `Lost_after_FEC
  | `Lost_before_FEC
  | `TP_per_IP
  | `Status
  | `Protocol
  | `Packet_size
  | `Input_bitrate
  | `Output_bitrate
  | `PCR_present
  | `Rate_change_counter
  | `Jitter_error_counter
  | `Lock_error_counter
  | `Delay_factor
  ] [@@deriving yojson]

let parameter_type_to_string = function
  | `FEC_delay -> "Задержка FEC"
  | `FEC_cols -> "FEC строк"
  | `FEC_rows -> "FEC строк"
  | `Jitter_tolerance -> "Допуск джиттера"
  | `Lost_after_FEC -> "Потери после FEC"
  | `Lost_before_FEC -> "потери до FEC"
  | `TP_per_IP -> "TP per IP"
  | `Status -> "Статус"
  | `Protocol -> "Протокол"
  | `Packet_size -> "Размер пакета"
  | `Input_bitrate -> "Входной битрейт"
  | `Output_bitrate -> "Выходной битрейт"
  | `PCR_present -> "Наличие PCR"
  | `Rate_change_counter -> "Изменений битрейта"
  | `Jitter_error_counter -> "Ошибка джиттера"
  | `Lock_error_counter -> "Ошибка захвата"
  | `Delay_factor -> ""

let parameter_type_to_unit = function
  | `FEC_delay -> "мс"
  | `FEC_cols -> ""
  | `FEC_rows -> ""
  | `Jitter_tolerance -> "мс"
  | `Lost_after_FEC -> ""
  | `Lost_before_FEC -> ""
  | `TP_per_IP -> ""
  | `Status -> ""
  | `Protocol -> ""
  | `Packet_size -> ""
  | `Input_bitrate -> "Мбит/с"
  | `Output_bitrate -> "Мбит/с"
  | `PCR_present -> ""
  | `Rate_change_counter -> ""
  | `Jitter_error_counter -> ""
  | `Lock_error_counter -> ""
  | `Delay_factor -> ""

type config =
  { typ : parameter_type
  } [@@deriving yojson]

let base_class = "ip-dektec-parameter"

module Make(M : M) = struct

  type event = M.t option React.event

  let inner_class = Markup.CSS.add_element base_class "inner"

  let value_to_string (config : config) = function
    | Some v ->
       begin match parameter_type_to_unit config.typ with
       | "" -> Printf.sprintf "%s" (M.to_string v)
       | s -> Printf.sprintf "%s %s" (M.to_string v) s
       end
    | None -> "-"
  let get_name (config : config) =
    parameter_type_to_string config.typ

  class t (event : event) (config : config) () =
    let value =
      new Typography.Text.t
        ~adjust_margin:false
        ~font:Headline_5
        ~text:(value_to_string config None)
        () in
    let _e = React.E.map Fun.(value#set_text % value_to_string config) event in
    let inner = Widget.create_div () in
    object
      inherit Widget.t Js_of_ocaml.Dom_html.(createDiv document) () as super

      method! init () : unit =
        inner#add_class inner_class;
        inner#append_child value;
        super#append_child inner;
        super#add_class base_class

      method! destroy () : unit =
        super#destroy ();
        React.E.stop ~strong:true _e
    end

  let make (event : event) (config : config) : 'a Dashboard.Item.item =
    Dashboard.Item.make_item
      ~name:(get_name config)
      (new t event config ())#widget

end

module Status = struct
  type t = state
  let to_string : t -> string = function
    | On -> "Вкл"
    | Off -> "Выкл"
    | Fail -> "Сбой"
end

module Float = struct
  type t = float
  let to_string : t -> string = Printf.sprintf "%.3g"
end

module Protocol = struct
  type t = protocol
  let to_string : t -> string = protocol_to_string
end

module Size = struct
  type t = packet_sz
  let to_string : t -> string = packet_sz_to_string
end

module Bool = struct
  type t = bool
  let to_string : t -> string = string_of_bool
end

module Int_param = Make(Int)
module Int32_param = Make(Int32)
module Int64_param = Make(Int64)
module Status_param = Make(Status)
module Protocol_param = Make(Protocol)
module Size_param = Make(Size)
module Bool_param = Make(Bool)
module Float_param = Make(Float)

let make (event : status React.event)
         (config : config) =
  let fmt_br x = float_of_int x /. 1_000_000. in
  let map f = React.E.map (fun x -> Some (f x)) in
  match config.typ with
  | `FEC_delay -> Int_param.make (map (fun s -> s.fec_delay) event) config
  | `FEC_cols -> Int_param.make (map (fun s -> s.fec_cols) event) config
  | `FEC_rows -> Int_param.make (map (fun s -> s.fec_rows) event) config
  | `Jitter_tolerance -> Int_param.make (map (fun s -> s.jitter_tol) event) config
  | `Lost_after_FEC -> Int64_param.make (map (fun s -> s.lost_after_fec) event) config
  | `Lost_before_FEC -> Int64_param.make (map (fun s -> s.lost_before_fec) event) config
  | `TP_per_IP -> Int_param.make (map (fun s -> s.tp_per_ip) event) config
  | `Status -> Status_param.make (map (fun s -> s.status) event) config
  | `Protocol -> Protocol_param.make (map (fun s -> s.protocol) event) config
  | `Packet_size -> Size_param.make (map (fun s -> s.packet_size) event) config
  | `Input_bitrate -> Float_param.make (map (fun s -> (fmt_br s.bitrate)) event) config
  | `Output_bitrate -> Float_param.make (map (fun s -> (fmt_br s.asi_bitrate)) event) config
  | `PCR_present -> Bool_param.make (map (fun s -> s.pcr_present) event) config
  | `Rate_change_counter -> Int32_param.make (map (fun s -> s.rate_change_cnt) event) config
  | `Jitter_error_counter -> Int32_param.make (map (fun s -> s.jitter_err_cnt) event) config
  | `Lock_error_counter -> Int32_param.make (map (fun s -> s.lock_err_cnt) event) config
  | `Delay_factor -> Int32_param.make (map (fun s -> s.delay_factor) event) config
