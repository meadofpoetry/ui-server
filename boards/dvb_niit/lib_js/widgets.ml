open Containers
open Components

module type Widget = sig
  type config
  type init
  type event

  val name : string
  val make : init:init -> event:event -> config -> Widget.widget
end

module type Measure_params = sig
  type t
  val name : string
  val unit : string
  val data_to_string : t -> string
end

module Meas = struct
  module Power   : Measure_params with type t = float = struct
    type t = float
    let name = "Мощность"
    let unit = "дБм"
    let data_to_string = Printf.sprintf "%.2f"
  end
  module Mer     : Measure_params with type t = float = struct
    type t   = float
    let name = "MER"
    let unit = "дБ"
    let data_to_string = Printf.sprintf "%.2f"
  end
  module Ber     : Measure_params with type t = float = struct
    type t   = float
    let name = "BER"
    let unit = ""
    let data_to_string = Printf.sprintf "%.2f"
  end
  module Freq    : Measure_params with type t = int32 = struct
    type t   = int32
    let name = "Отклонение частоты"
    let unit = "Гц"
    let data_to_string = Int32.to_string
  end
  module Bitrate : Measure_params with type t = int32 = struct
    type t   = int32
    let name = "Битрейт"
    let unit = "Бит/с"
    let data_to_string = Int32.to_string
  end
end

module Make_chart(M:Measure_params) : (Widget with type init   = M.t Measures.data
                                               and type event  = M.t Measures.data React.event
                                               and type config = M.t Measures.config) = struct

  type config = M.t Measures.config
  type init   = M.t Measures.data
  type event  = M.t Measures.data React.event

  let name = M.name
  let make ~(init:init) ~(event:event) (config:config) = (Measures.make_chart ~init ~event config)#widget

end

module Power_chart   = Make_chart(Meas.Power)
module Mer_chart     = Make_chart(Meas.Mer)
module Ber_chart     = Make_chart(Meas.Ber)
module Freq_chart    = Make_chart(Meas.Freq)
module Bitrate_chart = Make_chart(Meas.Bitrate)

module Make_param(M:Measure_params) : (Widget with type init   = M.t option
                                               and type event  = M.t option React.event
                                               and type config = int) = struct
  type config = int
  type init   = M.t option
  type event  = init React.event

  let name = M.name
  let val_to_string = function
    | Some v -> Printf.sprintf "%s %s" (M.data_to_string v) M.unit
    | None   -> "-"
  let get_name id = Printf.sprintf "Модуль %d. %s" (succ id) name
  let make ~(init:init) ~(event:event) (config:config) =
    let name  = new Typography.Text.t ~adjust_margin:false ~font:Caption ~text:(get_name config) () in
    let value = new Typography.Text.t ~adjust_margin:false ~font:Headline ~text:(val_to_string init) () in
    let box   = new Box.t ~vertical:true ~widgets:[name#widget;value#widget] () in
    let _     = React.E.map (fun v -> value#set_text @@ val_to_string v) event in
    box#widget

end

module Power_param   = Make_param(Meas.Power)
module Mer_param     = Make_param(Meas.Mer)
module Ber_param     = Make_param(Meas.Ber)
module Freq_param    = Make_param(Meas.Freq)
module Bitrate_param = Make_param(Meas.Bitrate)

module Params = struct

end

module Settings = struct

end
