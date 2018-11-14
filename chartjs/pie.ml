open Types

module Options = struct

  module Animation = struct
    include Animation
    include (Pie_.Options.Animation
             : module type of Pie_.Options.Animation
                              with type t := Animation.t)
  end
  include (Pie_.Options
           : module type of Pie_.Options
                            with module Animation := Animation)
  include (Options : module type of Options with type t := t)

  let make ?elements ?animation ?layout ?legend ?title ?tooltips ?hover
        ?legend_callback ?responsive ?responsive_animation_duration
        ?maintain_aspect_ratio ?aspect_ratio ?on_resize ?device_pixel_ratio
        ?events ?on_hover ?on_click ?cutout_percentage ?rotation
        ?circumference () =
    let t =
      Options.make ?elements ?animation ?layout ?legend ?title ?tooltips ?hover
        ?legend_callback ?responsive ?responsive_animation_duration
        ?maintain_aspect_ratio ?aspect_ratio ?on_resize ?device_pixel_ratio
        ?events ?on_hover ?on_click () in
    begin match cutout_percentage with
    | None -> ()
    | Some x -> set_cutout_percentage t x
    end;
    begin match rotation with
    | None -> ()
    | Some x -> set_rotation t x
    end;
    begin match circumference with
    | None -> ()
    | Some x -> set_circumference t x
    end;
    t

end

module Dataset = Pie_.Dataset

module Data = Pie_.Data

module Config = struct
  include Pie_.Config

  let make = make ~type_:"pie"
end

include Chart

let config (t : Chart.t) : Config.t =
  Ojs.get (Chart.t_to_js t) "config"
  |> Obj.magic

let make (node : node) (config : Config.t) : Chart.t =
  let node = match node with
    | `Id s -> Ojs.string_to_js s
    | `Canvas c -> Obj.magic c
    | `Context c -> Obj.magic c in
  Chart.new_chart node (Obj.magic config)
