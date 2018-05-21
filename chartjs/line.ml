open Containers
open Base

(* TODO remove *)
let (<) = Pervasives.(<)

include Line_types

module Dataset = Line_dataset

module Config = struct

  type 'a axis = 'a Axes_cartesian_common.axis

  class type data_js =
    object
      method datasets  : Dataset.t_js Js.t Js.js_array Js.t Js.prop
    end

  class type options_js =
    object
      inherit Options.t_js
      method showLines : bool Js.t Js.prop
      method spanGaps  : bool Js.t Js.prop
      method scales    : Axes.Cartesian.t_js Js.t Js.prop
    end

  class options ~(x_axis:(_,'a) #Axes_cartesian_common.t)
                ~(y_axis:(_,'b) #Axes_cartesian_common.t)
                () =
  object

    inherit [options_js] Options.t () as super

    val scales = new Axes.Cartesian.t ~x_axes:[x_axis] ~y_axes:[y_axis] ()

    method show_lines : bool = Js.to_bool obj##.showLines
    method set_show_lines x = obj##.showLines := Js.bool x

    method span_gaps : bool = Js.to_bool obj##.spanGaps
    method set_span_gaps x = obj##.spanGaps := Js.bool x

    method! replace x = super#replace x; scales#replace obj##.scales

    initializer
      obj##.scales := scales#get_obj

  end

  class ['a,'b] t ~(x_axis:(_,'a) #Axes_cartesian_common.t)
                ~(y_axis:(_,'b) #Axes_cartesian_common.t)
                ~(data:('a,'b) dataset list) () =
    let get_max l = List.fold_left (fun acc x ->
                        match acc with
                        | None -> Some x
                        | Some acc -> if x_axis#cmp_value x acc = 1 then Some x else Some acc) None l
    in
    let get_min = x_axis#calc_new_min in
    let max_x   = List.map (fun x -> get_max @@ List.map (fun x -> x.x) x.data) data
                  |> List.filter_map (fun x -> x) |> get_max
    in
    let s_max_x,s_max_x_push = React.S.create max_x in
    let datasets = List.map (fun data -> new Dataset.t ~data ~x_axis ~y_axis ~s_max_x ~s_max_x_push ())
                            data
    in
    let (data:data_js Js.t) =
      object%js
        val mutable datasets = List.map (fun x -> x#get_obj) datasets |> Array.of_list |> Js.array
      end in

    object
      val options     = new options ~x_axis ~y_axis ()
      method options  = options
      method datasets = datasets
      method data     = data
      initializer
        React.S.map (fun max_x -> match max_x,x_axis#delta with
                                  | Some max, Some d -> let min = get_min ~max ~delta:d in
                                                        x_axis#set_min (Some min);
                                                        x_axis#set_max (Some max)
                                  | _ -> ())
                    s_max_x |> ignore
    end

end

class ['a,'b] t ~(config:('a,'b) Config.t) () = object
  inherit [Config.options,Config.options_js] Base_chart.t ~typ:Line
                                             ~options:config#options
                                             ~data:(Js.Unsafe.inject config#data) ()

  method config = config
end
