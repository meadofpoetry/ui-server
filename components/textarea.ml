open Containers

module Pure = struct

  class t ~input_id ?placeholder ?rows ?cols () =
    let elt = Markup.Textfield.create ~input_id ?placeholder ?rows ?cols ~textarea:true ()
              |> Tyxml_js.To_dom.of_div in
    let input_elt = elt##querySelector (Js.string ("." ^ Markup.Textfield.input_class))
                    |> Js.Opt.to_option |> Option.get_exn |> Js.Unsafe.coerce in
    object
      inherit [string] Widget.text_input_widget ~input_elt Widget.Text elt ()
    end

end

class type mdc =
  object
    method disabled : bool Js.t Js.prop
    method valid    : bool Js.t Js.writeonly_prop
  end

class t ~input_id ?label ?placeholder ?rows ?cols () =

  let elt = (Markup.Textfield.create ~input_id ?label ?placeholder ?rows ?cols ~textarea:true ()
             |> Tyxml_js.To_dom.of_div) in
  let input_elt = elt##querySelector (Js.string ("." ^ Markup.Textfield.input_class))
                  |> Js.Opt.to_option |> Option.get_exn |> Js.Unsafe.coerce in

  object(self)

    inherit Widget.input_widget ~input_elt elt ()

    val mdc : mdc Js.t = elt |> (fun x -> Js.Unsafe.global##.mdc##.textField##.MDCTextField##attachTo x)

    method set_dense x      = self#add_or_remove_class x Markup.Textfield.dense_class
    method set_full_width x = self#add_or_remove_class x Markup.Textfield.fullwidth_class

    method disabled       = Js.to_bool mdc##.disabled
    method set_disabled x = mdc##.disabled := Js.bool x

    method set_valid x = mdc##.valid := (Js.bool x)

  end
