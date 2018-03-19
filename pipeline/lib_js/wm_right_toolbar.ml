open Containers
open Components
open Wm_types

module Make(I : Item) = struct

  module It = Wm_items.Make(I)

  class t ~layers ~selected ~candidates ~set_candidates () =
    let items,sel = It.make ~selected ~candidates ~set_candidates () in
    let layers,e  = Wm_layers.make ~init:layers ~max:I.max_layers in
    let _class    = "wm-right-toolbar" in
    object(self)
      inherit Box.t ~vertical:true ~widgets:[items#widget;layers#widget] ()
      method e_layers_action : Wm_layers.action React.event = e
      initializer
        React.S.map (fun i -> if Option.is_some i then sel `Props) selected |> ignore;
        self#add_class _class
    end

  let make ~layers ~selected = new t ~layers ~selected ()

end
