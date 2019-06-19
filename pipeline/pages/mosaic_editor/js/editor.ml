open Components
open Pipeline_types
open Wm_types

let ( % ) f g x = f (g x)

let remove ~eq cs set (t : 'a wm_item) =
  if t.unique then
    set @@ t :: (List.filter (not % eq t) @@ React.S.value cs)

let make_widget ?(type_ = Wm.Video) ?aspect ~x ~y ~w ~h () : string * Wm.widget =
  let position =
    Some { Wm.
           left = x
         ; top = y
         ; right = x + w
         ; bottom = y + h
         } in
  string_of_int @@ Random.bits (),
  { position
  ; description = "Sample widget"
  ; pid = None
  ; type_
  ; aspect
  ; domain = Nihil
  ; layer = 0
  }

let make_container ?(widgets = []) ~position () : Wm.container =
  { position
  ; widgets
  }

let container =
  make_container
    ~position:{ left = 0; top = 0; right = 1920; bottom = 1080 }
    ~widgets:[ make_widget ~x:0 ~y:0 ~w:111 ~h:150 ()
             ; make_widget ~x:111 ~y:0 ~w:189 ~h:150 ()
             ; make_widget ~x:0 ~y:150 ~w:200 ~h:150 ()
             ; make_widget ~x:210 ~y:150 ~w:90 ~h:150 ~type_:Audio ()
             ]
    ()

module Make(I : Item) = struct
  module RT = Controls.Make(I)

  type t =
    { ig : Widget.t
    ; rt : RT.t
    }

  let make ?(on_remove : (I.t -> unit) option)
      ~(title : string)
      ~(resolution : (int * int))
      ~(init : I.t list)
      ~(candidates : I.t list React.signal)
      ~(set_candidates : I.t list -> unit)
      () =
    let selected, selected_push = React.S.create None in
    let layers = List.sort compare @@ I.layers_of_t_list init in
    let rt = RT.make ~selected ~layers ~candidates ~set_candidates in
    let table = Widget_editor.make container in
    let ig =
      Widget.create_div
        ~widgets:[table]
        () in
    let _ =
      Ui_templates.Resize_observer.observe
        ~f:(fun _ -> table#layout ())
        ~node:ig#root
        () in
    ig#add_class "mosaic-table-wrapper";
    { ig; rt }

end
