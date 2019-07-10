open Pipeline_types

let make_widget ?(type_ = Wm.Video)
    ?(domain = Wm.Nihil)
    ?aspect
    ~x ~y ~w ~h () : string * Wm.widget =
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
  ; pid = Some 4096
  ; type_
  ; aspect
  ; domain
  ; layer = 0
  }

let make_container
    ?(title = "Sample container")
    ?(widgets = [])
    ~position () : string * Wm.container =
  title, { position; widgets }

let widgets =
  [ make_widget ~type_:Audio ~x:0 ~y:0 ~w:50 ~h:50 ()
  ; make_widget ~aspect:(16, 9) ~x:50 ~y:0 ~w:50 ~h:50 ()
  ; make_widget
      ~domain:(Chan { stream = Application_types.Stream.ID.make "id"
                    ; channel = 2
                    })
      ~x:0 ~y:50 ~w:50 ~h:50 ()
  ]

let containers =
  [ make_container
      ~title:"Россия 1"
      ~position:{ left = 0; top = 0; right = 640; bottom = 360 }
      ~widgets
      ()
  ; make_container
      ~title:"ТНТ"
      ~position:{ left = 640; top = 0; right = 1280; bottom = 360 }
      ~widgets
      ()
  ; make_container
      ~title:"Первый канал"
      ~position:{ left = 0; top = 360; right = 640; bottom = 720 }
      ~widgets
      ()
  ; make_container
      ~title:"СТС"
      ~position:{ left = 640; top = 360; right = 1280; bottom = 720 }
      ~widgets
      ()
  ]

let (wm : Wm.t) =
  { layout = containers
  ; widgets = widgets
  ; resolution = 1280, 720
  }
