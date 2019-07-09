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

let make_container ?(widgets = []) ~position () : string * Wm.container =
  "Sample container",
  { position
  ; widgets
  }

let widgets =
  [ make_widget ~type_:Audio ~x:0 ~y:0 ~w:50 ~h:50 ()
  ; make_widget ~aspect:(16, 9) ~x:50 ~y:0 ~w:50 ~h:50 ()
  ; make_widget ~domain:(Chan { stream = Application_types.Stream.ID.make "id"
                              ; channel = 2
                              })
      ~x:10 ~y:0 ~w:50 ~h:50 ()
  (* ; make_widget ~x:111 ~y:0 ~w:189 ~h:150 ()
   * ; make_widget ~x:0 ~y:150 ~w:200 ~h:150 ()
   * ; make_widget ~x:210 ~y:150 ~w:90 ~h:150 ~type_:Audio () *)
  ]

let container =
  make_container
    ~position:{ left = 0; top = 0; right = 1080; bottom = 1920 }
    ~widgets
    ()
