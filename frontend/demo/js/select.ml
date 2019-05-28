open Components

type standard =
  | T2
  | T
  | C

let standard_to_string = function
  | T2 -> "DVB-T2"
  | T -> "DVB-T"
  | C -> "DVB-C"

let standard_of_string = function
  | "DVB-T2" -> Some T2
  | "DVB-T" -> Some T
  | "DVB-C" -> Some C
  | _ -> None

let standard = Select.(
    Custom { to_string = standard_to_string
           ; of_string = fun x ->
               match standard_of_string x with
               | None -> Error "Bad standard value"
               | Some x -> Ok x
           })

let make ?outlined () =
  let items = Select.native_options_of_values
      ~with_empty:true
      standard
      [T2; T; C] in
  Select.make_native
    ?outlined
    ~label:"Стандарт"
    ~items
    standard

let section () =
  let select = make () in
  let select_2 = make ~outlined:true () in
  let b_t2 = Button.make
      ~appearance:Raised
      ~label:"SET T2"
      ~on_click:(fun _ _ _ ->
          select#set_value T2;
          select_2#set_value T2;
          Lwt.return_unit)
      () in
  let b_t = Button.make
      ~appearance:Raised
      ~label:"SET T"
      ~on_click:(fun _ _ _ ->
          select#set_value T;
          select_2#set_value T;
          Lwt.return_unit)
      () in
  let b_c = Button.make
      ~appearance:Raised
      ~label:"SET C"
      ~on_click:(fun _ _ _ ->
          select#set_value C;
          select_2#set_value C;
          Lwt.return_unit)
      () in
  let wrapper ?(padding = 20) w =
    let div = Widget.create_div ~widgets:[w] () in
    div#root##.style##.padding := Utils.px_js padding;
    div in
  Box.make ~dir:`Column
    [ wrapper select
    ; wrapper select_2
    ; wrapper ~padding:5 b_t2
    ; wrapper ~padding:5 b_t
    ; wrapper ~padding:5 b_c ]
