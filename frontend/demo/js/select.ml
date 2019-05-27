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

let make () =
  let items = Select.native_options_of_values
      ~with_empty:true
      standard
      [T2; T; C] in
  Select.make_native
    ~label:"Стандарт"
    ~items
    standard

let section () =
  let select = make () in
  let b_t2 = Button.make
      ~appearance:Raised
      ~label:"SET T2"
      ~on_click:(fun _ _ ->
          select#set_value T2;
          Lwt.return_unit)
      () in
  let b_t = Button.make
      ~appearance:Raised
      ~label:"SET T"
      ~on_click:(fun _ _ ->
          select#set_value T;
          Lwt.return_unit)
      () in
  let b_c = Button.make
      ~appearance:Raised
      ~label:"SET C"
      ~on_click:(fun _ _ ->
          select#set_value C;
          Lwt.return_unit)
      () in
  Box.make ~dir:`Column
    [ select#widget
    ; b_t2#widget
    ; b_t#widget
    ; b_c#widget ]
