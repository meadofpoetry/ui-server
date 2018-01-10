open Board_types
open Components

let t2mi_mode
      ~(init  : t2mi_mode)
      ~(event : t2mi_mode React.event) =
  let enabled = new Checkbox.t () in
  let pid     = new Textfield.t  ~input_type:(Integer (Some (0,8192))) ~label:"PID" () in
  let en_form = new Form_field.t ~input:enabled ~label:"Включить" () in
  enabled#set_checked init.enabled;
  let box     = new Box.t ~widgets:[ en_form#widget; pid#widget ] () in
  box
