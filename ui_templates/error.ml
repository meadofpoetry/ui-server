open Containers
open Components

let create_error_block ?(icon="error") e =
  let _class = "ats-error-block" in
  let t  = new Typography.Text.t ~adjust_margin:false ~split:true ~text:e () in
  let i  = new Icon.Font.t ~icon () in
  let b  = new Box.t ~vertical:true ~widgets:[i#widget;t#widget] () in
  let () = b#add_class _class in
  b#widget
