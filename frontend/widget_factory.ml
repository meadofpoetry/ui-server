open Containers
open Components

type 'a item =
  | Dvb_niit : 'a Board_dvb_niit_js.Widget_factory.item -> Widget.t item
