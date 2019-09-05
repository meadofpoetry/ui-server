module Make (Xml : Xml_sigs.NoWrap)
    (Svg : Svg_sigs.NoWrap with module Xml := Xml)
    (Html : Html_sigs.NoWrap with module Xml := Xml
                              and module Svg := Svg) = struct
  module Box = struct
    include Box
    include Box.Make(Xml)(Svg)(Html)
  end

  module Button = struct
    include Button
    include Button.Make(Xml)(Svg)(Html)
  end

  module Card = struct
    include Card
    include Card.Make(Xml)(Svg)(Html)
  end

  module Checkbox = struct
    include Checkbox
    include Checkbox.Make(Xml)(Svg)(Html)
  end

  module Circular_progress = struct
    include Circular_progress
    include Circular_progress.Make(Xml)(Svg)(Html)
  end

  module Data_table = struct
    include Data_table
    include Data_table.Make(Xml)(Svg)(Html)
  end

  module Dialog = struct
    include Dialog
    include Dialog.Make(Xml)(Svg)(Html)
  end

  module Divider = struct
    include Divider
    include Divider.Make(Xml)(Svg)(Html)
  end

  module Drawer = struct
    include Drawer
    include Drawer.Make(Xml)(Svg)(Html)
  end

  module Fab = struct
    include Fab
    include Fab.Make(Xml)(Svg)(Html)
  end

  module Floating_label = struct
    include Floating_label
    include Floating_label.Make(Xml)(Svg)(Html)
  end

  module Form_field = struct
    include Form_field
    include Form_field.Make(Xml)(Svg)(Html)
  end

  module Icon_button = struct
    include Icon_button
    include Icon_button.Make(Xml)(Svg)(Html)
  end

  module Icon = struct
    include Icon
    include Icon.Make(Xml)(Svg)(Html)
  end

  module Item_list = struct
    include Item_list
    include Item_list.Make(Xml)(Svg)(Html)
  end

  module Layout_grid = struct
    include Layout_grid
    include Layout_grid.Make(Xml)(Svg)(Html)
  end

  module Linear_progress = struct
    include Linear_progress
    include Linear_progress.Make(Xml)(Svg)(Html)
  end

  module Line_ripple = struct
    include Line_ripple
    include Line_ripple.Make(Xml)(Svg)(Html)
  end

  module Menu = struct
    include Menu
    include Menu.Make(Xml)(Svg)(Html)
  end

  module Menu_surface = struct
    include Menu_surface
    include Menu_surface.Make(Xml)(Svg)(Html)
  end

  module Notched_outline = struct
    include Notched_outline
    include Notched_outline.Make(Xml)(Svg)(Html)
  end

  module Radio = struct
    include Radio
    include Radio.Make(Xml)(Svg)(Html)
  end

  module Ripple = struct
    include Ripple
    include Ripple.Make(Xml)(Svg)(Html)
  end

  module Scaffold = struct
    include Scaffold
    include Scaffold.Make(Xml)(Svg)(Html)
  end

  module Select = struct
    include Select
    include Select.Make(Xml)(Svg)(Html)
  end

  module Side_sheet = struct
    include Side_sheet
    include Side_sheet.Make(Xml)(Svg)(Html)
  end

  module Slider = struct
    include Slider
    include Slider.Make(Xml)(Svg)(Html)
  end

  module Snackbar = struct
    include Snackbar
    include Snackbar.Make(Xml)(Svg)(Html)
  end

  module Switch = struct
    include Switch
    include Switch.Make(Xml)(Svg)(Html)
  end

  module Tab_bar = struct
    include Tab_bar
    include Tab_bar.Make(Xml)(Svg)(Html)
  end

  module Tab_indicator = struct
    include Tab_indicator
    include Tab_indicator.Make(Xml)(Svg)(Html)
  end

  module Tab = struct
    include Tab
    include Tab.Make(Xml)(Svg)(Html)
  end

  module Tab_scroller = struct
    include Tab_scroller
    include Tab_scroller.Make(Xml)(Svg)(Html)
  end

  module Textfield = struct
    include Textfield
    include Textfield.Make(Xml)(Svg)(Html)
  end

  module Top_app_bar = struct
    include Top_app_bar
    include Top_app_bar.Make(Xml)(Svg)(Html)
  end

  module Treeview = struct
    include Treeview
    include Treeview.Make(Xml)(Svg)(Html)
  end

  module Typography = struct
    include Typography
    include Typography.Make(Xml)(Svg)(Html)
  end
end

include Make(Tyxml.Xml)(Tyxml.Svg)(Tyxml.Html)
