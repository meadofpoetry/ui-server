open Components

let section () =
  let slider = Slider.make ~step:5. ~discrete:true ~markers:true () in
  let div = Widget.create_div ~widgets:[slider] () in
  div#add_class "slider-wrapper";
  div
