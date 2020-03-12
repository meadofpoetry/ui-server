open Components
open Application_types
module Storage = Ui_templates.Storage.Local_storage

let (key : string) = "pipeline-page-charts"

let make_default (stream_id : Stream.ID.t) :
    Widget_factory.item Dashboard.Item.positioned_item list =
  let config typ : Widget_parameter_chart.widget_config =
    {
      duration = Time.Span.of_int_s 120;
      typ;
      sources = [];
      filter = [ { stream_id; services = [] } ];
      settings = None;
    }
  in
  [
    {
      item = Chart (Some (config `Black));
      position = { x = 0; y = 0; w = 4; h = 3 };
    };
    {
      item = Chart (Some (config `Luma));
      position = { x = 0; y = 3; w = 4; h = 3 };
    };
    {
      item = Chart (Some (config `Freeze));
      position = { x = 0; y = 6; w = 4; h = 3 };
    };
    {
      item = Chart (Some (config `Diff));
      position = { x = 0; y = 9; w = 4; h = 3 };
    };
    {
      item = Chart (Some (config `Blocky));
      position = { x = 0; y = 12; w = 4; h = 3 };
    };
    {
      item = Chart (Some (config `Silence_shortt));
      position = { x = 0; y = 15; w = 4; h = 3 };
    };
    {
      item = Chart (Some (config `Silence_moment));
      position = { x = 0; y = 18; w = 4; h = 3 };
    };
  ]

let make (stream : Stream.ID.t) () =
  let factory = new Widget_factory.t () in
  new Dashboard.t
    ?init:(Option.map (fun x -> Dashboard.Serialized x) @@ Storage.get key)
    ~default:(Items (make_default stream))
    ~on_edit:(Storage.put key)
    ~edit_caps:(Partial { add = false; remove = false })
    factory ()
