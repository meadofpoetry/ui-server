open Js_of_ocaml
open Js_of_ocaml_tyxml
open Components
open Board_niitv_tsan_types
include Board_niitv_tsan_widgets_tyxml.Pid_bitrate_pie_chart
module Markup_js = Make (Tyxml_js.Xml) (Tyxml_js.Svg) (Tyxml_js.Html)

module Selector = struct
  let canvas = "canvas"
end

type event = [`Bitrate of Bitrate.t option]

let ( % ) f g x = f (g x)

let other = "Другие"

let colors =
  let open Material_color_palette in
  [| Red C500
   ; Orange C500
   ; Green C500
   ; Blue C500
   ; Purple C500
   ; Grey C500
   ; Brown C500
   ; Pink C500
   ; Blue_grey C500
   ; Deep_purple C500
   ; Deep_orange C500
   ; Indigo C500
   ; Amber C500
   ; Light_blue C500 |]

let make_pie_datalabels () =
  let open Chartjs_datalabels in
  let color (context : optionContext Js.t) =
    Chartjs.Color.of_string
    @@ Color.to_css_rgba
    @@ Color.text_color
    @@ Material_color_palette.make colors.(context##.dataIndex)
  in
  let display (context : optionContext Js.t) =
    let (dataset : float Chartjs.pieDataset Js.t) = Js.Unsafe.coerce context##.dataset in
    let callback = Js.wrap_callback (fun acc x _ _ -> acc +. x) in
    let sum = dataset##.data##reduce callback in
    let v = Js.array_get dataset##.data context##.dataIndex in
    Js.Optdef.case
      v
      (fun () -> Visibility.of_bool false)
      (fun v ->
        let pct = v *. 100. /. sum in
        Visibility.of_bool (pct > 5.))
  in
  let formatter _ (context : optionContext Js.t) =
    let data = context##.chart##.data in
    Js.Optdef.case
      data##.labels
      (fun () -> Js.string "")
      (fun labels ->
        Js.Optdef.get (Js.array_get labels context##.dataIndex) (fun () -> Js.string ""))
  in
  let font = empty_font () in
  font##.weight := Js.string "bold";
  let datalabels = empty_datalabels_config () in
  datalabels##.font := Chartjs.Scriptable_indexable.of_single font;
  datalabels##.offset := Chartjs.Scriptable_indexable.of_single (-50.);
  datalabels##.clip := Chartjs.Scriptable_indexable.of_single Js._true;
  datalabels##.color := Chartjs.Scriptable_indexable.of_fun color;
  datalabels##.display := Chartjs.Scriptable_indexable.of_fun display;
  datalabels##.formatter := Js.some @@ Js.wrap_callback formatter;
  datalabels

let label_callback _tooltip item (data : Chartjs.data Js.t) =
  Js.Optdef.case
    (Js.array_get data##.datasets item##.datasetIndex)
    (fun () -> assert false)
    (fun dataset ->
      let (ds : float Chartjs.pieDataset Js.t) = Js.Unsafe.coerce dataset in
      let value =
        Js.Optdef.get (Js.array_get ds##.data item##.index) (fun () -> assert false)
      in
      let label =
        Js.Optdef.(
          bind data##.labels (fun x -> Js.array_get x item##.index)
          |> (fun x -> map x Js.to_string)
          |> (fun x -> Js.Optdef.get x (fun () -> assert false))
          |> function
          | s when String.equal s other -> s
          | s -> Printf.sprintf "PID %s" s)
      in
      Chartjs.Indexable.of_single
      @@ Js.string
      @@ Printf.sprintf "%s: %.3g Мбит/с" label value)

let make_pie_options () =
  let open Chartjs in
  let hover = empty_hover () in
  hover##.animationDuration := 0;
  let tooltip_callbacks = empty_tooltip_callbacks () in
  let tooltips = empty_tooltip () in
  tooltip_callbacks##.label := Js.def @@ Js.wrap_meth_callback label_callback;
  tooltips##.callbacks := tooltip_callbacks;
  let legend = empty_legend () in
  legend##.position := Position.left;
  legend##.display := Js._false;
  let animation = empty_pie_animation () in
  animation##.animateRotate := Js._false;
  let plugins = Js.Unsafe.obj [||] in
  let datalabels = make_pie_datalabels () in
  plugins##.datalabels := datalabels;
  let options = empty_pie_options () in
  options##.responsive := Js._true;
  options##.maintainAspectRatio := Js._true;
  options##.aspectRatio := 1.;
  options##.animation := animation;
  options##.tooltips := tooltips;
  options##.legend := legend;
  options##.hover := hover;
  options##.plugins := plugins;
  options

let make_pie_dataset () =
  let background_color =
    Chartjs.Scriptable_indexable.of_array
    @@ Array.map
         (Chartjs.Color.of_string % Color.to_css_rgba % Material_color_palette.make)
         colors
  in
  let dataset = Chartjs.empty_pie_dataset () in
  dataset##.backgroundColor := background_color;
  dataset##.borderColor := background_color;
  dataset

let make_pie ?(canvas = Dom_html.(createCanvas document)) () =
  let dataset = make_pie_dataset () in
  let options = make_pie_options () in
  let data = Chartjs.empty_data () in
  data##.datasets := Js.array [|dataset|];
  Chartjs.chart_from_canvas Chartjs.Chart.pie data options canvas

let name = "PID bitrate pie chart"

let title = "Битрейт"

let map_rate {Bitrate.total; pids; _} =
  let pids = List.sort (fun a b -> compare (fst a) (fst b)) pids in
  let br =
    List.fold_left
      (fun acc (pid, br) ->
        let open Float in
        let pct = 100. *. of_int br /. of_int total in
        let br = of_int br /. 1_000_000. in
        (pid, (br, pct)) :: acc)
      []
      pids
  in
  List.fold_left
    (fun (pids, oth) (pid, (br, pct)) ->
      if pct > 1. then (pid, br) :: pids, oth else pids, br :: oth)
    ([], [])
    br

class t ?(hex = false) ?rate (elt : Dom_html.element Js.t) =
  object (self)
    val mutable _hex = hex

    val mutable _rate = None

    val pie =
      Js.Opt.case
        (Dom_html.CoerceTo.canvas (Element.query_selector_exn elt Selector.canvas))
        (fun () -> failwith (name ^ ": `canvas` element not found"))
        (fun canvas -> make_pie ~canvas ())

    inherit Widget.t elt () as super

    method! init () : unit =
      self#set_hex _hex;
      self#set_rate rate;
      super#init ()

    method! destroy () : unit =
      pie##destroy;
      super#destroy ()

    method set_hex (x : bool) : unit =
      _hex <- x;
      match _rate with
      | None -> ()
      | Some (pids, oth) ->
          pie##.data##.labels := self#make_labels pids oth;
          pie##update

    method set_rate : Bitrate.t option -> unit =
      function
      | None ->
          _rate <- None;
          self#dataset##.hidden := Js._true;
          pie##update
      | Some rate ->
          let pids, oth = map_rate rate in
          self#dataset##.hidden := Js._false;
          _rate <- Some (pids, oth);
          let data =
            let pids = List.map snd pids in
            match oth with
            | [] -> pids
            | l -> pids @ [List.fold_left ( +. ) 0. l]
          in
          pie##.data##.labels := self#make_labels pids oth;
          self#dataset##.data := Js.array @@ Array.of_list data;
          pie##update

    method notify : event -> unit =
      function
      | `Bitrate x -> self#set_rate x

    (* Private methods *)
    method private dataset : float Chartjs.pieDataset Js.t =
      Js.array_get pie##.data##.datasets 0
      |> (fun x -> Js.Optdef.map x Js.Unsafe.coerce)
      |> fun x -> Js.Optdef.get x (fun () -> failwith (name ^ ": dataset not found"))

    method private make_labels pids oth : Js.js_string Js.t Js.js_array Js.t =
      let to_string = if _hex then Util.pid_to_hex_string else Util.pid_to_dec_string in
      let pids = List.map (Js.string % to_string % fst) pids in
      Js.array
      @@ Array.of_list
      @@
      match oth with
      | [] -> pids
      | _ -> pids @ [Js.string other]
  end

let make ?hex ?rate () : t =
  let title = Markup_js.create_title title in
  let canvas = Tyxml_js.Html.canvas [] in
  let elt = Tyxml_js.To_dom.of_element @@ Markup_js.create ~title ~canvas () in
  let t = new t ?hex ?rate elt in
  t

let attach ?hex ?rate (elt : #Dom_html.element Js.t) : t =
  new t ?hex ?rate (elt :> Dom_html.element Js.t)
