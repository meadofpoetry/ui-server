open Containers
open Components
open Common
open Board_types.Streams.TS
open Lwt_result.Infix
open Api_js.Api_types
open Ui_templates.Sdom

type config =
  { stream : Stream.t
  }

let name = "Сервисы"

let settings = None

let base_class = "qos-niit-services-overview"

module Service = struct

  module Id = Int

  type model = service_info [@@deriving eq]
  type widget = service_info Item_list.Item.t

  let id_of_model = fun (x:model) -> x.id
  let widget      = fun w -> w#widget

  let make (init:model) =
    let open Printf in
    let graphic = new Icon.SVG.t ~icon:Tv () in
    let prev    = ref init in
    let leaf, update_primary, update_secondary =
      let to_primary x = x in
      let to_secondary x = sprintf "ID: %d" x in
      let w = new Item_list.Item.t
                ~text:""
                ~secondary_text:""
                ~graphic
                ~value:init () in
      let v_primary =
        { get = (fun (x:model) -> x.name)
        ; eq  = String.equal
        ; upd = Fun.(w#set_text % to_primary) } in
      let v_secondary =
        { get = (fun (x:model) -> x.id)
        ; eq  = (=)
        ; upd = (fun id ->
          let s = to_secondary id in
          w#set_secondary_text s) } in
      w, v_primary, v_secondary in
    let update = fun ?previous (model:model) ->
      leaf#set_value model;
      setter ?previous model update_primary;
      setter ?previous model update_secondary in
    update init;
    leaf, fun x -> update ~previous:!prev x

end

module Services =
  Make_array(struct
      type widget = service_info Item_list.t
      module Node = Service

      let root (w:widget) = w#root
      let append_child (w:widget) (i:Node.widget) =
        i#listen Widget.Event.click (fun _ _ -> w#set_active i; true)
        |> ignore;
        w#append_item i
      let insert_child_at_idx (w:widget) idx (i:Node.widget) =
        i#listen Widget.Event.click (fun _ _ -> w#set_active i; true)
        |> ignore;
        w#insert_item_at_idx idx i
      let remove_child (w:widget) (i:Node.widget) =
        w#remove_item i
      let make nodes =
        let items = List.map (fun x -> let w, _ = Node.make x in
                                       `Item w) nodes in
        let tree  = new Item_list.t ~two_line:true ~items () in
        let ()    = tree#set_dense true in
        tree, fun _ -> ()
    end)

let make_list (init:service_info list)
      (event:service_info list React.event) =
  let event =
    React.S.diff (fun n o -> o, n)
    @@ React.S.hold ~eq:(Equal.list equal_service_info) init event in
  let list, update_list = Services.make init in
  let _e = React.E.map (fun (prev, model) ->
               if not @@ (Equal.list equal_service_info) prev model
               then update_list model) event in
  list

let make_info () =
  let _class = Markup.CSS.add_element base_class "info" in
  let id, id_lbl, set_id =
    let wdg = new Typography.Text.t ~text:"" () in
    let lbl = new Typography.Text.t ~text:"Service ID:" () in
    let set = fun x -> wdg#set_text @@ Printf.sprintf "%d" x in
    wdg, lbl, set in
  let pmt, pmt_lbl, set_pmt =
    let wdg = new Typography.Text.t ~text:"" () in
    let lbl = new Typography.Text.t ~text:"PMT PID:" () in
    let set = fun x -> wdg#set_text @@ Printf.sprintf "%d" x in
    wdg, lbl, set in
  let pcr, pcr_lbl, set_pcr =
    let wdg = new Typography.Text.t ~text:"" () in
    let lbl = new Typography.Text.t ~text:"PCR PID:" () in
    let set = fun x -> wdg#set_text @@ Printf.sprintf "%d" x in
    wdg, lbl, set in
  let to_row wdg lbl =
    let row_class  = Markup.CSS.add_element base_class "info-row" in
    let cell_class = Markup.CSS.add_element base_class "info-cell" in
    let box = new Hbox.t ~widgets:[ lbl; wdg ] () in
    box#add_class row_class;
    wdg#add_class cell_class;
    lbl#add_class cell_class;
    box in
  let box = new Vbox.t ~widgets:[ to_row id id_lbl
                                ; to_row pmt pmt_lbl
                                ; to_row pcr pcr_lbl ] () in
  box#add_class _class;
  box, fun (x:service_info) ->
       set_id x.id;
       set_pmt x.pmt_pid;
       set_pcr x.pcr_pid

let make_details (init:service_info list)
      (event:service_info list React.event) =
  let list = make_list init event in
  let info, set_info = make_info () in
  let _s = React.S.map (function Some x -> set_info x#value
                               | None   -> ()) list#s_active in
  object
    inherit Hbox.t ~widgets:[ list#widget; info#widget ] ()
    method list = list
  end

let get_service_bitrate
      (br:(int * int) list)
      (s:service_info) =
  let ecm = List.fold_left (fun acc (x:ecm_info) ->
                match List.Assoc.get ~eq:(=) x.pid br with
                | None   -> acc
                | Some x -> x + acc) 0 s.ecm in
  let es  = List.fold_left (fun acc (x:es_info) ->
                match List.Assoc.get ~eq:(=) x.pid br with
                | None   -> acc
                | Some x -> x + acc) 0 s.es in
  let pmt = Option.get_or ~default:0
            @@ List.Assoc.get ~eq:(=) s.pmt_pid br in
  ecm + es + pmt

let make_table (init:service_info list)
      (event:service_info list React.event)
      (bitrate:bitrate React.event) =
  let is_hex     = false in
  let hex_id_fmt = Some (Printf.sprintf "0x%04X") in
  let br_fmt     = Table.(Option (Float None, "-")) in
  let pct_fmt = Table.(Option (Float (Some (Printf.sprintf "%.2f")), "-")) in
  let fmt =
    let open Table in
    let open Format in
    (   to_column "ID", if is_hex then Int hex_id_fmt else Int None)
    :: (to_column "Сервис", String None)
    :: (to_column "PMT PID", if is_hex then Int hex_id_fmt else Int None)
    :: (to_column "PCR PID", if is_hex then Int hex_id_fmt else Int None)
    :: (to_column "Битрейт, Мбит/с", br_fmt)
    :: (to_column "%", pct_fmt)
    :: (to_column "Max, Мбит/с", br_fmt)
    :: (to_column "Min, Мбит/с", br_fmt)
    :: [] in
  let table   = new Table.t ~dense:true ~fmt () in
  let details = make_details init event in
  let on_change = fun (x:bool) ->
    List.iter (fun row ->
        let open Table in
        match row#cells with
        | id :: _ :: pmt :: pcr :: _ ->
           let fmt = if x then Int hex_id_fmt else Int None in
           id#set_format  fmt;
           pmt#set_format fmt;
           pcr#set_format fmt)
      table#rows in
  let back_ico = new Icon.Button.Font.t ~icon:"arrow_back" () in
  let back_txt = new Typography.Text.t ~adjust_margin:false
                   ~font:Caption
                   ~text:"Назад" () in
  let back    = new Hbox.t
                  ~valign:`Center
                  ~widgets:[ back_ico#widget; back_txt#widget ] () in
  let ()      = back#add_class @@ Markup.CSS.add_element base_class "back" in
  let switch  = new Switch.t ~state:is_hex ~on_change () in
  let hex     = new Form_field.t ~input:switch ~label:"HEX IDs" () in
  let actions = new Card.Actions.t ~widgets:[ back#widget; hex#widget ] () in
  let media   = new Card.Media.t ~widgets:[ table ] () in
  let card    = new Card.t ~widgets:[ actions#widget
                                    ; (new Divider.t ())#widget
                                    ; media#widget ] () in
  let hide w = w#style##.visibility := Js.string "hidden" in
  let show w = w#style##.visibility := Js.string "" in
  back#listen Widget.Event.click (fun _ _ ->
      media#append_child table;
      media#remove_child details;
      show hex; hide back;
      true) |> ignore;
  hide back;
  let add_row (x:service_info) =
    let row = table#add_row (
                  x.id :: x.name :: x.pmt_pid :: x.pcr_pid
                  :: None :: None :: None :: None :: []) in
    row#listen Widget.Event.click (fun _ _ ->
        show back; hide hex;
        media#remove_child table;
        media#append_child details;
        (match List.find_opt (fun i -> equal_service_info i#value x)
                 details#list#items with
         | Some item -> details#list#set_active item;
                        item#root##scrollIntoView Js._true;
         | None      -> ());
        true) |> ignore in
  let _ =
    React.E.map (fun (bitrate:bitrate) ->
        List.iter (fun row ->
            let open Table in
            let id, cur, per, min, max =
              match row#cells with
              | id :: _ :: _ :: _ :: a :: b :: c :: d :: _ ->
                 id, a, b, c, d in
            let br = get_service_bitrate bitrate.pids
                       (List.find (fun (x:service_info) ->
                            x.id = id#value) init) in
            let pct = 100. *. (float_of_int br)
                      /. (float_of_int bitrate.total) in
            let br = (float_of_int br) /. 1_000_000. in
            cur#set_value @@ Some br;
            per#set_value @@ Some pct;
            (match min#value with
             | None -> min#set_value (Some br)
             | Some v -> if br <. v then min#set_value (Some br));
            (match max#value with
             | None -> max#set_value (Some br)
             | Some v -> if br >. v then max#set_value (Some br)))
          table#rows;
        bitrate) bitrate in
  List.iter Fun.(ignore % add_row) init;
  let () = card#add_class base_class in
  card

let make ~(config:config) control =
  let id   = match config.stream.id with
    | `Ts id -> id
    | `Ip _  -> failwith "UDP" in
  let init =
    Requests.Streams.HTTP.get_services ~id ~limit:1 control
    >>= (function
         | Raw s -> Lwt_result.return s.data
         | _     -> Lwt.fail_with "got compressed") in
  let loader =
    init
    >|= (fun init ->
      let e_br, br_sock = Requests.Streams.WS.get_bitrate ~id control in
      let services = match List.head_opt init with
        | Some (_, services) -> services.services
        | None -> [] in
      make_table services React.E.never e_br)
    >|= Widget.coerce
    |> Lwt_result.map_err Api_js.Requests.err_to_string
    |> Ui_templates.Loader.create_widget_loader
  in loader


