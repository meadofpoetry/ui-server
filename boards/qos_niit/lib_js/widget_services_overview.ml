open Containers
open Components
open Common
open Board_types.Streams.TS
open Lwt_result.Infix
open Api_js.Api_types
open Ui_templates.Sdom

let base_class = "qos-niit-services-overview"

let get_service_bitrate (br:(int * int) list) (s:service_info) =
  let ecm =
    List.fold_left (fun acc (x:ecm_info) ->
        match List.Assoc.get ~eq:(=) x.pid br with
        | None   -> acc
        | Some b -> (x.pid, b) :: acc) [] s.ecm in
  let es  =
    List.fold_left (fun acc (x:es_info) ->
        match List.Assoc.get ~eq:(=) x.pid br with
        | None   -> acc
        | Some b -> (x.pid, b) :: acc) [] s.es in
  let pmt = match s.has_pmt with
    | false -> None
    | true  -> Some (s.pmt_pid,
                     Option.get_or ~default:0
                     @@ List.Assoc.get ~eq:(=) s.pmt_pid br) in
  List.cons_maybe pmt (es @ ecm)

let sum_bitrate rate =
  List.fold_left (fun acc x -> acc + snd x) 0 rate

let acc_bitrate total rate =
  let sum  = sum_bitrate rate in
  let pct  = Float.(100. * (of_int sum / of_int total)) in
  let br_f = Float.(of_int sum / 1_000_000.) in
  sum, br_f, pct

let make_back () =
  let back_ico = new Icon.Button.Font.t ~icon:"arrow_back" () in
  let back_txt =
    new Typography.Text.t
      ~adjust_margin:false
      ~font:Caption
      ~text:"Назад" () in
  let back =
    new Hbox.t
      ~valign:`Center
      ~widgets:[ back_ico#widget; back_txt#widget ] () in
  back#add_class @@ Markup.CSS.add_element base_class "back";
  back

let br_to_string = function
  | Some l -> List.fold_left (fun acc x -> acc + snd x) 0 l
              |> (fun x -> Float.(of_int x /. 1_000_000.))
              |> Printf.sprintf "%f"
  | None   -> "-"

let br_fmt =
  let open Table in
  { to_string  = br_to_string
  ; compare    = Ord.(option @@ list @@ pair Int.compare Int.compare)
  ; is_numeric = true
  }

let min_fmt =
  let open Table in
  { to_string = br_to_string
  ; compare   = (fun l1 l2 ->
    let sum = sum_bitrate in
    let cmp x1 x2 = match Int.compare (sum x1) (sum x2) with
      | -1 | 0 -> 0
      | _      -> 1 in
    (Ord.option cmp) l1 l2)
  ; is_numeric = true
  }

let max_fmt =
  let open Table in
  { to_string = br_to_string
  ; compare   = (fun prev cur ->
    let sum = sum_bitrate in
    let cmp prev cur = match Int.compare (sum prev) (sum cur) with
      | 1 | 0 -> 0
      | _     -> 1 in
    (Ord.option cmp) prev cur)
  ; is_numeric = true
  }

let make_table
      (is_hex:bool)
      (init:service_info list)
      (event:service_info list React.event) =
  let hex_id_fmt = Some (Printf.sprintf "0x%04X") in
  let pct_fmt    = Table.(Option (Float (Some (Printf.sprintf "%.2f")), "-")) in
  let fmt =
    let open Table in
    let open Format in
    (   to_column ~sortable:true "ID",              Int None)
    :: (to_column ~sortable:true "Сервис",          String None)
    :: (to_column ~sortable:true "PMT PID",         Int None)
    :: (to_column ~sortable:true "PCR PID",         Int None)
    :: (to_column ~sortable:true "Битрейт, Мбит/с", Custom br_fmt)
    :: (to_column ~sortable:true "%",               pct_fmt)
    :: (to_column ~sortable:true "Min, Мбит/с",     Custom min_fmt)
    :: (to_column ~sortable:true "Max, Мбит/с",     Custom max_fmt)
    :: [] in
  let table     = new Table.t ~dense:true ~fmt () in
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
  if is_hex then on_change true;
  table, on_change

let map_details (details:Widget_service_info.t option ref)
      (info:service_info) =
  match !details with
  | Some x when equal_service_info x#info info -> Some x
  | _ -> None

let set_bitrate init (rate:bitrate) details' row =
  let open Table in
  let id, cur, per, min, max = match row#cells with
    | id :: _ :: _ :: _ :: a :: b :: c :: d :: _ -> id, a, b, c, d in
  match List.find_opt (fun (x:service_info) -> x.id = id#value) init with
  | None -> ()
  | Some info ->
     let open Option in
     let lst = get_service_bitrate rate.pids info in
     let br, br_f, pct = acc_bitrate rate.total lst in
     let details = map_details details' info in
     cur#set_value @@ Some lst;
     per#set_value @@ Some pct;
     min#set_value @@ Some lst;
     max#set_value @@ Some lst;
     iter (fun x -> x#set_rate @@ Some lst) details

let make_card
      (init:service_info list)
      (pids:pid_info list)
      (event:service_info list React.event)
      (bitrate:bitrate React.event) =
  let is_hex  = false in
  let table, on_change = make_table is_hex init event in
  let actions = new Card.Actions.t ~widgets:[ ] () in
  let media   = new Card.Media.t ~widgets:[ table ] () in
  let card    =
    new Card.t ~widgets:[ actions#widget
                        ; (new Divider.t ())#widget
                        ; media#widget ] () in
  let details' = ref None in
  let add_row (x:service_info) =
    let row =
      table#add_row (x.id :: x.name :: x.pmt_pid :: x.pcr_pid
                     :: None :: None :: None :: None :: []) in
    row#listen_lwt Widget.Event.click (fun _ _ ->
        let open Lwt.Infix in
        let rate, min, max =
          let open Table in
          match row#cells with
          | _ :: _ :: _ :: _ :: a :: _ :: b :: c :: _ ->
             a#value, b#value, c#value in
        let back    = make_back () in
        let details = Widget_service_info.make ?rate ?min ?max x pids in
        details' := Some details;
        back#listen_once_lwt Widget.Event.click
        >|= (fun _ -> media#append_child table;
                      media#remove_child details;
                      actions#remove_child back)
        |> Lwt.ignore_result;
        actions#insert_child_at_idx 0 back;
        media#remove_child table;
        media#append_child details;
        Lwt.return_unit)
    |> Lwt.ignore_result in
  let on_change = fun x ->
    Option.iter (fun d -> d#set_hex x) !details';
    on_change x in
  let switch  = new Switch.t ~state:is_hex ~on_change () in
  let hex     = new Form_field.t ~input:switch ~label:"HEX IDs" () in
  actions#append_child hex;
  let _ =
    React.E.map (fun (bitrate:bitrate) ->
        (* FIXME change init to actual list *)
        List.iter (set_bitrate init bitrate details') table#rows;
        bitrate) bitrate in
  List.iter Fun.(ignore % add_row) init;
  let () = card#add_class base_class in
  card

let make (init:service_info list)
      (pids:pid_info list)
      (bitrate:bitrate React.event) =
  make_card init pids React.E.never bitrate


