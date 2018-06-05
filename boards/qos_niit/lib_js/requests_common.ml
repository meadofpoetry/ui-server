open Common

let req_to_uri control req =
  Boards_js.Requests.req_to_uri ~conv:Api_utils.req_to_path control req
