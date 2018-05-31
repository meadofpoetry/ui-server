open Common

let req_to_uri ?uri control req =
  Boards_js.Requests.req_to_uri ?uri ~conv:Api_utils.req_to_path control req
