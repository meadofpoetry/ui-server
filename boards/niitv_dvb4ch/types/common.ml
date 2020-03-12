type 'a ts = { data : 'a; timestamp : Time.t } [@@deriving yojson, show, eq]
