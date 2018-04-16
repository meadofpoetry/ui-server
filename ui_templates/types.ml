open Components

type ('a,'b) settings_block = Widget.widget * 'a option React.signal * ('a -> ('b,string) Lwt_result.t)
