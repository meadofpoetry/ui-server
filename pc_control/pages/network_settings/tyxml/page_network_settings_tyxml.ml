module CSS = struct
  include Common.CSS
end

module Dns = Dns
module Ethernet = Ethernet
module Ipv4 = Ipv4
module Routes = Routes

module Make
    (Xml : Xml_sigs.NoWrap)
    (Svg : Svg_sigs.NoWrap with module Xml := Xml)
    (Html : Html_sigs.NoWrap with module Xml := Xml and module Svg := Svg) =
struct
  include Common.Make (Xml) (Svg) (Html)
end

module Markup = Make (Tyxml.Xml) (Tyxml.Svg) (Tyxml.Html)
