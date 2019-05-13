module Show_RFC3339 : Netlib.Uri.Query.Show with type t = Time.t

module Show_float : Netlib.Uri.Query.Show with type t = Time.t

module Show : Netlib.Uri.Query.Show with type t = Time.t

module Show_period : sig
  module Hours : Netlib.Uri.Query.Show with type t = Time.Period.t
  module Seconds : Netlib.Uri.Query.Show with type t = Time.Period.t
  module Useconds : Netlib.Uri.Query.Show with type t = Time.Period.t
end

module Show_relative : Netlib.Uri.Query.Show with type t = Time.Relative.t

