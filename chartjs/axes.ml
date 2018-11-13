open Containers
open Scales
open Types

module Scale_label = Scale_label
module Grid_lines = Grid_lines

module Cartesian = struct

  module Category = struct

    module Ticks = struct
      include Ticks
      include (Category.Ticks : module type of Category.Ticks
                                               with type t := Ticks.t)
    end

    include Cartesian
    let make = make ~type_:"category"
  end

  module Linear = struct

    module Ticks = struct
      include Ticks
      include (Linear.Ticks : module type of Linear.Ticks
                                             with type t := Ticks.t)
    end

    include Cartesian
    let make = make ~type_:"linear"

  end

  module Logarithmic = struct

    module Ticks = struct
      include Ticks
      include (Logarithmic.Ticks : module type of Logarithmic.Ticks
                                                  with type t := Ticks.t)
    end

    include Cartesian
    let make = make ~type_:"logarithmic"
  end

  module Time = struct

    module Ticks = struct
      include Ticks
      include (Time.Ticks : module type of Time.Ticks
                                           with type t := Ticks.t)
    end

    include Cartesian
    include (Time : module type of Time with type t := Cartesian.t
                                   with module Ticks := Ticks)
    let make = make ~type_:"time"
  end

end

module Radial = struct

end

let make = make
