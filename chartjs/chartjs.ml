open Types

module Axes = Axes
module Options = Options
module Config = Config

(** Chart types *)
module Pie = Pie
module Doughnut = Doughnut

module Line = struct

  module Dataset = struct
    include Line.Dataset

    let coerce : t -> Config.dataset = Obj.magic
  end

  include (Line : module type of Line with module Dataset := Dataset)

end

include Chart
