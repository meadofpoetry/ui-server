type axis_value_js

type cubic_interpolation_mode = Default | Monotone
type stepped_line             = Disabled | Before | After
type fill                     = Disabled
                              | Start
                              | End
                              | Origin
                              | Absolute_index of int
                              | Relative_index of int

type ('a,'b) point =
  { x : 'a
  ; y : 'b
  }
type ('a,'b) dataset =
  { data  : ('a,'b) point list
  ; label : string
  }

type bool_or_string
type 'a or_array
type ('a,'b,'c) point_setting =
  [ `Val of 'c
  | `Lst of ' c list
  | `Fun of (int -> ('a,'b) point -> 'c) ]
