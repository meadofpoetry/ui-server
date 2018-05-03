module Position = Dynamic_grid_position

type 'a item =
  { pos          : Position.t
  ; min_w        : int option
  ; min_h        : int option
  ; max_w        : int option
  ; max_h        : int option
  ; keep_ar      : bool
  ; resizable    : bool
  ; draggable    : bool
  ; selectable   : bool

  ; move_widget  : Widget.widget option
  ; close_widget : Widget.widget option
  ; widget       : Widget.widget option
  ; value        : 'a

  ; on_resize    : (Position.t -> Position.t -> int -> int -> unit) option
  ; on_resizing  : (Position.t -> Position.t -> int -> int -> unit) option
  ; on_drag      : (Position.t -> Position.t -> int -> int -> unit) option
  ; on_dragging  : (Position.t -> Position.t -> int -> int -> unit) option
  }

type grid =
  { min_col_width    : int
  ; max_col_width    : int option
  ; cols             : int
  ; rows             : int option
  ; row_height       : int option
  ; vertical_compact : bool
  ; items_margin     : int * int
  ; multi_select     : bool
  ; restrict_move    : bool
  }
