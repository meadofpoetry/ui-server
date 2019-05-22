open Application_types
open Board_niitv_tsan_types

let name = "Настройки"

let make ~(state : Topology.state React.signal)
    ~(t2mi_mode : t2mi_mode React.signal)
    ~(streams : Stream.t list React.signal)
    (control : int) =
  Widget_t2mi_settings.make
    ~state
    ~streams
    ~mode:t2mi_mode
    control
