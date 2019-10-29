open Components
include Board_niitv_tsan_widgets_tyxml.Pid_summary
module D = Make (Impl.Xml) (Impl.Svg) (Impl.Html)
module R = Make (Impl.R.Xml) (Impl.R.Svg) (Impl.R.Html)
