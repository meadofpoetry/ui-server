module Channel : sig

end

module Errors : sig

  val get_video_data_for_stream  : Common.Stream.t -> Qoe_errors.Video_data.t
  val get_video_data_for_channel : Common.Stream.t -> Structure.channel -> Qoe_errors.Video_data.t
  val get_video_data_for_pid     : Common.Stream.t -> Structure.channel -> int -> Qoe_errors.Video_data.t

  val get_audio_data_for_stream  : Common.Stream.t -> Qoe_errors.Audio_data.t
  val get_audio_data_for_channel : Common.Stream.t -> Structure.channel -> Qoe_errors.Audio_data.t
  val get_audio_data_for_pid     : Common.Stream.t -> Structure.channel -> int -> Qoe_errors.Audio_data.t

end
