open Common

let stream_assoc_of_yojson _of =
  Json.(List.of_yojson (Pair.of_yojson Stream.ID.of_yojson _of))
