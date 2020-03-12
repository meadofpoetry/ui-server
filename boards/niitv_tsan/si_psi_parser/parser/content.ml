let name = "content_descriptor"

let parse_lvl1 = function
  | 0x0 -> "Undefined"
  | 0x1 -> "Movie/Drama"
  | 0x2 -> "News/Current affairs"
  | 0x3 -> "Show/Game show"
  | 0x4 -> "Sports"
  | 0x5 -> "Children's/Youth programmes"
  | 0x6 -> "Music/Ballet/Dance"
  | 0x7 -> "Arts/Culture (without music)"
  | 0x8 -> "Social/Political issues/Economics"
  | 0x9 -> "Education/Science/Factual topics"
  | 0xA -> "Leisure hobbies"
  | 0xB -> "Special characteristics"
  | 0xF -> "User defined"
  | 0xC | 0xD | 0xE -> "Reserved for future use"
  | _ -> assert false

let parse_lvl2 ~lvl_1 ~lvl_2 =
  match lvl_1 with
  | 0x0 -> (
      match lvl_2 with
      | x when x >= 0x0 && x <= 0xF -> "undefined content"
      | _ -> assert false )
  | 0x1 -> (
      match lvl_2 with
      | 0x0 -> "movie/drama (general)"
      | 0x1 -> "detective/thriller"
      | 0x2 -> "adventure/western/war"
      | 0x3 -> "science fiction/fantasy/horror"
      | 0x4 -> "comedy"
      | 0x5 -> "soap/melodrama/folkloric"
      | 0x6 -> "romance"
      | 0x7 -> "serious/classical/religious movie/drama"
      | 0x8 -> "adult movie/drama"
      | 0xF -> "user defined"
      | x when x >= 0x9 && x <= 0xE -> "reserved for future use"
      | _ -> assert false )
  | 0x2 -> (
      match lvl_2 with
      | 0x0 -> "news/current affairs (general)"
      | 0x1 -> "news/weather report"
      | 0x2 -> "news magazine"
      | 0x3 -> "documentary"
      | 0x4 -> "discussion/interview/debate"
      | 0xF -> "user defined"
      | x when x >= 0x5 && x <= 0xE -> "reserved for future use"
      | _ -> assert false )
  | 0x3 -> (
      match lvl_2 with
      | 0x0 -> "show/game show (general)"
      | 0x1 -> "game show/quiz/contest"
      | 0x2 -> "variety show"
      | 0x3 -> "talk show"
      | 0xF -> "user defined"
      | x when x >= 0x4 && x <= 0xE -> "reserved for future use"
      | _ -> assert false )
  | 0x4 -> (
      match lvl_2 with
      | 0x0 -> "sports (general)"
      | 0x1 -> "special events (Olympic Games, World Cup, etc.)"
      | 0x2 -> "sport magazines"
      | 0x3 -> "football/soccer"
      | 0x4 -> "tennis/squash"
      | 0x5 -> "team sports (excluding football)"
      | 0x6 -> "athletics"
      | 0x7 -> "motor sport"
      | 0x8 -> "water sport"
      | 0x9 -> "winter sports"
      | 0xA -> "equestrian"
      | 0xB -> "martial sports"
      | 0xF -> "user defined"
      | x when x >= 0xC && x <= 0xE -> "reserved for future use"
      | _ -> assert false )
  | 0x5 -> (
      match lvl_2 with
      | 0x0 -> "children's/youth programmes (general)"
      | 0x1 -> "pre-school children's programmes"
      | 0x2 -> "entertainment programmes for 6 to 14"
      | 0x3 -> "entertainment programmes for 10 to 16"
      | 0x4 -> "informational/educational/school programmes"
      | 0x5 -> "cartoons/puppets"
      | 0xF -> "user defined"
      | x when x >= 0x6 && x <= 0xE -> "reserved for future use"
      | _ -> assert false )
  | 0x6 -> (
      match lvl_2 with
      | 0x0 -> "music/ballet/dance (general)"
      | 0x1 -> "rock/pop"
      | 0x2 -> "serious music/classical music"
      | 0x3 -> "folk/traditional music"
      | 0x4 -> "jazz"
      | 0x5 -> "musical/opera"
      | 0x6 -> "ballet"
      | 0xF -> "user defined"
      | x when x >= 0x7 && x <= 0xE -> "reserved for future use"
      | _ -> assert false )
  | 0x7 -> (
      match lvl_2 with
      | 0x0 -> "arts/culture (without music, general)"
      | 0x1 -> "performing arts"
      | 0x2 -> "fine arts"
      | 0x3 -> "religion"
      | 0x4 -> "popular culture/traditional arts"
      | 0x5 -> "literature"
      | 0x6 -> "film/cinema"
      | 0x7 -> "experimental film/video"
      | 0x8 -> "broadcasting/press"
      | 0x9 -> "new media"
      | 0xA -> "arts/culture magazines"
      | 0xB -> "fashion"
      | 0xF -> "user defined"
      | 0xC | 0xD | 0xE -> "reserved for future use"
      | _ -> assert false )
  | 0x8 -> (
      match lvl_2 with
      | 0x0 -> "social/political issues/economics (general)"
      | 0x1 -> "magazines/reports/documentary"
      | 0x2 -> "economics/social advisory"
      | 0x3 -> "remarkable people"
      | 0xF -> "user defined"
      | x when x >= 0x4 && x <= 0xE -> "reserved for future use"
      | _ -> assert false )
  | 0x9 -> (
      match lvl_2 with
      | 0x0 -> "education/science/factual topics (general)"
      | 0x1 -> "nature/animals/environment"
      | 0x2 -> "technology/natural sciences"
      | 0x3 -> "medicine/physiology/psychology"
      | 0x4 -> "foreign countries/expeditions"
      | 0x5 -> "social/spiritual sciences"
      | 0x6 -> "further education"
      | 0x7 -> "languages"
      | 0xF -> "user defined"
      | x when x >= 0x8 && x <= 0xE -> "reserved for future use"
      | _ -> assert false )
  | 0xA -> (
      match lvl_2 with
      | 0x0 -> "leisure hobbies (general)"
      | 0x1 -> "tourism/travel"
      | 0x2 -> "handicraft"
      | 0x3 -> "motoring"
      | 0x4 -> "fitness and health"
      | 0x5 -> "cooking"
      | 0x6 -> "advertisement/shopping"
      | 0x7 -> "gardening"
      | 0xF -> "user defined"
      | x when x >= 0x8 && x <= 0xE -> "reserved for future use"
      | _ -> assert false )
  | 0xB -> (
      match lvl_2 with
      | 0x0 -> "original language"
      | 0x1 -> "black and white"
      | 0x2 -> "unpublished"
      | 0x3 -> "live broadcast"
      | 0x4 -> "plano-stereoscopic"
      | 0x5 -> "local or regional"
      | 0xF -> "user defined"
      | x when x >= 0x6 && x <= 0xE -> "reserved for future use"
      | _ -> assert false )
  | 0xF -> (
      match lvl_2 with
      | x when x >= 0x0 && x <= 0xF -> "user defined"
      | _ -> assert false )
  | 0xC | 0xD | 0xE -> (
      match lvl_2 with
      | x when x >= 0x0 && x <= 0xF -> "reserved for future use"
      | _ -> assert false )
  | _ -> assert false

let rec parse bs off =
  if Bitstring.bitstring_length bs = 0 then []
  else
    match%bitstring bs with
    | {| cont_lvl1 : 4
       ; cont_lvl2 : 4  : save_offset_to (off_1)
       ; user_byte : 8  : save_offset_to (off_2)
       ; rest      : -1 : save_offset_to (off_3), bitstring
       |}
      ->
        let lvl_1 = parse_lvl1 cont_lvl1 in
        let lvl_2 = parse_lvl2 ~lvl_1:cont_lvl1 ~lvl_2:cont_lvl2 in
        let nodes =
          [
            Node.make ~parsed:lvl_1 ~offset:off 4 "content_nibble_level_1"
              (Hex (Int cont_lvl1));
            Node.make ~parsed:lvl_2 ~offset:(off + off_1) 4
              "content_nibble_level_2" (Hex (Int cont_lvl2));
            Node.make ~offset:(off + off_2) 8 "user_byte" (Bits (Int user_byte));
          ]
        in
        nodes @ parse rest (off + off_3)
