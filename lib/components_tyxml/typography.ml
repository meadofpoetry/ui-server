module CSS = struct
  (** Sets the font to Roboto. *)
  let root = "mdc-typography"

  (** Sets font properties as Headline 1 *)
  let headline1 = BEM.add_modifier root "headline1"

  (** Sets font properties as Headline 2 *)
  let headline2 = BEM.add_modifier root "headline2"

  (** Sets font properties as Headline 3 *)
  let headline3 = BEM.add_modifier root "headline3"

  (** Sets font properties as Headline 4 *)
  let headline4 = BEM.add_modifier root "headline4"

  (** Sets font properties as Headline 5 *)
  let headline5 = BEM.add_modifier root "headline5"

  (** Sets font properties as Headline 6 *)
  let headline6 = BEM.add_modifier root "headline6"

  (** Sets font properties as Subtitle 1 *)
  let subtitle1 = BEM.add_modifier root "subtitle1"

  (** Sets font properties as Subtitle 2 *)
  let subtitle2 = BEM.add_modifier root "subtitle2"

  (** Sets font properties as Body 1 *)
  let body1 = BEM.add_modifier root "body1"

  (** Sets font properties as Body 2 *)
  let body2 = BEM.add_modifier root "body2"

  (** Sets font properties as Button *)
  let button = BEM.add_modifier root "button"

  (** Sets font properties as Caption *)
  let caption = BEM.add_modifier root "caption"

  (** Sets font properties as Overline *)
  let overline = BEM.add_modifier root "overline"
end
