let parse code =
  match%bitstring code with
  | {| s : 24 : string |} -> (
      (s, match%bitstring code with {| d : 24 : int |} -> d) )
