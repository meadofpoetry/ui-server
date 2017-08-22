open Tyxml.Html
open Components

let card title =
  let sections = [ Card.create_primary [ Card.create_title title ()
                                       ; Card.create_subtitle "Subtitle" () ]
                                       ()
                 ; Card.create_media [ Text_field.create ~password:true
                                                         "Старый пароль"
                                                         ()
                                     ; Text_field.create ~password:true
                                                         "Новый пароль"
                                                         ()
                                     ; Text_field.create ~password:true
                                                         "Повторите новый пароль"
                                                         ()]
                                     ()
                 ; Card.create_actions [ Button.create ~ripple:true
                                                       "Применить"
                                                       () ]
                                       () ] in
  Card.create ~sections ()

let users () =
  (List.map (fun x -> div ~a:[a_class ["mdc-layout-grid__cell mdc-layout-grid__cell--span-12"]]
                          [card x])
            ["Наблюдатель"; "Оператор"; "Администратор"])
