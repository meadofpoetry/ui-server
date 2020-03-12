open Components_tyxml
open Components_lab_tyxml
open Pc_control_types.Software_updates

let status_to_string = function
  | Unknown -> "Неизвестный статус"
  | Wait -> "Ожидание"
  | Setup -> "Установка"
  | Running -> "Запуск"
  | Query -> "Обращение к репозиторию"
  | Info -> "Получение информации"
  | Remove -> "Удаление"
  | Refresh_cache -> "Обновление кэша"
  | Download -> "Загрузка пакетов"
  | Install -> "Установка пакетов"
  | Update -> "Обновление пакетов"
  | Cleanup -> "Очистка"
  | Obsolete -> "Проверка устаревших пакетов"
  | Dep_resolve -> "Разрешение зависимостей"
  | Sig_check -> "Проверка подписей"
  | Test_commit -> "Проверка транзакции"
  | Commit -> "Завершение транзакции"
  | Request -> "Посылка запроса"
  | Finished -> "Завершение"
  | Cancel -> "Отмена"
  | Download_repository -> "Загрузка репозитория"
  | Download_packagelist -> "Загрузка списка пакетов"
  | Download_filelist -> "Загрузка списка файлов"
  | Download_changelog -> "Загрузка лога изменений"
  | Download_group -> "Загрука группы"
  | Download_updateinfo ->
      "Загрузка информации об обновлениях"
  | Repackaging -> "Перепаковка"
  | Loading_cache -> "Загрузка кэша"
  | Scan_applications -> "Сканирование приложений"
  | Generate_package_list ->
      "Формирование списка пакетов"
  | Waiting_for_lock -> "Ожидание"
  | Waiting_for_auth -> "Ожидание авторизации"
  | Scan_process_list ->
      "Сканирование списка процессов"
  | Check_executable_files ->
      "Проверка исполняемых файлов"
  | Check_libraries -> "Проверка библиотек"
  | Copy_files -> "Копирование файлов"
  | Run_hook -> "Запуск скритов"

let action_label_check_updates =
  "Проверить наличие обновлений"

let action_label_update = "Обновить"

let action_label_reboot = "Перезагрузить"

let equal_state (a : state) (b : state) =
  match (a, b) with
  | `Updates_avail, `Updates_avail -> true
  | `Updates_not_avail, `Updates_not_avail -> true
  | `Unchecked, `Unchecked -> true
  | `Need_reboot, `Need_reboot -> true
  | `Checking (a, _), `Checking (b, _) -> a = b
  | `Upgrading (a, _), `Upgrading (b, _) -> a = b
  | _ -> false

let state_to_svg_path = function
  | `Updates_avail -> Svg_icons.cloud_download
  | `Updates_not_avail | `Need_reboot -> Svg_icons.cloud_check
  | `Unchecked -> Svg_icons.cloud_question
  | `Checking _ -> Svg_icons.cloud_search
  | `Upgrading _ -> Svg_icons.cloud_sync

let state_to_action_label ~auto_reboot = function
  | `Checking _ | `Upgrading _ -> "Загрузка..."
  | `Updates_avail -> "Обновить"
  | `Need_reboot ->
      if auto_reboot then "Обновить страницу"
      else "Перезагрузить прибор"
  | `Unchecked | `Updates_not_avail ->
      "Проверить наличие обновлений"

let state_to_action_disabled = function
  | `Checking _ | `Upgrading _ -> true
  | _ -> false

let state_to_hint ~auto_reboot = function
  | `Checking (status, _) ->
      Printf.sprintf
        "Идёт проверка наличия обновлений.\n%s"
        (status_to_string status)
  | `Upgrading (status, _) ->
      Printf.sprintf "Идёт обновление.\n%s"
        (status_to_string status)
  | `Updates_avail ->
      "Обнаружены доступные обновления."
  | `Need_reboot ->
      let suffix =
        if auto_reboot then
          "Прибор перезагружен, обновите \
           страницу для продолжения работы."
        else "Требуется перезагрузка прибора."
      in
      Printf.sprintf
        "Обновления успешно установлены.\n%s" suffix
  | `Unchecked ->
      "Нет данных о доступных обновлениях."
  | `Updates_not_avail ->
      "На приборе установлены все доступные \
       обновления."

let is_loading = function `Checking _ | `Upgrading _ -> true | _ -> false

let auto_reboot = true

let default_state = `Unchecked

module Make
    (Xml : Xml_sigs.NoWrap)
    (Svg : Svg_sigs.NoWrap with module Xml := Xml)
    (Html : Html_sigs.NoWrap with module Xml := Xml and module Svg := Svg) =
struct
  open Linear_progress.Make (Xml) (Svg) (Html)

  open Button.Make (Xml) (Svg) (Html)

  open Placeholder.Make (Xml) (Svg) (Html)

  open Icon.Make (Xml) (Svg) (Html)

  open Ui_templates_tyxml.Settings_page.Make (Xml) (Svg) (Html)

  let create_placeholder ?classes ?a () =
    let path = state_to_svg_path default_state in
    placeholder ?classes ?a ~icon:(SVG.icon ~d:path ())
      ~text:(`Text (state_to_hint ~auto_reboot default_state))
      ()

  let create ?classes ?(a = []) () =
    create_section ?classes
      ~a:(Html.a_id "remote-update" :: a)
      ~header:
        (create_section_header
           ~title:(`Text "Дистанционное обновление") ())
      ~children:
        [
          Card_markup.card_media
            ~children:[ create_placeholder (); linear_progress ~closed:true () ]
            ();
          Card_markup.card_actions
            ~children:
              [
                button ~appearance:Raised
                  ~label:(state_to_action_label ~auto_reboot default_state)
                  ();
              ]
            ();
        ]
      ()
end

module F = Make (Tyxml.Xml) (Tyxml.Svg) (Tyxml.Html)
