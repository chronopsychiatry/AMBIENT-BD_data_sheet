#' @export
clean_data <- function(sessions, date_range = c(NULL, NULL)) {
  sessions |>
    nocturn::filter_by_night_range(from_night = date_range[1], to_night = date_range[2]) |>
    nocturn::remove_sessions_no_sleep() |>
    nocturn::set_min_sleep_period(3) |>
    nocturn::set_session_sleep_onset_range("19:00", "06:00")
}
