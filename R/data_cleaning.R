#' @export
clean_data <- function(sessions) {
  sessions |>
    nocturn::remove_sessions_no_sleep() |>
    nocturn::set_min_sleep_period(3) |>
    nocturn::set_session_sleep_onset_range("19:00", "06:00")
}