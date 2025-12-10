load_data <- function(somnofy_file, ema_folder, axivity_folder, date_range = c(NULL, NULL)) {
  somnofy <- nocturn::load_sessions(somnofy_file) |>
    clean_data(date_range = date_range)

  ema <- nocturn::load_batch(ema_folder, pattern = "bfef9dv") |> # Will need to be updated when module names change
    nocturn::set_colnames(ema_colnames)
  ema_col <- nocturn::get_colnames(ema)
  ema[[ema_col$sleep_onset_latency]] <- ema[[ema_col$sleep_onset_latency]] * 60 # minutes to seconds
  ema <- nocturn::clean_sessions(ema) |>
    clean_data(date_range = date_range)

  axivity <- nocturn::load_batch(axivity_folder, pattern = "part4_nightsummary") |>
    clean_data(date_range = date_range)

  list(somnofy = somnofy,
       ema = ema,
       axivity = axivity)
}
