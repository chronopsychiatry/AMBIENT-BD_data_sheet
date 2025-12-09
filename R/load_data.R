load_data <- function(date_range = c(NULL, NULL)) {
  data_folder <- "/media/Store/Daniel/Ambient-BD/Data/Data_sheet_test_input/18-months_test/"

  somnofy_file <- paste0(data_folder, "simulated_sessions_18months.csv")
  somnofy <- nocturn::load_sessions(somnofy_file) |>
    clean_data(date_range = date_range)

  ema <- nocturn::load_batch(data_folder, pattern = "PROM-EMA") |>
    nocturn::set_colnames(ema_colnames)
  ema_col <- nocturn::get_colnames(ema)
  ema[[ema_col$sleep_onset_latency]] <- ema[[ema_col$sleep_onset_latency]] * 60 # minutes to seconds
  ema <- nocturn::clean_sessions(ema) |>
    clean_data(date_range = date_range)

  axivity <- nocturn::load_batch(data_folder, pattern = "GGIR_part4") |>
    clean_data(date_range = date_range)

  list(somnofy = somnofy,
       ema = ema,
       axivity = axivity)
}
