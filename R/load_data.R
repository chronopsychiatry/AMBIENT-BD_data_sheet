load_data <- function() {
  somnofy_file <- "/media/Store/Daniel/Ambient-BD/Data/AmbientViewer_test_data/simulated_sessions_18months.csv"
  somnofy <- nocturn::load_sessions(somnofy_file) |>
    AmbientDataSheet::clean_data()

  ema_file <- "/media/Store/Daniel/Ambient-BD/Data/Data_sheet_test_input/WP4_test/ABD001_PROM-EMA_module_bfef9dv80oupx78m_20251112.csv"

  ema <- nocturn::load_sessions(ema_file) |>
    nocturn::set_colnames(AmbientDataSheet::ema_colnames)
  ema_col <- nocturn::get_colnames(ema)
  ema[[ema_col$sleep_onset_latency]] <- ema[[ema_col$sleep_onset_latency]] * 60 # minutes to seconds
  ema <- nocturn::clean_sessions(ema) |>
    AmbientDataSheet::clean_data()

  axivity_file <- "/media/Store/Daniel/Ambient-BD/Data/Data_sheet_test_input/WP4_test/Fake_GGIR_part4.csv"

  axivity <- nocturn::load_sessions(axivity_file) |>
    AmbientDataSheet::clean_data()

  list(somnofy = somnofy,
       ema = ema,
       axivity = axivity)
}
