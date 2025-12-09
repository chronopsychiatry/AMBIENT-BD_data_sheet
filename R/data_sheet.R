data_sheet <- function(output_file, date_range = c(NULL, NULL)) {
  data <- load_data(date_range = date_range)

  somnofy <- data$somnofy
  ema <- data$ema
  axivity <- data$axivity

  col_som <- nocturn::get_colnames(somnofy)

  from_date <- min(somnofy[[col_som$night]], na.rm = TRUE)
  to_date <- max(somnofy[[col_som$night]], na.rm = TRUE)
  participant_id <- somnofy[["participant_id"]][1]
  n_som_sessions <- length(unique(somnofy[[col_som$id]]))
  n_ema_bursts <- length(unique(ema[["filename"]]))
  n_axv_bursts <- length(unique(axivity[["filename"]]))

  p_sessions <- plot_somnofy_sessions(somnofy)
  p_density <- nocturn::sleeptimes_density(somnofy, circular = TRUE)
  p_bubbles <- nocturn::plot_sleep_bubbles(somnofy)
  p_comparison <- plot_method_comparison(somnofy, ema, axivity)
  p_mood_board <- plot_ema_mood_board(ema)

  svgedit::draw(
    input_svg = system.file("svg", "Datasheet_template.svg", package = "ambient_data_sheet"),
    output_svg = output_file,
    plots = list(
      Sessions = p_sessions,
      Bubbles = p_bubbles,
      Density = p_density,
      Comparison = p_comparison,
      Mood_board = p_mood_board
    ),
    plot_scale = list(
      Bubbles = 0.6,
      Density = 0.6,
      Comparison = 0.8,
      Mood_board = 0.5
    ),
    text = list(
      dates = c(from_date, to_date),
      participant_id = participant_id,
      Som_sessions = n_som_sessions,
      EMA_bursts = n_ema_bursts,
      Axivity_bursts = n_axv_bursts
    )
  )
}
