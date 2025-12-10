#' Build an SVG datasheet from somnofy, axivity and EMA data
#'
#' @param somnofy Data frame containing Somnofy sleep data
#' @param axivity Data frame containing Axivity sleep data
#' @param ema Data frame containing EMA data
#' @param output_file Path to save the generated datasheet (pdf format)
#' @return None. The function saves the datasheet SVG file to the specified location.
#' @export
build_datasheet <- function(somnofy, axivity, ema, output_file, format = "pdf") {
  col_som <- nocturn::get_colnames(somnofy)

  from_date <- min(somnofy[[col_som$night]], na.rm = TRUE)
  to_date <- max(somnofy[[col_som$night]], na.rm = TRUE)
  participant_id <- somnofy[["participant_id"]][1]
  n_som_sessions <- length(unique(somnofy[[col_som$id]]))
  n_ema_bursts <- length(unique(ema[["filename"]]))
  n_axv_bursts <- length(unique(axivity[["filename"]]))

  p_sessions <- plot_somnofy_sessions(somnofy)
  p_density <- nocturn::sleeptimes_density(somnofy, circular = TRUE)
  p_bubbles <- nocturn::plot_sleep_bubbles(somnofy, bubble_size = 3)
  p_comparison <- plot_method_comparison(somnofy, ema, axivity)
  p_mood_board <- plot_ema_mood_board(ema)

  if (format == "svg") {
    filled_svg <- output_file
  } else if (format == "pdf") {
    filled_svg <- tempfile(fileext = ".svg")
  } else {
    cli::cli_abort(c("x" = "Unsupported format: {format}",
                     "i" = "Use 'svg' or 'pdf'"))
  }

  svgedit::draw(
    input_svg = system.file("svg", "Datasheet_template.svg", package = "ambient_data_sheet"),
    output_svg = filled_svg,
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

  if (format == "pdf") {
    rsvg::rsvg_pdf(filled_svg, output_file)
  }
}
