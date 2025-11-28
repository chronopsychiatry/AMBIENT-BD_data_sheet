data_sheet <- function(output_file) {
  data <- load_data()

  somnofy <- data$somnofy
  # ema <- data$ema
  # axivity <- data$axivity

  p1 <- AmbientDataSheet::plot_somnofy_sessions(somnofy)
  p2 <- nocturn::sleeptimes_density(somnofy, circular = TRUE)
  p3 <- nocturn::plot_sleep_bubbles(somnofy)

  svgedit::draw(
    input_svg = system.file("svg", "test_template_ink.svg", package = "ambient_data_sheet"),
    output_svg = output_file,
    plots = list(
      Sessions = p1,
      Bubbles = p3,
      Comparison = p2
    )
  )
}
