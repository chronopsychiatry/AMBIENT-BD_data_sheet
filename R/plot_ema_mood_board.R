plot_ema_mood_board <- function(ema_mood) {
  col <- nocturn::get_colnames(ema_mood)

  plot_data <- ema_mood |>
    dplyr::mutate(
      burst = as.factor(dplyr::dense_rank(.data$filename)),
      mood = .data[[col$mood_level]],
      anxiety = .data[[col$anxiety_level]]
    ) |>
    dplyr::arrange(.data$burst, .data[[col$night]]) |>
    create_burst_labels(burst_col = "burst", night_col = col$night) |>
    tidyr::pivot_longer(
      cols = tidyselect::all_of(c("mood", "anxiety")),
      names_to = "measure",
      values_to = "value"
    )

  ggplot2::ggplot(plot_data,
    ggplot2::aes(
      x = .data[[col$session_start]],
      y = .data$value
    )
  ) +
    ggplot2::facet_grid(
      cols = ggplot2::vars(.data$burst_label),
      rows = ggplot2::vars(.data$measure),
      scales = "free"
    ) +
    ggplot2::geom_point(
      color = "black",
      alpha = 0.3,
      size = 3,
      show.legend = FALSE
    ) +
    ggplot2::scale_y_continuous(
      limits = function(x) {
        if (all(plot_data$measure[plot_data$value %in% x] == "anxiety")) {
          c(0, 5)
        } else {
          c(0, 10)
        }
      }
    ) +
    ggplot2::theme_minimal() +
    ggplot2::theme(
      axis.title.x = ggplot2::element_blank(),
      axis.title.y = ggplot2::element_blank(),
      # axis.ticks.x = ggplot2::element_blank(),
      axis.text.x = ggplot2::element_text(size = 14),
      axis.text.y = ggplot2::element_text(size = 14),
      strip.background = ggplot2::element_blank(),
      strip.text.x = ggplot2::element_text(size = 14),
      strip.text.y = ggplot2::element_text(size = 16),
      panel.border = ggplot2::element_rect(colour = "black", fill = NA, linewidth = 1)
    )
}
