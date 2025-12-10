plot_ema_mood_board <- function(ema) {
  col <- nocturn::get_colnames(ema)

  plot_data <- ema |>
    dplyr::mutate(
      burst = as.factor(dplyr::dense_rank(.data$filename))
    ) |>
    dplyr::arrange(.data$burst, .data[[col$night]]) |>
    create_burst_labels(burst_col = "burst", night_col = col$night)

  ggplot2::ggplot(plot_data,
    ggplot2::aes(
      x = .data[[col$anxiety_level]],
      y = .data[[col$mood_level]]
    )
  ) +
    ggplot2::facet_grid(
      cols = ggplot2::vars(.data$burst_label)
    ) +
    ggplot2::geom_point(
      color = "black",
      alpha = 0.3,
      size = 4,
      show.legend = FALSE
    ) +
    ggplot2::geom_path(
      ggplot2::aes(
        group = .data$burst
      ),
      show.legend = FALSE
    ) +
    ggplot2::scale_x_continuous(limits = c(0, 5)) +
    ggplot2::scale_y_continuous(limits = c(0, 10)) +
    ggplot2::labs(
      x = "Anxiety",
      y = "Mood",
      color = NULL
    ) +
    ggplot2::theme_minimal() +
    ggplot2::theme(
      axis.title.x = ggplot2::element_text(size = 16),
      axis.title.y = ggplot2::element_text(size = 16),
      axis.text.x = ggplot2::element_text(size = 14),
      axis.text.y = ggplot2::element_text(size = 14),
      strip.background = ggplot2::element_blank(),
      strip.text.x = ggplot2::element_text(size = 14),
      panel.border = ggplot2::element_rect(colour = "black", fill = NA, linewidth = 1),
      aspect.ratio = 1
    )
}
