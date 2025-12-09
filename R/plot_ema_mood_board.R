plot_ema_mood_board <- function(ema) {
  col <- nocturn::get_colnames(ema)

  plot_data <- ema |>
    dplyr::mutate(
      burst = as.factor(dplyr::dense_rank(.data$filename))
    ) |>
    dplyr::arrange(.data$burst, .data[[col$night]]) |>
    dplyr::group_by(burst) |>
    dplyr::mutate(
      burst_label = paste0(
        format(min(.data[[col$night]], na.rm = TRUE), "%d-%m-%Y"),
        " to\n",
        format(max(.data[[col$night]], na.rm = TRUE), "%d-%m-%Y")
      ),
      burst_min_night = min(.data[[col$night]], na.rm = TRUE)
    ) |>
    dplyr::ungroup()

  plot_data <- plot_data |>
    dplyr::mutate(
      burst_label = factor(
        burst_label,
        levels = plot_data |>
          dplyr::distinct(burst_label, burst_min_night) |>
          dplyr::arrange(burst_min_night) |>
          dplyr::pull(burst_label)
      )
    )

  ggplot2::ggplot(plot_data,
    ggplot2::aes(
      x = .data[[col$anxiety_level]],
      y = .data[[col$mood_level]],
      color = .data$burst_label
    )
  ) +
    ggplot2::facet_grid(
      cols = ggplot2::vars(.data$burst_label)
    ) +
    ggplot2::geom_point(
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
