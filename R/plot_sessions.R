#' @export
plot_somnofy_sessions <- function(sessions, ema = NULL) {
  col <- nocturn::get_session_colnames(sessions)

  plot_data <- sessions |>
    dplyr::filter(!is.na(.data[[col$time_at_sleep_onset]]) & !is.na(.data[[col$time_at_wakeup]])) |>
    dplyr::group_by(.data[[col$night]]) |>
    dplyr::summarise(
      sleep_start = nocturn::mean_time(.data[[col$time_at_sleep_onset]]),
      sleep_end = nocturn::mean_time(.data[[col$time_at_wakeup]]),
      midsleep = nocturn::mean_time(.data[[col$time_at_midsleep]]),
      ) |>
    dplyr::ungroup() |>
    dplyr::mutate(
      sleep_start = nocturn::time_to_hours(nocturn::shift_times_by_12h(.data$sleep_start)),
      sleep_end = nocturn::time_to_hours(nocturn::shift_times_by_12h(.data$sleep_end)),
      midsleep = nocturn::time_to_hours(nocturn::shift_times_by_12h(.data$midsleep)),
      night = .data[[col$night]],
      night_groups = as.numeric(as.factor(.data[[col$night]]))
      )

  if (!is.null(ema)) {
    ema_col <- nocturn::get_colnames(ema)
    plot_data <- plot_data |>
      dplyr::left_join(
        ema |>
        dplyr::mutate(night = .data[[ema_col$night]]) |>
        dplyr::select(dplyr::all_of(c("night", ema_col$sleep_info))),
        by = "night"
      )
  }

  p <- ggplot2::ggplot(plot_data) +
    ggplot2::geom_rect(
    ggplot2::aes(
      xmin = .data$sleep_start,
      xmax = .data$sleep_end,
      ymin = .data$night_groups - 0.3,
      ymax = .data$night_groups + 0.3
    ),
    fill = "black",
    alpha = 1,
    show.legend = FALSE
    ) +
    ggplot2::geom_segment(
      ggplot2::aes(
          x = .data$midsleep,
          xend = .data$midsleep,
          y = .data$night_groups - 0.3,
          yend = .data$night_groups + 0.3
      ),
      color = "white",
      linewidth = 0.8
    ) +
    ggplot2::scale_y_reverse() +
    ggplot2::labs(
      title = NULL,
      x = NULL,
      y = NULL,
      fill = NULL
    ) +
    ggplot2::theme_minimal() +
    ggplot2::theme(
      axis.text.x = ggplot2::element_blank(),
      axis.text.y = ggplot2::element_blank(),
      panel.grid = ggplot2::element_blank(),
      panel.widths = grid::unit(1.5, "in"),
      panel.heights = grid::unit(3, "in")
    )
  
  if (is.null(ema)) {
    p
  } else {
    p + 
    ggplot2::geom_text(
        ggplot2::aes(
            x = max(.data$sleep_end) +0.5,
            y = .data$night_groups,
            label = .data[[ema_col$sleep_info]]
        ),
        hjust = 0,
        size = 3,
        color = "black"
    )
    # ggplot2::coord_cartesian(clip = "off")
  }
}
