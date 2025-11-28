#' @export
plot_somnofy_sessions <- function(sessions) {
  col <- nocturn::get_session_colnames(sessions)

  plot_data <- sessions |>
    dplyr::filter(.data[[col$sleep_period]] > 0) |>
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
    ) |>
    tidyr::pivot_longer(
      cols = c("sleep_start", "sleep_end", "midsleep"),
      names_to = "type",
      values_to = "time"
    )

  ggplot2::ggplot(plot_data) +
    ggplot2::geom_line(
      ggplot2::aes(
        x = .data$time,
        y = .data$night_groups,
        group = .data$night
      ),
      color = "gray80",
      linewidth = 0.5,
      alpha = 0.5
    ) +
    ggplot2::geom_point(
      ggplot2::aes(
        x = .data$time,
        y = .data$night_groups,
        color = .data$type
      ),
      size = 1,
      show.legend = FALSE
    ) +
    ggplot2::scale_color_manual(
      values = c(
        "sleep_start" = "purple",
        "midsleep" = "cornflowerblue",
        "sleep_end" = "orange"
      )
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
      panel.grid = ggplot2::element_blank()
    )
}
