#' @export
plot_method_comparison <- function(somnofy, ema, axivity) {
  col_som <- nocturn::get_colnames(somnofy)
  col_ema <- nocturn::get_colnames(ema)
  col_axv <- nocturn::get_colnames(axivity)

  somnofy <- melt_df(somnofy, col_som, "Somnofy")
  ema <- melt_df(ema, col_ema, "EMA")
  axivity <- melt_df(axivity, col_axv, "Axivity")

  common_nights <- Reduce(
    intersect,
    list(
      unique(somnofy$night),
      unique(ema$night),
      unique(axivity$night)
    )
  )

  plot_data <- dplyr::bind_rows(somnofy, ema, axivity) |>
    dplyr::filter(night %in% common_nights) |>
    dplyr::mutate(
      time = nocturn::time_to_hours(nocturn::shift_times_by_12h(.data$time)),
      measure = factor(
        .data$measure,
        levels = c("time_at_sleep_onset", "time_at_midsleep", "time_at_wakeup"),
        labels = c("Sleep Onset", "Midsleep", "Wakeup")
      )
    ) |>
    dplyr::group_by(.data$night, .data$measure, .data$method) |>
    dplyr::summarise(
      time = mean(.data$time, na.rm = TRUE)
    ) |>
    dplyr::ungroup()

  ggplot2::ggplot(plot_data,
                  ggplot2::aes(
                    x = night,
                    y = time,
                    color = measure,
                    shape = method,
                    group = interaction(method, measure))) +
    ggplot2::geom_line() +
    ggplot2::geom_point(size = 3, alpha = 0.7) +
    ggplot2::scale_y_continuous(
      labels = function(x) {
        x <- nocturn::shift_times_by_12h(x)
        hrs <- floor(x)
        mins <- round((x - hrs) * 60)
        sprintf("%02d:%02d", hrs %% 24, mins)
      }
    ) +
    ggplot2::scale_color_manual(
      values = c("Sleep Onset" = "purple", "Midsleep" = "cornflowerblue", "Wakeup" = "orange")
    ) +
    ggplot2::labs(x = NULL,
                  y = NULL,
                  color = "Timing",
                  shape = "Method"
                  ) +
    ggplot2::theme_minimal() +
    ggplot2::theme(
      axis.text = ggplot2::element_text(size = 14, color = "black")
    )

}

melt_df <- function(df, colnames, method_name) {
  df |>
    dplyr::select(
      night = .data[[colnames$night]],
      time_at_sleep_onset = .data[[colnames$time_at_sleep_onset]],
      time_at_midsleep = .data[[colnames$time_at_midsleep]],
      time_at_wakeup = .data[[colnames$time_at_wakeup]]
    ) |>
    tidyr::pivot_longer(
      cols = c(
        "time_at_sleep_onset",
        "time_at_midsleep",
        "time_at_wakeup"
      ),
      names_to = "measure",
      values_to = "time"
    ) |>
    dplyr::mutate(
      method = method_name
    )
}
