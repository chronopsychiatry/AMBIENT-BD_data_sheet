plot_method_comparison <- function(somnofy, ema, axivity) {
  col_som <- nocturn::get_colnames(somnofy)
  col_ema <- nocturn::get_colnames(ema)
  col_axv <- nocturn::get_colnames(axivity)

  # Fudge filename in Somnofy for now
  somnofy$filename <- NA_character_

  somnofy <- preprocess_df(somnofy, col_som, "Somnofy")
  ema <- preprocess_df(ema, col_ema, "EMA")
  axivity <- preprocess_df(axivity, col_axv, "Axivity")

  plot_data <- list(
    somnofy = somnofy,
    ema = ema,
    axivity = axivity
  ) |>
    purrr::reduce(~dplyr::full_join(.x, .y, by = "night")) |>
    dplyr::mutate(
      sleep_onset_ema = shortest_time_diff(.data$time_at_sleep_onset_somnofy, .data$time_at_sleep_onset_ema),
      midsleep_ema    = shortest_time_diff(.data$time_at_midsleep_somnofy,    .data$time_at_midsleep_ema),
      wakeup_ema      = shortest_time_diff(.data$time_at_wakeup_somnofy,      .data$time_at_wakeup_ema),
      sleep_onset_axv = shortest_time_diff(.data$time_at_sleep_onset_somnofy, .data$time_at_sleep_onset_axivity),
      midsleep_axv    = shortest_time_diff(.data$time_at_midsleep_somnofy,    .data$time_at_midsleep_axivity),
      wakeup_axv      = shortest_time_diff(.data$time_at_wakeup_somnofy,      .data$time_at_wakeup_axivity),
      burst           = dplyr::coalesce(.data$filename_ema, .data$filename_axivity)
    ) |>
    tidyr::pivot_longer(
      cols = dplyr::matches("^(sleep_onset|midsleep|wakeup)_(ema|axv)$"),
      names_to = "measure",
      values_to = "time"
    ) |>
    dplyr::mutate(
      method = dplyr::case_when(
        grepl("_ema$", measure) ~ "EMA",
        grepl("_axv$", measure) ~ "Axivity"
      ),
      measure = sub("_(ema|axv)$", "", .data$measure)
    ) |>
    dplyr::filter(!is.na(.data$method) & !is.na(.data$burst)) |>
    dplyr::mutate(
      measure = factor(.data$measure, levels = c("sleep_onset", "midsleep", "wakeup"), labels = c("Sleep Onset", "Midsleep", "Wakeup")),
      method = factor(.data$method, levels = c("Axivity", "EMA"))
    ) |>
    dplyr::arrange(method) |>
    create_burst_labels(burst_col = "burst", night_col = "night")


  ggplot2::ggplot(plot_data,
    ggplot2::aes(
      x = .data$night,
      y = .data$time,
      fill = .data$time,
      shape = .data$method
    )
  ) +
    ggplot2::facet_grid(
      rows = ggplot2::vars(.data$measure),
      cols = ggplot2::vars(.data$burst_label),
      scales = "free_x"
    ) +
    ggplot2::geom_hline(yintercept = 0, colour = "black", linetype = "solid") +
    ggplot2::geom_point(size = 2, stroke = 0.6) +
    ggplot2::scale_shape_manual(values = c("EMA" = 24, "Axivity" = 21)) +
    ggplot2::scale_fill_gradient2(low = "blue", mid = "white", high = "red", midpoint = 0) +
    ggplot2::theme_minimal() +
    ggplot2::theme(
      axis.title.y = ggplot2::element_text(size = 14),
      panel.grid = ggplot2::element_blank(),
      panel.border = ggplot2::element_rect(colour = "black", fill = NA, linewidth = 1),
      strip.background = ggplot2::element_blank(),
      strip.text.x = ggplot2::element_text(size = 10),
      strip.text.y = ggplot2::element_text(size = 10),
      axis.text.x = ggplot2::element_blank(),
      axis.text.y = ggplot2::element_text(size = 10),
      axis.ticks.x = ggplot2::element_blank()
    ) +
    ggplot2::labs(
      x = NULL,
      y = "Difference to Somnofy (minutes)",
      fill = "Difference (min)",
      shape = "Method"
    )

}

#' Rename columns to differentiate between methods (somnofy, ema, axivity)
preprocess_df <- function(df, colnames, method_name) {
  suffix <- tolower(method_name)
  filename_col <- paste0("filename_", suffix)
  df |>
    dplyr::select(
      night = .data[[colnames$night]],
      !!paste0("time_at_sleep_onset_", suffix) := .data[[colnames$time_at_sleep_onset]],
      !!paste0("time_at_midsleep_", suffix) := .data[[colnames$time_at_midsleep]],
      !!paste0("time_at_wakeup_", suffix) := .data[[colnames$time_at_wakeup]],
      !!filename_col := .data$filename
    ) |>
    dplyr::mutate(
      !!filename_col := as.numeric(as.factor(.data[[filename_col]])),
      method = method_name
    )
}

#' Returns the shortest difference between two times (in minutes)
shortest_time_diff <- function(t1, t2) {
  h1 <- nocturn::time_to_hours(t1)
  h2 <- nocturn::time_to_hours(t2)
  diff <- (h2 - h1) %% 24
  diff <- ifelse(diff > 12, diff - 24, diff)
  diff * 60 # in minutes
}
