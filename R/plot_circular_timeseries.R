plot_circular_timeseries <- function(sessions, variable, color = "black", exclude_zero = FALSE) {
  nocturn::check_session_colnames(sessions, c("night", "sleep_period"))

  if (exclude_zero) {
    sessions <- sessions |>
      dplyr::filter(.data[[variable]] != 0)
  }

  if (nocturn::is_iso8601_datetime(sessions[[variable]])) {
    sessions$variable <- nocturn::parse_time(sessions[[variable]]) |>
      nocturn::update_date(date = "1970-01-01")
    y_is_time <- TRUE
  } else {
    sessions$variable <- sessions[[variable]]
    y_is_time <- FALSE
  }

  k <- 7

  sessions <- keep_longest(sessions) |>
    dplyr::arrange(night) |>
    dplyr::mutate(variable = slider::slide_dbl(variable, mean, .before = k %/% 2, .after = k %/% 2, .complete = TRUE)) |>
    dplyr::mutate(
      year = lubridate::year(night),
      doy  = lubridate::yday(night),
      days_in_year = dplyr::if_else(lubridate::leap_year(night), 366L, 365L),
      x = (doy - 1) / days_in_year
    )

  y_breaks <- c(0, 10, 20, 30)

  p <- ggplot2::ggplot(sessions, ggplot2::aes(x = x, y = variable, group = year)) +
    ggplot2::geom_line(alpha = 0.8, color = color) +
    ggplot2::geom_text(
      data = data.frame(x = 0.005, y = y_breaks, lab = y_breaks),
      ggplot2::aes(x = x, y = y, label = lab),
      inherit.aes = FALSE,
      hjust = 0,
      size = 6,
      colour = "grey30"
    ) +
    ggplot2::scale_x_continuous(
      limits = c(0, 1),
      breaks = seq(0, 11) / 12,
      labels = month.abb
    ) +
    ggplot2::scale_y_continuous(limits = c(0, 30), labels = NULL) +
    ggplot2::labs(
      x = NULL,
      y = NULL
    ) +
    ggplot2::coord_polar() +
    ggplot2::theme_minimal(base_size = 16)

  if (y_is_time) {
    p <- p + ggplot2::scale_y_datetime(
      labels = function(x) format(x, "%H:%M")
    )
  }
  p
}

keep_longest <- function(sessions) {
  nocturn::check_session_colnames(sessions, c("night", "sleep_period"))
  sessions |>
    dplyr::arrange(dplyr::desc(.data$sleep_period)) |>
    dplyr::distinct(.data$night, .keep_all = TRUE) |>
    dplyr::arrange(.data$night)
}
