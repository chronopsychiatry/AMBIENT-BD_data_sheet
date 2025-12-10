#' Create burst labels for plotting
#'
#' Burst labels show the date range for each burst in the data (typically EMA or Axivity).
#' @param df A nocturn sessions dataframe
#' @param burst_col The name of the column indicating bursts (default is "burst")
#' @return The input data frame with an additional `burst_label` column
#' @details This function needs the `night` column to be defined
create_burst_labels <- function(df, burst_col = "burst", night_col = "night") {
  df <- df |>
    dplyr::group_by(.data[[burst_col]]) |>
    dplyr::mutate(
      burst_label = paste0(
        format(min(.data[[night_col]], na.rm = TRUE), "%d-%m-%Y"),
        " to\n",
        format(max(.data[[night_col]], na.rm = TRUE), "%d-%m-%Y")
      ),
      burst_min_night = min(.data[[night_col]], na.rm = TRUE)
    ) |>
    dplyr::ungroup()

  df |>
    dplyr::mutate(
      burst_label = factor(
        .data$burst_label,
        levels = df |>
          dplyr::distinct(.data$burst_label, .data$burst_min_night) |>
          dplyr::arrange(.data$burst_min_night) |>
          dplyr::pull(.data$burst_label)
      )
    )
}
