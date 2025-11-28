#' @export
plot_ema_meters <- function(ema) {
  col <- nocturn::get_colnames(ema)

  plot_data <- ema |>
    dplyr::mutate(
      Anxiety = .data[[col$anxiety_level]] *2,
      Mood = .data[[col$mood_level]]
    ) |>
    tidyr::pivot_longer(
      cols = c("Mood", "Anxiety"),
      names_to = "type",
      values_to = "value"
    ) |>
    dplyr::group_by(.data$type) |>
    dplyr::summarise(
      mean_value = mean(.data$value, na.rm = TRUE)
    ) |>
    dplyr::mutate(
      ymin = 0,
      ymax = 10
    )

  ggplot2::ggplot(plot_data, ggplot2::aes(x = type, y = mean_value, fill = mean_value)) +
    ggplot2::geom_bar(stat = "identity", width = 0.6) +
    ggplot2::scale_fill_gradientn(
      colors = c("blue", "green", "red"),
      limits = c(0, 10)
    ) +
    ggplot2::geom_rect(
      ggplot2::aes(
        xmin = as.numeric(factor(type)) - 0.3,
        xmax = as.numeric(factor(type)) + 0.3,
        ymin = ymin,
        ymax = ymax
      ),
      fill = NA,
      color = "black",
      linewidth = 5
    ) +
    ggplot2::labs(x = NULL, y = NULL) +
    ggplot2::theme_minimal() +
    ggplot2::theme(
      legend.position = "none",
      panel.grid = ggplot2::element_blank(),
      axis.text.x = ggplot2::element_text(size = 25, color = "black"),
      axis.text.y = ggplot2::element_blank()
    )
}
