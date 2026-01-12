#' Check if data paths exist
#'
#' @param somnofy_file Path to the Somnofy data file
#' @param ema_folder Path to the EMA data folder
#' @param axivity_folder Path to the Axivity data folder
#' @return None. The function throws an error if any of the paths do not exist.
check_data_paths <- function(somnofy_file, ema_folder, axivity_folder) {
  if (!file.exists(somnofy_file)) {
    cli::cli_abort(c("x" = "Could not find Somnofy data at: {somnofy_file}"))
  }

  if (!dir.exists(ema_folder) || dir_is_empty(ema_folder)) {
    cli::cli_abort(c("x" = "Could not find EMA folder at: {ema_folder}"))
  }

  if (!dir.exists(axivity_folder) || dir_is_empty(axivity_folder)) {
    cli::cli_abort(c("x" = "Could not find Axivity folder at: {axivity_folder}"))
  }
}

#' Check if a directory is empty
#'
#' @param path Path to the directory
#' @return TRUE if the directory is empty, FALSE otherwise
dir_is_empty <- function(path) {
  length(list.files(path, all.files = TRUE, no.. = TRUE)) == 0
}
