#' Generate a datasheet for a given participant, using data from the Ambient-BD datastore
#'
#' @param datastore_root Root directory of the Ambient-BD datastore
#' @param participant_id ID of the participant (e.g., "ABD001")
#' @param output_file Path to save the generated datasheet SVG file
#' @return None. The function saves the datasheet SVG file to the specified location.
#' @export
generate_datasheet <- function(datastore_root, participant_id, output_file, format = "pdf") {
  paths <- datastore_paths(
    datastore_root = datastore_root,
    participant_id = participant_id
  )

  data <- load_data(
    somnofy_file = paths$somnofy,
    ema_folder = paths$ema,
    axivity_folder = paths$axivity,
    date_range = c(NULL, NULL)
  )

  build_datasheet(
    somnofy = data$somnofy,
    axivity = data$axivity,
    ema = data$ema,
    ema_mood = data$ema_mood,
    output_file = output_file,
    format = format
  )

}
