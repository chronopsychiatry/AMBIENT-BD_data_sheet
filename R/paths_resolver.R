datastore_paths <- function(datastore_root, participant_id) {
  somnofy_path <- file.path(
    datastore_root,
    "04-Somnofy_data",
    "Raw_data",
    "WP4- Prospective study",
    participant_id,
    "data",
    "all_sessions_report.csv"
  )

  ema_folder <- file.path(
    datastore_root,
    "03-EMA_data",
    "Raw_data",
    "REDCap_EMA",
    "reports",
    participant_id
  )

  axivity_folder <- file.path(
    datastore_root,
    "02-Axivity_data",
    "GGIR_results",
    paste0("output_", participant_id),
    "results"
  )

  list(
    somnofy = somnofy_path,
    ema = ema_folder,
    axivity = axivity_folder
  )
}