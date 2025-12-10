participant_id <- "ABD001"
datastore_root <- "/media/Ambient-BD"
output_file <- paste0("~/Downloads/datasheet_", participant_id, ".pdf")

generate_datasheet(
  datastore_root = datastore_root,
  participant_id = participant_id,
  output_file = output_file,
  format = tools::file_ext(output_file)
)
