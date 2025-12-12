somnofy_file <- "/media/Store/Daniel/Ambient-BD/Data/Data_sheet_test_input/18-months_test/simulated_sessions_18months.csv"
ema_folder <- "/media/Store/Daniel/Ambient-BD/Data/Data_sheet_test_input/18-months_test/"
axivity_folder <- "/media/Store/Daniel/Ambient-BD/Data/Data_sheet_test_input/18-months_test/"

data <- load_data(
  somnofy_file = somnofy_file,
  ema_folder = ema_folder,
  axivity_folder = axivity_folder,
  date_range = c(NULL, NULL)
)

build_datasheet(
  somnofy = data$somnofy,
  axivity = data$axivity,
  ema = data$ema,
  output_file = "/media/Store/Daniel/Ambient-BD/Data_sheets/datasheet_fake_data_test.pdf",
  format = "pdf"
)
