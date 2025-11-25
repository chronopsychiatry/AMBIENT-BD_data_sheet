generate <- function() {
  quarto::quarto_render(
    input = "inst/quarto/data_sheet_main.qmd",
    output_file = "data_sheet.pdf",
    output_format = "pdf"
  )
}
