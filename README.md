# AMBIENT-BD_data_sheet

R script to create a data sheet from different types of input data (Somnofy, Axivity, EMA).

## Installation

Install the `remotes` package

```r
install.packages("remotes")
```

Install the nocturn and Ambient Data Sheet package:

```r
remotes::install_github("Chronopsychiatry/AMBIENT-BD-nocturn")
remotes::install_github("Chronopsychiatry/AMBIENT-BD_data_sheet")
```

## Usage

### Ambient-BD WP4

The Ambient-BD WP4 can make use of the data storage structure on datastore to generate the data sheet easily. To do so, use the `generatedatasheet` function.

```r
library(AmbientDataSheet)

generate_datasheet(
	datastore_root = "path/to/dsmith35-AMBIENT-BD",
	participant_id = "ABD001",
	output_file = "path/to/output/ABD001_datasheet.pdf",
	format = "pdf"  # Can also be set to svg
)
```

### Other projects

If your data structure is different, you can use the `load_data` and `build_datasheet` functions.

```r
library(AmbientDataSheet)

data <- load_data(
  somnofy_file = "path/to/somnofy",
  ema_folder = "path/to/ema",
  axivity_folder = "path/to/axivity"
)

build_datasheet(
  somnofy = data$somnofy,
  axivity = data$axivity,
  ema = data$ema,
  ema_mood = data$ema_mood,
  output_file = "path/to/output.pdf",
  format = "pdf"  # Can also be set to svg
)
```
