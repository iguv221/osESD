# # File: prepare_data.R located in data-raw/
# library(usethis)
# library(tidyverse)  # if you need to do data manipulation
#
#
# setwd(dirname(rstudioapi::getSourceEditorContext()$path))
#
# csv_files <- dir(pattern = "\\.csv$")
# # Loop through each CSV file
# for (file_name in csv_files) {
#   # Extract the base name without extension for use as the object name
#   base_name <- tools::file_path_sans_ext(file_name)
#
#   # Read the CSV file
#   data <- read.csv(file_name)
#
#   # Assign the data frame to a name in the environment based on the file name
#   assign(base_name, data)
#
#   # Use usethis::use_data() to save the data frame as .rda in the 'data/' directory of your package
#   use_data(list = base_name, overwrite = TRUE)
# }
#
# # load("A3Benchmark_TS79.rda")
# # print(A3Benchmark_TS79)  # Should print the dataframe, not just a string
# #
