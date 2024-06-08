

rm(list=ls())
setwd(dirname(rstudioapi::getSourceEditorContext()$path))
getwd()


library(devtools)
devtools::install_github("iguv221/osESD",force=TRUE)
library(osESD)

osESD
auto_osESD

osESD_df1 <- data(A3Benchmark_TS79)

osESD_df1

length(osESD_df1)



