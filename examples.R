

rm(list=ls())
setwd(dirname(rstudioapi::getSourceEditorContext()$path))
getwd()


library(devtools)
install_github("iguv221/osESD",force=TRUE)
library(osESD)

data(A3Benchmark_TS79)
data(ARIMA5_quad_2)

osESD_df1 <- A3Benchmark_TS79

ex_osESD_anoms<-osESD(data=ARIMA_df,size=100)
ex_osESD_anoms

ARIMA_df <- ARIMA5_quad_2









