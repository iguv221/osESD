

library(optparse);
library(tcltk);
library(reticulate)

# setwd(dirname(rstudioapi::getSourceEditorContext()$path))
print(getwd())

source("R//osESD_modules.R")
source("R//osESD_Detector_auto.R")
source("R//modules//functions.R")
source("R//modules//data_functions.R")


#' Title
#'
#' @param data
#' @param size
#' @param dwin
#' @param rwin
#' @param maxr
#' @param alpha
#' @param condition
#' @param visualize
#'
#' @return
#' @export
osESD <- function(data,size=100,dwin=5,rwin=5,maxr=10,alpha=0.01,condition=TRUE,visualize=FALSE){
  data<-timestamp_creator(data)
  osESD_anoms <- osESD_Detector(data=data$value, time=data$timestamps, dwin=dwin, rwin=rwin,
                                train_size=size, alpha=alpha, maxr=maxr, condition=condition)
  return (osESD_anoms)
}


