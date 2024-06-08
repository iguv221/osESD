# Function to parse arguments and mark the list
parse_and_mark <- function(parser) {
  args <- parse_args(parser)
  attr(args, "parsed") <- TRUE
  return(args)
}

# Check function to determine if a list is from parse_args
is_parsed_args <- function(x) {
  is.list(x) && !is.null(attr(x, "parsed"))
}


convert_params_to_parsed_args <- function(custom_params) {
  allowed_params <- c("WindowSizes", "AndOr", "MaxRs", "Dwins", "Rwins", "Alphas", "help")
  if (is_parsed_args(custom_params)){
    param_names <- gsub("^--", "", names(custom_params))
    if (!all(param_names %in% allowed_params)) {
      stop("One or more parameter names from parsed arguments are not allowed. Allowed names are: ", paste(allowed_params, collapse=", "))
    }
    return (custom_params)
  }
  
  if (!all(names(custom_params) %in% allowed_params)) {
    stop("One or more parameter names are not allowed. Allowed names are: ", paste(allowed_params, collapse=", "))
  }
  option_list <- lapply(names(custom_params), function(key) {
    default_value = unlist(custom_params[[key]])
    make_option(c(sprintf("--%s", key)), type="double", default=default_value, action="store")
  })
  opt_parser <- OptionParser(option_list = option_list)
  parsed_params <- parse_args(opt_parser, args = character())
  return(parsed_params)
}


timestamp_creator <- function(data) {
  if (length(data$timestamps)==0){
    time=1:length(data$value)
  }
  else{
    return (list('timestamps'=data$timestamps, 'value'=data$value, 'anomaly'=data$anomaly))
  }
  return (list('timestamps'=time, 'value'=data$value, 'anomaly'=data$anomaly))
}

PRFTcalculator <- function(Reals, Preds, t1, t2) {
  TP <- 0
  TN <- 0
  FP <- 0
  FN <- 0
  for (i in 1:length(Reals)) {
    real <- Reals[i]
    pred <- Preds[i]
    if (real == 0 & pred == 0) { TN <- TN + 1 }
    else if (real == 0 & pred == 1) { FP <- FP + 1 }
    else if (real == 1 & pred == 0) { FN <- FN + 1 }
    else { TP <- TP + 1 }
  }
  run_time <- as.numeric(difftime(t2, t1, units="secs"))

  if (TP + FN == 0) {return(list('precision'=0,'recall'=0,'F1Score'=0,'Run_Time'=run_time))}
  else if (TP + FN == 0) { P <- TP / (TP + FP); return(list('precision'=P,'recall'=0,'F1Score'=0,'Run_Time'=run_time)) }
  else if (TP + FP == 0) { R <- TP / (TP + FN); return(list('precision'=0,'recall'=R,'F1Score'=0,'Run_Time'=run_time)) }

  R <- TP / (TP + FN)
  P <- TP / (TP + FP)
  if (R + P == 0) { return(list('precision'=0,'recall'=0,'F1Score'=0,'Run_Time'=run_time)) }
  F1 <- 2 * R * P / (R + P)

  return(list('precision'=P,'recall'=R,'F1Score'=F1,'Run_Time'=run_time))
}
