



#' Title
#'
#' @param data, dataset used for anomaly detection. x label 'value', y label 'anomaly'
#' @param labeled, TRUE for supervised learning, FALSE for unsupervised learning
#' @param parameter_learning_length, length for initiating tuning, default 20%
#' @param parameters, parameters, either list or parse arguments
#' @param weights, list of weights for scoring, (precision, recall, f1-score, time)
#' @param min_max_switch, TRUE for only minimum and maximum values in parameters, FALSE for full parameters
#'
#' @return indices of anomalies
#' @export
auto_osESD <- function(data, labeled=FALSE, parameter_learning_length=0.2,
                       parameters=parse_args(OptionParser(option_list = list())), weights=c(0, 0, 1, 0), min_max_switch=FALSE) {

  input_parameters <- convert_params_to_parsed_args(parameters)
  tuning_results <- osESD_Detector_auto(database=data, data_label=labeled, weights=weights,
                                        par_len=parameter_learning_length, parameters=input_parameters,
                                        min_max_switch=min_max_switch)

  pred_anoms <- grid_search_osESD(data=data$value, time=1:length(data$value),
                                  full_size=length(data$value), init_size=tuning_results$params[2],
                                  params=tuning_results$params)

  pred_anoms_index <- which(pred_anoms == 1)
  pred_anoms_index <- pred_anoms_index[pred_anoms_index > tuning_results$params[2]]

  return(list(pred_anoms_index=pred_anoms_index, params=tuning_results$params))
}


