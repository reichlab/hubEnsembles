#' Create ensemble using mean or median average.
#'
#' @param forecasts `data.frame` containing all the forecasts to be summarised
#' as an ensemble.
#' @param method One of `mean` (default) or `median`.
#' @param model_name character string for `model` column in output
#'
#' @return ensemble model
#'
#' @importFrom dplyr group_by %>% summarise n
#' @importFrom stats median
#'
#' @export

create_ensemble_average <- function(
  forecasts,
  method = c("mean", "median"),
  model_name = NULL) {

  method <- match.arg(method)

  # Mean
  if (method == "mean") {
    ensemble <- forecasts %>%
      group_by(forecast_date, target_variable, horizon, temporal_resolution,
               target_end_date, location, type, quantile) %>%
      summarise(forecasts = n(),
                value = mean(value),
                .groups = "drop")
    # Median
  } else if (method == "median") {
    ensemble <- forecasts %>%
      group_by(forecast_date, target_variable, horizon, temporal_resolution,
               target_end_date, location, type, quantile) %>%
      summarise(forecasts = n(),
                value = median(value),
                .groups = "drop")
  }

  ## add model name column if present
  if(!is.null(model_name))
    ensemble$model <- model_name

  # Return ensemble
  return(ensemble)
}



