#' Create quantile ensemble using mean or median
#'
#' @param forecast_data `data.frame` containing all the forecasts to be summarised
#' as an ensemble.
#' @param method One of `median` (default) or `mean`.
#' @param model_name character string for `model` column in output; if NULL no such column will be added
#' @param forecast_date character string interpretable as a date which will populate the forecast_date column of
#' the ensemble data frame 
#' @param location_data an optional data.frame with metadata about location. If present it will be left joined
#' with the ensemble data frame on the column `location_col_name`. This is useful when passing the ensemble to
#' `plot_forecasts` which requires full location names.
#' @param location_col_name character string giving the column name in `location_data` matching
#' with the `location` column in `forecast_data`. Defaults to "fips".
#' @return ensemble model
#'
#' @importFrom dplyr group_by %>% summarise n left_join
#' @importFrom stats median
#'
#' @export

build_quantile_ensemble <- function(
  forecast_data,
  method = c("median", "mean"),
  model_name,
  forecast_date,
  location_data = NULL,
  location_col_name = "fips"
) {
  method <- match.arg(method)

    # Mean
  if (method == "mean") {
    ensemble <- forecast_data %>%
    group_by(location, horizon, temporal_resolution, 
      target_variable, target_end_date, type, quantile) %>%
    summarise(forecast_count = n(),
      value = mean(value),
      .groups = "drop")
    # Median
  } else if (method == "median") {
    ensemble <- forecast_data %>%
    group_by(location, horizon, temporal_resolution, 
      target_variable, target_end_date, type, quantile) %>%
    summarise(forecast_count = n(),
      value = median(value),
      .groups = "drop")
  }

  # add model name column
  if(!is.null(model_name))
    ensemble$model <- model_name

  # add forecast date to ensemble
  ensemble$forecast_date <- as.Date(forecast_date)

  if (!is.null(location_data)) {
    ensemble <- ensemble %>% 
    left_join(
      location_data,
      by = c("location" = location_col_name))
  }

  # Return ensemble
  return(ensemble)

}
