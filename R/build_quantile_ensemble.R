#' Create quantile (weighted or unweighted) ensemble using mean or median
#'
#' @param forecast_data `data.frame` containing all the forecasts to be summarised
#' as an ensemble.
#' @param weights_df `data.frame` containing weights for all the models included in `forecast_data`
#' to create a weighted ensemble, should include a `model` column and `weight` column. 
#' Defaults to NULL, which creates an unweighted ensemble.
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
#' @importFrom matrixStats weightedMedian weightedMean
#'
#' @export

build_quantile_ensemble <- function(
  forecast_data,
  weights_df = NULL,
  method = c("median", "mean"),
  model_name,
  forecast_date,
  location_data = NULL,
  location_col_name = "fips"
) {
  method <- match.arg(method)

  if ((length(weights_df) != 0) && all(names(weights_df) != c("model", "weight"))) {
    stop("weights_df did not have required columns", call. = FALSE)
  }


    # Mean
  if (method == "mean") {
    if (is.null(weights_df)) {
      ensemble <- forecast_data %>%
      group_by(location, horizon, temporal_resolution, 
        target_variable, target_end_date, type, quantile) %>%
      summarise(forecast_count = n(),
        value = mean(value),
        .groups = "drop")
    } else {
      ensemble <- forecast_data %>%
      left_join(weights_df, by = "model") %>%
      group_by(location, horizon, temporal_resolution, 
        target_variable, target_end_date, type, quantile) %>%
      summarise(forecast_count = n(),
        value = matrixStats::weightedMean(value, weight, na.rm = TRUE),
        .groups = "drop")
    }
    # Median
  } else if (method == "median") {
    if (is.null(weights_df)) {
      ensemble <- forecast_data %>%
      group_by(location, horizon, temporal_resolution, 
        target_variable, target_end_date, type, quantile) %>%
      summarise(forecast_count = n(),
        value = median(value),
        .groups = "drop")
    } else {
      ensemble <- forecast_data %>%
      left_join(weights_df, by = "model") %>%
      group_by(location, horizon, temporal_resolution, 
        target_variable, target_end_date, type, quantile) %>%
      summarise(forecast_count = n(),
        value = matrixStats::weightedMedian(value, weight, na.rm = TRUE),
        .groups = "drop")
    }
  }

  # add model name column
  if(!is.null(model_name))
    ensemble$model <- model_name

  # add forecast date to ensemble
  ensemble$forecast_date <- as.Date(forecast_date)

  if (!is.null(location_data)) {
  # warn if if there are any locations in the ensemble are not in location_data 
    mslocs <- setdiff(ensemble$location, location_data[[location_col_name]]) 
    if (length(mslocs) > 0) {
      warning(paste("Locations not present in location_data:", mslocs))
    }
    ensemble <- ensemble %>% 
    left_join(
      location_data,
      by = c("location" = location_col_name))
  }

  # Return ensemble
  return(ensemble)

}
