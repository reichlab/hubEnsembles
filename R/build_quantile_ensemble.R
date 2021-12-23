#' Create quantile ensemble using mean or median
#'
#' @param forecast_data `data.frame` containing all the forecasts to be summarised
#' as an ensemble.
#' @param method One of `median` (default) or `mean`.
#' @param model_name character string for `model` column in output
#' @param forecast_date a character string interpretable as a date
#' @param location_data am optional data.frame with metadata about location
#'
#' @return ensemble model
#'
#' @importFrom dplyr group_by %>% summarise n
#' @importFrom stats median
#'
#' @export

build_quantile_ensemble <- function(
  forecast_data,
  method = c("median", "mean"),
  model_name,
  forecast_date,
  location_data = NULL
) {
  method <- match.arg(method)

}
