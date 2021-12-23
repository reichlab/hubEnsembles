#' Run ensembling methods
#'
#' @param method name of ensembling method
#' @param forecast_date date or character
#' @param hub specification of hub
#' @param data_processed_subpath hub-dependent, passed to load_forecasts
#' @param criteria filtering criteria,
#' @param quantiles numeric vector of quantiles
#' @param locations character vector of locations to ensemble
#' @param target_variables description of target_variables to include in ensemble
#' @param horizons numeric vector of horizons to include
#' @param exclude_models data frame of submissions such as other ensembles to be excluded
#' @param return_criteria return data frame with criteria passing info?
#' @param verbose boolean indicator of whether to print messages
#' @param exclude_designated_other not sure what this is.
#'
#' @details
#' Used to create a single ensemble forecast.
#' Takes a forecast date and loads all forecasts for the preceding week, then:
#' Filters models according to criteria
#' Ensembles forecasts according to given method
#' Formats ensemble forecast
#' Returns ensemble forecast and optionally the criteria for inclusion
#'
#' @importFrom covidHubUtils load_forecasts
#' @importFrom dplyr %>% filter pull mutate group_by summarise if_else
#'
#' @export

run_ensemble <- function(
  method = "median",
  forecast_date,
  hub = "FluSight",
  data_processed_subpath = "data-forecasts/",
  criteria = c(),
  quantiles = round(c(0.01, 0.025, seq(0.05, 0.95, by = 0.05), 0.975, 0.99), 3),
  locations = covidHubUtils::hub_locations_flusight,
  target_variables = c("inc flu hosp"),
  horizons = 1:4,
  exclude_models = NULL,
  return_criteria = TRUE,
  verbose = FALSE,
  exclude_designated_other = TRUE) {

  # Dates ------------------------------------------------------------------
  # determine forecast dates matching the forecast date
  forecast_date <- as.Date(forecast_date)
  forecast_dates <- seq.Date(from = forecast_date,
                             by = -1,
                             length.out = 6)

  # Load forecasts and save criteria --------------------------------------------
  # Get all forecasts
  all_forecasts <- suppressMessages(
    load_forecasts(source = "local_hub_repo",
                   hub_repo_path = here(),
                   hub = hub,
                   data_processed_subpath = data_processed_subpath,
                   dates = forecast_dates,
                   verbose = FALSE))

  if (verbose) {message(paste0(
    "Forecasts loaded from ",
    as.character(min(forecast_dates)), " to ",
    as.character(max(forecast_dates))))
  }

  # Exclusions --------------------------------------------------------------
  # If manual exclusion is csv, convert to vector
  if ("data.frame" %in% class(exclude_models)) {
    exclude_models <- exclude_models %>%
      filter(forecast_date == !!forecast_date) %>%
      pull(model)
  }

  # Filter by inclusion criteria
  forecasts <- use_ensemble_criteria(
    forecasts = all_forecasts,
    criteria = criteria,
    quantiles = quantiles,
    locations = locations,
    target_variables = target_variables,
    horizons = horizons,
    exclude_models = exclude_models,
    return_criteria = return_criteria,
    exclude_designated_other = exclude_designated_other)

  if (return_criteria) {
    criteria <- forecasts$criteria
    forecasts <- forecasts$forecasts
  }

  forecasts <- forecasts %>%
    filter(type == "quantile") %>%
    mutate(quantile = round(quantile, 3),
           horizon = as.numeric(horizon))

  # Run  ensembles ---------------------------------------------------
  # Averages
  if (method %in% c("mean", "median")) {
    ensemble <- create_ensemble_average(method = method,
                                        forecasts = forecasts)
  }

  # Add other ensemble methods here as:
  #   if (method == "method") {
  #     ensemble <- method_function_call()
  #   }

  # Format and return -----------------------------------------------------------
  ensemble <- format_ensemble(ensemble = ensemble,
                              forecast_date = max(forecast_dates))

  if (verbose) {message("Ensemble formatted in hub standard")}

  if (return_criteria) {
    return(list("ensemble" = ensemble,
                "criteria" = criteria,
                "method" = method,
                "forecast_date" = max(forecast_dates)))
  }

  return(ensemble)
}
