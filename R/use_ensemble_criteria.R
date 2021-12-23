#' Load models according to inclusion criteria before ensembling
#'
#' @inheritParams create_ensemble_average
#' @param exclude_models optional character vector to exclude over all dates,
#'   or data.frame with cols model and forecast_date, to exclude for specific
#'    dates
#' @param exclude_designated_other logical: whether to exclude models designated
#' as "other" in their metadata file (default `TRUE`)
#' @param return_criteria logical : whether to return a model/inclusion criteria
#' grid as well as the ensemble forecast (default `TRUE`)
#'
#' @return
#' - if `return_criteria = TRUE`, a list with the following elements
#'   * "ensemble" : tibble : a single ensemble forecast
#'   * "criteria": tibble : all candidate models against criteria
#'     for inclusion in ensemble (all locations and horizons)
#'   * "forecast_date" : date : latest date
#' - if `return_criteria = FALSE`, a tibble of a single ensemble forecast
#'
#' @details
#' Steps:
#' Currently, models included based on having:
#' 1. All quantiles
#' 2. 4 horizons
#' 3. Not manually specified for exclusion
#' 4. Not the hub ensemble
#'
#' @importFrom dplyr filter %>% group_by summarise mutate left_join select inner_join
#' @importFrom here here
#'
#' @export
use_ensemble_criteria <- function(forecasts,
                                  exclude_models = NULL,
                                  criteria,
                                  quantiles,
                                  locations,
                                  target_variables,
                                  horizons,
                                  exclude_designated_other = TRUE,
                                  return_criteria = TRUE) {

  # Remove point forecasts and extra quantiles or horizons
  forecasts <- forecasts %>% filter(
    type == "quantile",
    quantile %in% quantiles,
    horizon %in% horizons)

  criteria_df <- tidyr::expand_grid(
    model = unique(forecasts$model),
    location = locations, 
    target_variable = target_variables, 
    horizon = horizons)

  if ("all_quantiles" %in% criteria | "all_quantiles_all_horizons" %in% criteria) {
  # Identify models with all quantiles
    criteria_df <- criteria_df %>% 
    left_join(forecasts %>%
    # Check all quantiles per target/location
    group_by(model, target_variable, location, horizon) %>%
    summarise(all_quantiles =
      (length(setdiff(quantiles, quantile)) == 0),
      .groups = "drop"))
  }

  if ("all_quantiles_all_horizons" %in% criteria) {
  # Identify models with all quantiles at all horizons
    criteria_df <- criteria_df %>% 
    group_by(model, target_variable, location) %>% 
    mutate(all_quantiles_all_horizons = all(all_quantiles),
      .groups = "drop") %>% 
    ungroup()
  }

  if ("all_horizons" %in% criteria) {
  # Identify models with forecasts of some quantiles at all horizons
    criteria_df <- criteria_df %>% 
    left_join(forecasts %>%
    group_by(model, target_variable, location) %>%
    summarise(all_horizons =
      (length(setdiff(horizons, horizon)) == 0),
      .groups = "drop"))
  }

  # Drop "other" designated models
  if (exclude_designated_other) {
    criteria_df <- criteria_df %>%
    left_join(get_model_metadata(hub_repo_path = here()) %>%
    transmute(model = model, not_other = designation != "other"))
  }

  # Manually excluded forecasts
  criteria_df <- criteria_df %>%
    mutate(not_excluded_manually = !(model %in% exclude_models))

  criteria_df <- criteria_df %>% 
  rowwise() %>% 
  mutate(included_in_ensemble = all(c_across(-c(model, location, target_variable, horizon))))

  # Create an inclusion list
  include <- criteria_df %>% 
  filter(included_in_ensemble = TRUE) %>%
  select(model, target_variable, location, horizon) 

  # Return
  forecasts <- inner_join(forecasts, include,
                          by = c("model", "target_variable", "location", "horizon"))

  if (return_criteria) {
    return(list("forecasts" = forecasts,
                "criteria" = criteria_df))
  }

  return(forecasts)
}
