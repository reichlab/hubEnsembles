context("build_quantile_ensemble")
library(hubEnsembles)

tmp_dat <- readr::read_csv("test-data/minimal-forecast.csv")

test_that("invalid method argument throws error", {
  expect_error(
    build_quantile_ensemble(tmp_dat,
                            method="weighted mean",
                            model_name = "example")
    )
})

test_that("medians and means correctly calculated", {
  fdat <- expand.grid(
    stringsAsFactors = FALSE,
    model = letters[1:4],
    location = c("222", "888"),
    horizon = 1,
    temporal_resolution = "wk",
    target_variable = "inc death",
    target_end_date = as.Date("2021-12-25"),
    type = "quantile",
    quantile = c(.1, .5, .9))

  fdat$value[fdat$location == "222" & fdat$quantile == .1] <- v2.1 <- c(10, 30, 15, 20)
  fdat$value[fdat$location == "222" & fdat$quantile == .5] <- v2.5 <- c(40, 40, 45, 50)
  fdat$value[fdat$location == "222" & fdat$quantile == .9] <- v2.9 <- c(60, 70, 75, 80)
  fdat$value[fdat$location == "888" & fdat$quantile == .1] <- v8.1 <- c(100, 300, 400, 250)
  fdat$value[fdat$location == "888" & fdat$quantile == .5] <- v8.5 <- c(150, 325, 500, 300)
  fdat$value[fdat$location == "888" & fdat$quantile == .9] <- v8.9 <- c(250, 350, 500, 350)

  med_vals <- sapply(list(v2.1, v2.5, v2.9, v8.1, v8.5, v8.9), median)
  mean_vals <- sapply(list(v2.1, v2.5, v2.9, v8.1, v8.5, v8.9), mean)

  median_actual <- build_quantile_ensemble(
    forecast_data = fdat, method = "median", model_name = "med_ens", forecast_date = "2021-12-20")

  mean_actual <- build_quantile_ensemble(
    forecast_data = fdat, method = "mean", model_name = "mean_ens", forecast_date = "2021-12-20")

  median_expected <- mean_expected <- tibble::tibble(
    location = rep(c("222", "888"), each = 3), 
    horizon = 1, 
    temporal_resolution = "wk", 
    target_variable = "inc death",     
    target_end_date = as.Date("2021-12-25"), 
    type = "quantile", 
    quantile = rep(c(.1, .5, .9), 2), 
    forecast_count = 4,
    value = 0,
    model = NA,
    forecast_date = as.Date("2021-12-20"))

  median_expected$value <- sapply(list(v2.1, v2.5, v2.9, v8.1, v8.5, v8.9), median)
  median_expected$model <- "med_ens"
  mean_expected$value <- sapply(list(v2.1, v2.5, v2.9, v8.1, v8.5, v8.9), mean)
  mean_expected$model <- "mean_ens"

  expect_equal(median_actual, median_expected)

  expect_equal(mean_actual, mean_expected)
})
