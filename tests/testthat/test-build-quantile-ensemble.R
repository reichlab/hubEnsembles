library(hubEnsembles)

tmp_dat <- readr::read_csv("test-data/minimal-forecast.csv")

test_that("invalid method argument throws error", {
  expect_error(
    build_quantile_ensemble(tmp_dat,
                            method="weighted mean",
                            model_name = "example")
    )
})
