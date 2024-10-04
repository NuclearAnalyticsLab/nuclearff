library(testthat)
library(dplyr)
library(rlang)
library(nflreadr)

# Test getting play-by-play data
test_that("get_pbp_data snapshot test", {
  # Call your function with sample inputs
  pbp_output <- get_pbp_data(pbp_db = NULL,
                             pbp_db_tbl = NULL,
                             season = 2023,
                             week_min = 1,
                             week_max = 1)

  # Snapshot the result
  expect_snapshot(pbp_output)
})
