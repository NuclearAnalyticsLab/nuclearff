# Test getting play-by-play positional stats for TE
test_that("TE pbp stats snapshot test", {
  # Get TE stats
  te_pbp_stats <- get_te_pbp_stats(pbp_db = NULL,
                                   pbp_db_tbl = NULL,
                                   season = 2023,
                                   week_min = 1,
                                   week_max = 1)

  # Snapshot the result
  expect_snapshot(te_pbp_stats)
})
