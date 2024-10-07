################################################################################
# Author: Nolan MacDonald
# Purpose:
#   Helper functions to validate inputs for obtaining advanced stats
# Code Style Guide:
#   styler::tidyverse_style(), lintr::use_lintr(type = "tidyverse")
################################################################################

#' Helper - Validate NFL Next Gen Stats (NGS) Data Seasons
#'
#' @description
#' Validation helper to determine if the seasons argument to obtain NGS is
#' 2016 or later.
#'
#' @details
#'  The function `validate_ngs_season` can be utilized to validate whether the
#'  arguments for `season` are appropriate as NFL NGS dates back
#'  to 2016. If any season is defined prior to 2016, an error will occur.
#'
#' @seealso \code{\link[nuclearff]{get_qb_ngs_advstats_season}}:
#'  Obtain QB cumulative season stats from `nflreadr`, to acquire
#'  Next Gen Stats (NGS) data. The function utilizes this helper.
#' @seealso \code{\link[nuclearff]{get_rb_ngs_advstats_season}}:
#'  Obtain RB cumulative season stats from `nflreadr`, to acquire
#'  Next Gen Stats (NGS) data. The function utilizes this helper.
#' @seealso \code{\link[nuclearff]{get_wr_ngs_advstats_season}}:
#'  Obtain WR cumulative season stats from `nflreadr`, to acquire
#'  Next Gen Stats (NGS) data. The function utilizes this helper.
#' @seealso \code{\link[nuclearff]{get_te_ngs_advstats_season}}:
#'  Obtain TE cumulative season stats from `nflreadr`, to acquire
#'  Next Gen Stats (NGS) data. The function utilizes this helper.
#'
#' @param season NFL season to obtain NGS data. The
#'  season can be defined as a single season, `season = 2024`. For multiple
#'  seasons, use either `season = c(2023,2024)` or `season = 2022:2024`.
#'
#' @return Validate if `season` is defined as an integer of 2016 or greater
#'
#' @author Nolan MacDonald
#'
#' @export
validate_ngs_season <- function(season) {
  if (!is.numeric(season) || any(season <= 2016)) {
    stop("Please provide a valid season as an integer (2016 or later).")
  }
}

#' Helper - Validate Pro Football Reference (PFR) Advanced Stats Seasons
#'
#' @description
#' Validation helper to determine if the seasons argument to obtain PFR is
#' 2018 or later.
#'
#' @details
#'  The function `validate_pfr_season` can be utilized to validate whether the
#'  arguments for `season` are appropriate as PFR advanced stats dates back
#'  to 2018. If any season is defined prior to 2018, an error will occur.
#'
#' @seealso \code{\link[nuclearff]{get_qb_pfr_advstats_season}}:
#'  Obtain QB advanced season stats from `nflreadr`, to acquire Pro Football
#'  Reference (PFR) data. The function utilizes this helper.
#' @seealso \code{\link[nuclearff]{get_rb_pfr_advstats_season}}:
#'  Obtain RB advanced season stats from `nflreadr`, to acquire Pro Football
#'  Reference (PFR) data. The function utilizes this helper.
#' @seealso \code{\link[nuclearff]{get_wr_pfr_advstats_season}}:
#'  Obtain WR advanced season stats from `nflreadr`, to acquire Pro Football
#'  Reference (PFR) data. The function utilizes this helper.
#' @seealso \code{\link[nuclearff]{get_te_pfr_advstats_season}}:
#'  Obtain TE advanced season stats from `nflreadr`, to acquire Pro Football
#'  Reference (PFR) data. The function utilizes this helper.
#'
#' @param season NFL season to obtain NGS data. The
#'  season can be defined as a single season, `season = 2024`. For multiple
#'  seasons, use either `season = c(2023,2024)` or `season = 2022:2024`.
#'
#' @return Validate if `season` is defined as an integer of 2018 or greater
#'
#' @author Nolan MacDonald
#'
#' @export
validate_pfr_season <- function(season) {
  if (!is.numeric(season) || any(season <= 2018)) {
    stop("Please provide a valid season as an integer (2018 or later).")
  }
}
