################################################################################
# Author: Nolan MacDonald
# Purpose:
#   Obtain NFL play-by-play data with a database or using `nflreadr`
# Code Style Guide:
#   styler::tidyverse_style(), lintr::use_lintr(type = "tidyverse")
################################################################################

#' Helper - Validate Play-by-Play Database Path and Table Arguments
#'
#' @description
#' Validation helper to determine if the path to the play-by-play database,
#' `pbp_db`, and the database table name, `pbp_db_tbl`, are defined as strings.
#'
#' @details
#'  The function `validate_pbp_db` can be utilized to validate whether the
#'  arguments to load a database in `get_pbp_data` are valid and defined as
#'  strings.
#'
#' @seealso \code{\link[nuclearff]{get_pbp_data}}:
#'  Obtain filtered play-by-play data utilizes this helper function
#'
#' @param pbp_dp Play-by-Play database path (str)
#' @param pbp_db_tbl Play-by-Play database table name (str)
#'
#' @return Validate if `pbp_db` and `pbp_db_tbl` are strings
#'
#' @author Nolan MacDonald
#'
#' @export
validate_pbp_db <- function(pbp_db,
                            pbp_db_tbl) {
  # Helper function to validate play-by-play database inputs
  if (!is.null(pbp_db) && !is.null(pbp_db_tbl)) {
    if (!is.character(pbp_db) || !is.character(pbp_db_tbl)) {
      stop("Both pbp_db and pbp_db_tbl must be strings if provided")
    }
  }
}

#' Helper - Validate Play-by-Play Data Seasons
#'
#' Validation helper to determine if the seasons argument to obtain the
#' play-by-play database is 1999 or later.
#'
#' @details
#'  The function `validate_pbp_season` can be utilized to validate whether the
#'  arguments for `season` are appropriate as NFL play-by-play data dates back
#'  to 1999. If any season is defined prior to 1999, an error will occur.
#'
#' @seealso \code{\link[nuclearff]{get_pbp_data}}:
#'  Obtain filtered play-by-play data utilizes this helper function
#'
#' @param season NFL season to obtain play-by-play data. The
#'  season can be defined as a single season, `season = 2024`. For multiple
#'  seasons, use either `season = c(2023,2024)` or `season = 2022:2024`.
#'
#' @return Validate if `season` is defined as an integer of 1999 or greater
#'
#' @author Nolan MacDonald
#'
#' @export
validate_pbp_season <- function(season) {
  if (!is.numeric(season) || any(season <= 1999)) {
    stop("Please provide a valid season as an integer (1999 or later).")
  }
}

#' Helper - Validate Arguments for Weeks
#'
#' @description
#' Validation helper to determine if weeks are defined properly to filter the
#' play-by-play data.
#'
#' @details
#'  The function `validate_pbp_weeks` is utilized to determine weeks are
#'  defined as integers for `week_min` and `week_max`.
#'
#' @seealso \code{\link[nuclearff]{get_pbp_data}}:
#'  Obtain filtered play-by-play data utilizes this helper function
#'
#' @param week_min Minimum week to define whether pulling a range
#'  of weeks or the entire season. Use `week_min = 1` for the first week of
#'  the season, must be an integer.
#' @param week_max Maximum week to define a range of weeks to pull
#'  from an NFL season. If not defined, the data will be pulled for all weeks,
#'  beginning with `week_min`.
#'
#' @return Validate if weeks are defined as integers
#'
#' @author Nolan MacDonald
#'
#' @export
validate_pbp_weeks <- function(week_min,
                               week_max) {
  if (is.null(week_min) || !is.numeric(week_min) ||
        week_min != as.integer(week_min)) {
    stop("week_min is required. Provide an integer.")
  }

  if (!is.null(week_max) && (!is.numeric(week_max) ||
                               week_max != as.integer(week_max))) {
    stop("week_max is optional. Provide an integer or leave it NULL.")
  }
}

#' Helper - Load Play-by-Play Data from Database
#'
#' @description
#' Helper function to load play-by-play data from `nflreadr` for a specified
#' time frame.
#'
#' @details
#' Helper function to load play-by-play data from from a database. The function
#' filters the data to include a specified time frame including seasons
#' and a range of weeks.
#' If `week_max` is provided, filter between `week_min` and `week_max`
#' If `week_max` is not provided, filter for `weeks >= week_min` to get the
#' remainder of the season.
#' This function is typically utilized if the user
#' desires loading play-by-play data from a database and defined `pbp_db` and
#' `pbp_db_tbl` as strings.
#'
#' @seealso \code{\link[nflfastR]{update_db}}:
#'  Update or Create a nflfastR Play-by-Play Database
#' @seealso \code{\link[nuclearff]{get_pbp_data}}:
#'  Obtain filtered play-by-play data utilizes this helper function
#'
#' @param pbp_dp Play-by-Play database path
#' @param pbp_db_tbl Play-by-Play database table name
#' @param season NFL season to obtain play-by-play data. The
#'  season can be defined as a single season, `season = 2024`. For multiple
#'  seasons, use either `season = c(2023,2024)` or `season = 2022:2024`.
#' @param week_min Minimum week to define whether pulling a range
#'  of weeks or the entire season. Use `week_min = 1` for the first week of
#'  the season, must be an integer.
#' @param week_max Maximum week to define a range of weeks to pull
#'  from an NFL season. If not defined, the data will be pulled for all weeks,
#'  beginning with `week_min`.
#'
#' @return Dataframe containing filtered play-by-play data
#'
#' @author Nolan MacDonald
#'
#' @export
load_data_from_db <- function(pbp_db, pbp_db_tbl, season, week_min, week_max) {
  connect <- DBI::dbConnect(RSQLite::SQLite(), pbp_db)
  pbp_db <- dplyr::tbl(connect, pbp_db_tbl)

  pbp <- pbp_db %>%
    dplyr::filter(
      season %in% !!season,
      (is.null(week_max) & !!sym("week") >= !!week_min) |
        (!is.null(week_max) & !!sym("week") >= !!week_min &
           !!sym("week") <= !!week_max)
    ) %>%
    dplyr::collect()

  DBI::dbDisconnect(connect)

  return(pbp)
}

#' Helper - Load Play-by-Play Data from `nflreadr`
#'
#' @description
#' Helper function to load play-by-play data from `nflreadr` for a specified
#' time frame.
#'
#' @details
#' Helper function to load play-by-play data from `nflreadr`. The function
#' filters the data to include a specified time frame including seasons
#' and a range of weeks.
#' If `week_max` is provided, filter between `week_min` and `week_max`
#' If `week_max` is not provided, filter for `weeks >= week_min` to get the
#' remainder of the season.
#' This function is typically utilized if the user is
#' not loading play-by-play data from a database.
#'
#' @seealso \code{\link[nflreadr]{load_pbp}}:
#'  Load play-by-play data
#' @seealso \code{\link[nuclearff]{get_pbp_data}}:
#'  Obtain filtered play-by-play data utilizes this helper function
#'
#' @param season NFL season to obtain play-by-play data. The
#'  season can be defined as a single season, `season = 2024`. For multiple
#'  seasons, use either `season = c(2023,2024)` or `season = 2022:2024`.
#' @param week_min Minimum week to define whether pulling a range
#'  of weeks or the entire season. Use `week_min = 1` for the first week of
#'  the season, must be an integer.
#' @param week_max Maximum week to define a range of weeks to pull
#'  from an NFL season. If not defined, the data will be pulled for all weeks,
#'  beginning with `week_min`.
#'
#' @return Dataframe containing filtered play-by-play data
#'
#' @author Nolan MacDonald
#'
#' @export
load_data_from_nflreadr <- function(season, week_min, week_max) {
  pbp <- nflreadr::load_pbp(season) %>%
    dplyr::filter(
      season %in% !!season,
      (is.null(week_max) & !!sym("week") >= !!week_min) |
        (!is.null(week_max) & !!sym("week") >= !!week_min &
           !!sym("week") <= !!week_max)
    )

  return(pbp)
}

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
