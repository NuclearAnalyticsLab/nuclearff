################################################################################
# Author: Nolan MacDonald
# Purpose:
#   Obtain NFL play-by-play data with a database or using `nflreadr`
# Code Style Guide:
#   styler::tidyverse_style(), lintr::use_lintr(type = "tidyverse")
################################################################################

#' Obtain Play-by-Play Data for Specified Time Frame
#'
#' @description
#' Obtain play-by-play data for a specified time frame from either a saved
#' database or if not defined, using `nflreadr::load_pbp()`.
#'
#' @details
#'  The function `get_pbp_data` can be utilized to obtain play-by-play data
#'  by either loading from a saved database or using `nflreadr::load_pbp()`.
#'  The data is obtained for a user-defined season or multiple seasons.
#'  A range of weeks can also be defined with `week_min` and `week_max`.
#'  If the entire season is desired, use `week_min = 1` and `week_max` does
#'  not need to be defined.
#'  To specify loading from a database, define the path to the database with
#'  `pbp_db` as well as the name of the table to load with `pbp_db_tbl`.
#'  To load from a database, you will need to save play-by-play data to a
#'  database using the `nflfastR` function, `update_db()`.
#'  For example, the database is saved by default as `pbp_db` with a table that
#'  is stored containing all play-by-play information called `nflfastR_pbp`.
#'  Assume that the database is saved in `project_name/data/`.
#'  Using the default naming scheme, `get_pbp_data` can be defined using the
#'  database with `pbp_db = "data/pbp_db"` and `pbp_db_tbl = "nflfastR_pbp"`.
#'  Note that these two arguments must be defined as strings.
#'  For more information, `nflfastR` provides an example on using the database
#'  in [Example 8: Using the built-in database function]
#'  (https://www.nflfastr.com/articles/nflfastR.html
#'  #example-8-using-the-built-in-database-function)
#'
#'
#' @seealso \code{\link[nflreadr]{load_pbp}}
#'  Load play-by-play data,
#' @seealso \code{\link[nflfastR]{update_db}}
#'  Update or Create a nflfastR Play-by-Play Database
#'
#' @param pbp_dp Play-by-Play database path (optional)
#' @param pbp_db_tbl Play-by-Play database table name (optional)
#' @param season NFL season (required) to obtain play-by-play data. The
#'  season can be defined as a single season, `season = 2024`. For multiple
#'  seasons, use either `season = c(2023,2024)` or `season = 2022:2024`.
#' @param week_min Minimum week (required) to define whether pulling a range
#'  of weeks or the entire season. Use `week_min = 1` for the first week of
#'  the season, must be an integer.
#' @param week_max Maximum week (optional) to define a range of weeks to pull
#'  from an NFL season. If not defined, the data will be pulled for all weeks,
#'  beginning with `week_min`.
#'
#' @return Dataframe with NFL play-by-play data
#'
#' @author Nolan MacDonald
#'
#' \itemize{
#'  \item{\code{connect}}{
#'    Connect to the play-by-play database with the `RSQLite` package
#'    }
#'  \item{\code{pbp_db}}{Load the play-by-play data from the database table}
#'  \item{\code{pbp}}{Play-by-play dataframe filtered for season(s) and week(s)}
#'  }
#'
#' @export
get_pbp_data <- function(pbp_db = NULL,
                         pbp_db_tbl = NULL,
                         season = NULL,
                         week_min = NULL,
                         week_max = NULL) {
  # Validate play-by-play inputs
  nuclearff::validate_pbp_db(pbp_db, pbp_db_tbl)
  nuclearff::validate_pbp_season(season)
  nuclearff::validate_pbp_weeks(week_min, week_max)

  if (!is.null(pbp_db) && !is.null(pbp_db_tbl)) {
    pbp <- nuclearff::load_data_from_db(pbp_db,
                                        pbp_db_tbl,
                                        season,
                                        week_min,
                                        week_max)
  } else {
    pbp <- nuclearff::load_data_from_nflreadr(season,
                                              week_min,
                                              week_max)
  }

  return(pbp)
}
