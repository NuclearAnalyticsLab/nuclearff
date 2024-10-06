################################################################################
# Author: Nolan MacDonald
# Purpose:
#   Obtain Positional Cumulative Season Advanced Statistics from NFL
#   Next Gen Stats (NGS).
# Code Style Guide:
#   styler::tidyverse_style(), lintr::use_lintr(type = "tidyverse")
################################################################################

#' QB Stats from NFL Play-by-Play Data
#'
#' @description
#' Obtain QB stats from NFL play-by-play data for a specified time frame
#' from either a saved database or `nflreadr::load_pbp()`.
#'
#' @details
#'  The function `get_qb_pbp_stats` can be utilized to obtain player stats
#'  for a specified time frame that can include multiple seasons or even a
#'  range of weeks instead of the entire season.
#'  This includes stats for passing, rushing, and receiving obtained by using
#'  the `nflfastR` function `calculate_player_stats()`.
#'  The player stats are utilized to calculate fantasy points
#'  based on common scoring formats (4/6 point TD, STD, Half PPR, Full PPR).
#'  The function acquires stats by using cumulative play-by-play data
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
#'  Using the default naming scheme, `nuclearff::get_pbp_data` can be defined using the
#'  database with `pbp_db = "data/pbp_db"` and `pbp_db_tbl = "nflfastR_pbp"`.
#'  Note that these two arguments must be defined as strings.
#'  For more information on calculated player stats, refer to the
#'  [nflfastR `calculate_player_stats()`](
#'    https://www.nflfastr.com/reference/calculate_player_stats.html
#'    )
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
#' @return Dataframe with QB stats for user-defined season(s) and week(s)
#'  obtained from NFL play-by-play data
#'
#' @seealso \code{\link[nuclearff]{nuclearff::get_pbp_data}}
#'  Obtain play-by-play data for a specified time frame from either a saved
#'  database or if not defined, using `nflreadr::load_pbp()`,
#' @seealso \code{\link[nflreadr]{load_pbp}}
#'  Load play-by-play data,
#' @seealso \code{\link[nflfastR]{update_db}}
#'  Update or Create a nflfastR Play-by-Play Database
#'
#' @author Nolan MacDonald
#'
#' \itemize{
#'  \item{\code{player_id}}{Player gsis id (e.g., 00-0034796)}
#'  \item{\code{player_display_name}}{Player name (e.g., Lamar Jackson)}
#'  \item{\code{player_name}}{Player shortened name (e.g., L.Jackson)}
#'  \item{\code{position}}{Player position (e.g., QB)}
#'  \item{\code{team}}{Player team (e.g., BAL)}
#'  \item{\code{games}}{Number of games played}
#'  \item{\code{fpts_std_4pt_td}}{
#'    Fantasy points for standard format with 4 point TD
#'    }
#'  \item{\code{ppg_std_4pt_td}}{
#'    Fantasy points per game for standard format with 4 point TD
#'    }
#'  \item{\code{fpts_half_ppr_4pt_td}}{
#'    Fantasy points for half PPR format with 4 point TD
#'    }
#'  \item{\code{ppg_half_ppr_4pt_td}}{
#'    Fantasy points per game for half PPR format with 4 point TD
#'    }
#'  \item{\code{fpts_ppr_4pt_td}}{
#'    Fantasy points for full PPR format with 4 point TD
#'    }
#'  \item{\code{ppg_ppr_4pt_td}}{
#'    Fantasy points per game for full PPR format with 4 point TD
#'    }
#'  \item{\code{fpts_std_6pt_td}}{
#'    Fantasy points for standard format with 6 point TD
#'    }
#'  \item{\code{ppg_std_6pt_td}}{
#'    Fantasy points per game for standard format with 6 point TD
#'    }
#'  \item{\code{fpts_half_ppr_6pt_td}}{
#'    Fantasy points for half PPR format with 6 point TD
#'    }
#'  \item{\code{ppg_half_ppr_6pt_td}}{
#'    Fantasy points per game for half PPR format with 6 point TD
#'    }
#'  \item{\code{fpts_ppr_6pt_td}}{
#'    Fantasy points for full PPR format with 6 point TD
#'    }
#'  \item{\code{ppg_ppr_6pt_td}}{
#'    Fantasy points per game for full PPR format with 6 point TD
#'    }
#'  \item{\code{completions}}{Total pass completions (CMP)}
#'  \item{\code{attempts}}{Total pass attempts (ATT)}
#'  \item{\code{cmp_pct}}{Pass completion percentage}
#'  \item{\code{passing_yards}}{Total passing yards}
#'  \item{\code{passing_tds}}{Total passing touchdowns}
#'  \item{\code{interceptions}}{Total pass interceptions (INT)}
#'  \item{\code{sacks}}{Total number of sacks taken}
#'  \item{\code{sack_yards}}{Total yards taken from sacks}
#'  \item{\code{sack_fumbles}}{Total fumbles from sacks}
#'  \item{\code{sack_fumbles_lost}}{Total fumbles lost from sacks}
#'  \item{\code{passing_air_yards}}{
#'    Passing air yards (includes incomplete passes)
#'    }
#'  \item{\code{passing_yards_after_catch}}{
#'    Yards after the catch gained on plays in which player was the passer
#'    (this is an unofficial stat and may differ slightly between different
#'    sources).
#'    }
#'  \item{\code{passing_first_downs}}{First downs on pass attempts}
#'  \item{\code{passing_epa}}{
#'    Total expected points added (EPA) on pass attempts and sacks.
#'    NOTE: This uses the variable `qb_epa`, which gives QB credit for EPA for
#'    up to the point where a receiver lost a fumble after a completed catch
#'    and makes EPA work more like passing yards on plays with fumbles
#'    }
#'  \item{\code{passing_2pt_conversions}}{Two-point conversion passes}
#'  \item{\code{pacr}}{
#'    Passing Air Conversion Ratio. PACR = `passing_yards` / `passing_air_yards`
#'    }
#'  \item{\code{dakota}}{
#'    Adjusted EPA + CPOE composite based on coefficients which best predicts
#'    adjusted EPA/play in the following year
#'    }
#'  \item{\code{carries}}{
#'    Number of rush attempts including scrambles and kneel downs. Rushes after
#'    a lateral reception don't count as a carry
#'    }
#'  \item{\code{rushing_yards}}{
#'    Yards gained when rushing including scrambles and kneel downs. Also
#'    includes yards gained after obtaining a lateral on a play that started
#'    with a rushing attempt
#'    }
#'  \item{\code{rushing_tds}}{
#'    The number of rushing touchdowns (incl. scrambles). Also includes
#'    touchdowns after obtaining a lateral on a play that started with a
#'    rushing attempt
#'    }
#'  \item{\code{rushing_fumbles}}{Number of rushes with a fumble}
#'  \item{\code{rushing_fumbles_lost}}{Number of rushes with a lost fumble}
#'  \item{\code{rushing_first_downs}}{Number of rushing first downs}
#'  \item{\code{rushing_epa}}{
#'    Expected points added (EPA) on rush attempts including scrambles and
#'    kneel downs
#'    }
#'  \item{\code{rushing_2pt_conversions}}{Two-point conversion rushes}
#'  \item{\code{targets}}{
#'    Number of pass plays where the player was targeted as a receiver
#'    }
#'  \item{\code{receptions}}{
#'    Number of pass receptions. Lateral receptions don't count as a reception
#'    }
#'  \item{\code{receiving_yards}}{
#'    Yards gained after a pass reception. Includes yards gained after
#'    receiving a lateral on a play that started as a pass play
#'    }
#'  \item{\code{receiving_tds}}{
#'    Number of reception touchdowns, including after receiving a lateral on a
#'    play that began as a pass play
#'    }
#'  \item{\code{receiving_fumbles}}{Number of fumbles after a pass reception}
#'  \item{\code{receiving_fumbles_lost}}{
#'    Number of fumbles lost after a pass reception
#'    }
#'  \item{\code{receiving_air_yards}}{
#'    Receiving air yards including incomplete passes
#'    }
#'  \item{\code{receiving_yards_after_catch}}{
#'    Yards after the catch gained on plays in which player was receiver (this
#'    is an unofficial stat and may differ slightly between different sources)
#'    }
#'  \item{\code{receiving_first_downs}}{
#'    Number of first downs gained on a reception
#'    }
#'  \item{\code{receiving_2pt_conversions}}{Two-point conversion receptions}
#'  }
#'
#' @export
get_qb_pbp_stats <- function(pbp_db = NULL,
                             pbp_db_tbl = NULL,
                             season = NULL,
                             week_min = NULL,
                             week_max = NULL) {
  # Load play-by-play data with database or nflreadr
  pbp <- nuclearff::get_pbp_data(pbp_db, pbp_db_tbl, season, week_min, week_max)

  # Filter data for RB stats and compile
  qb_pbp <- pbp %>%
    nflfastR::calculate_player_stats() %>%
    # Calculate fantasy points for STD/Half PPR/Full PPR scoring
    nuclearff::calc_fpts_common_formats() %>%
    nuclearff::calc_fpts_ppg_common_formats() %>%
    # Calculations
    dplyr::mutate(
      # Completion percentage (CMP\%)
      cmp_pct = (completions / attempts) * 100,
      # Target share percentage (TGT\%)
      tgt_pct = target_share * 100
    ) %>%
    dplyr::inner_join(
      nflfastR::fast_scraper_roster(season) %>%
        # Filter by player position
        dplyr::filter(position == "QB") %>%
        dplyr::select(player_id = gsis_id),
      by = "player_id"
    ) %>%
    # Select columns to keep
    dplyr::select(player_id,
      player_display_name,
      player_name,
      position,
      team = recent_team,
      games,
      # Fantasy points for common scoring formats
      # Calculated from nuclearff functions
      fpts_std_4pt_td, ppg_std_4pt_td,
      fpts_half_ppr_4pt_td, ppg_half_ppr_4pt_td,
      fpts_ppr_4pt_td, ppg_ppr_4pt_td,
      fpts_std_6pt_td, ppg_std_6pt_td,
      fpts_half_ppr_6pt_td, ppg_half_ppr_6pt_td,
      fpts_ppr_6pt_td, ppg_ppr_6pt_td,
      # Play-by-Play Stats
      # Passing
      completions,
      attempts,
      cmp_pct,
      passing_yards,
      passing_tds,
      interceptions,
      sacks,
      sack_yards,
      sack_fumbles,
      sack_fumbles_lost,
      passing_air_yards,
      passing_yards_after_catch,
      passing_first_downs,
      passing_epa,
      passing_2pt_conversions,
      pacr,
      dakota,
      # Rushing
      carries,
      rushing_yards,
      rushing_tds,
      rushing_fumbles,
      rushing_fumbles_lost,
      rushing_first_downs,
      rushing_epa,
      rushing_2pt_conversions,
      # Receiving
      targets,
      receptions,
      receiving_yards,
      receiving_tds,
      receiving_fumbles,
      receiving_fumbles_lost,
      receiving_air_yards,
      receiving_yards_after_catch,
      receiving_first_downs,
      receiving_2pt_conversions
    )

  return(qb_pbp)
}

#' RB Stats from NFL Play-by-Play Data
#'
#' @description
#' Obtain RB stats from NFL play-by-play data for a specified time frame
#' from either a saved database or `nflreadr::load_pbp()`.
#'
#' @details
#'  The function `get_rb_pbp_stats` can be utilized to obtain player stats
#'  for a specified time frame that can include multiple seasons or even a
#'  range of weeks instead of the entire season.
#'  This includes stats for passing, rushing, and receiving obtained by using
#'  the `nflfastR` function `calculate_player_stats()`.
#'  The player stats are utilized to calculate fantasy points
#'  based on common scoring formats (4/6 point TD, STD, Half PPR, Full PPR).
#'  The function acquires stats by using cumulative play-by-play data
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
#'  Using the default naming scheme, `nuclearff::get_pbp_data` can be defined using the
#'  database with `pbp_db = "data/pbp_db"` and `pbp_db_tbl = "nflfastR_pbp"`.
#'  Note that these two arguments must be defined as strings.
#'  For more information on calculated player stats, refer to the
#'  [nflfastR `calculate_player_stats()`](
#'    https://www.nflfastr.com/reference/calculate_player_stats.html
#'    )
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
#' @return Dataframe with RB stats for user-defined season(s) and week(s)
#'  obtained from NFL play-by-play data
#'
#' @seealso \code{\link[nuclearff]{nuclearff::get_pbp_data}}
#'  Obtain play-by-play data for a specified time frame from either a saved
#'  database or if not defined, using `nflreadr::load_pbp()`,
#' @seealso \code{\link[nflreadr]{load_pbp}}
#'  Load play-by-play data,
#' @seealso \code{\link[nflfastR]{update_db}}
#'  Update or Create a nflfastR Play-by-Play Database
#'
#' @author Nolan MacDonald
#'
#' \itemize{
#'  \item{\code{player_id}}{Player gsis id (e.g., 00-0038120)}
#'  \item{\code{player_display_name}}{Player name (e.g., Breece Hall)}
#'  \item{\code{player_name}}{Player shortened name (e.g., B.Hall)}
#'  \item{\code{position}}{Player position (e.g., RB)}
#'  \item{\code{team}}{Player team (e.g., NYJ)}
#'  \item{\code{games}}{Number of games played}
#'  \item{\code{fpts_std_4pt_td}}{
#'    Fantasy points for standard format with 4 point TD
#'    }
#'  \item{\code{ppg_std_4pt_td}}{
#'    Fantasy points per game for standard format with 4 point TD
#'    }
#'  \item{\code{fpts_half_ppr_4pt_td}}{
#'    Fantasy points for half PPR format with 4 point TD
#'    }
#'  \item{\code{ppg_half_ppr_4pt_td}}{
#'    Fantasy points per game for half PPR format with 4 point TD
#'    }
#'  \item{\code{fpts_ppr_4pt_td}}{
#'    Fantasy points for full PPR format with 4 point TD
#'    }
#'  \item{\code{ppg_ppr_4pt_td}}{
#'    Fantasy points per game for full PPR format with 4 point TD
#'    }
#'  \item{\code{fpts_std_6pt_td}}{
#'    Fantasy points for standard format with 6 point TD
#'    }
#'  \item{\code{ppg_std_6pt_td}}{
#'    Fantasy points per game for standard format with 6 point TD
#'    }
#'  \item{\code{fpts_half_ppr_6pt_td}}{
#'    Fantasy points for half PPR format with 6 point TD
#'    }
#'  \item{\code{ppg_half_ppr_6pt_td}}{
#'    Fantasy points per game for half PPR format with 6 point TD
#'    }
#'  \item{\code{fpts_ppr_6pt_td}}{
#'    Fantasy points for full PPR format with 6 point TD
#'    }
#'  \item{\code{ppg_ppr_6pt_td}}{
#'    Fantasy points per game for full PPR format with 6 point TD
#'    }
#'  \item{\code{carries}}{
#'    Number of rush attempts including scrambles and kneel downs. Rushes after
#'    a lateral reception don't count as a carry
#'    }
#'  \item{\code{rushing_yards}}{
#'    Yards gained when rushing including scrambles and kneel downs. Also
#'    includes yards gained after obtaining a lateral on a play that started
#'    with a rushing attempt
#'    }
#'  \item{\code{rushing_tds}}{
#'    The number of rushing touchdowns (incl. scrambles). Also includes
#'    touchdowns after obtaining a lateral on a play that started with a
#'    rushing attempt
#'    }
#'  \item{\code{rushing_fumbles}}{Number of rushes with a fumble}
#'  \item{\code{rushing_fumbles_lost}}{Number of rushes with a lost fumble}
#'  \item{\code{rushing_first_downs}}{Number of rushing first downs}
#'  \item{\code{rushing_epa}}{
#'    Expected points added (EPA) on rush attempts including scrambles and
#'    kneel downs
#'    }
#'  \item{\code{rushing_2pt_conversions}}{Two-point conversion rushes}
#'  \item{\code{targets}}{
#'    Number of pass plays where the player was targeted as a receiver
#'    }
#'  \item{\code{receptions}}{
#'    Number of pass receptions. Lateral receptions don't count as a reception
#'    }
#'  \item{\code{receiving_yards}}{
#'    Yards gained after a pass reception. Includes yards gained after
#'    receiving a lateral on a play that started as a pass play
#'    }
#'  \item{\code{receiving_tds}}{
#'    Number of reception touchdowns, including after receiving a lateral on a
#'    play that began as a pass play
#'    }
#'  \item{\code{receiving_fumbles}}{Number of fumbles after a pass reception}
#'  \item{\code{receiving_fumbles_lost}}{
#'    Number of fumbles lost after a pass reception
#'    }
#'  \item{\code{receiving_air_yards}}{
#'    Receiving air yards including incomplete passes
#'    }
#'  \item{\code{receiving_yards_after_catch}}{
#'    Yards after the catch gained on plays in which player was receiver (this
#'    is an unofficial stat and may differ slightly between different sources)
#'    }
#'  \item{\code{receiving_first_downs}}{
#'    Number of first downs gained on a reception
#'    }
#'  \item{\code{receiving_epa}}{Expected points added on receptions}
#'  \item{\code{receiving_2pt_conversions}}{Two-point conversion receptions}
#'  \item{\code{racr}}{
#'    Receiving Air Conversion Ratio.
#'    RACR = `receiving_yards` / `receiving_air_yards`
#'    }
#'  \item{\code{target_share}}{
#'    Share of targets of player compared to all team targets
#'    }
#'  \item{\code{tgt_pct}}{Share of targets percentage}
#'  \item{\code{air_yards_share}}{
#'    Share of `receiving_air_yards` of the player to all team `air_yards`
#'    }
#'  \item{\code{wopr}}{
#'    Weighted Opportunity Rating.
#'    WOPR = 1.5 x `target_share` + 0.7 x `air_yards_share`
#'    }
#'  \item{\code{completions}}{Total pass completions (CMP)}
#'  \item{\code{attempts}}{Total pass attempts (ATT)}
#'  \item{\code{cmp_pct}}{Pass completion percentage}
#'  \item{\code{passing_yards}}{Total passing yards}
#'  \item{\code{passing_tds}}{Total passing touchdowns}
#'  \item{\code{interceptions}}{Total pass interceptions (INT)}
#'  \item{\code{sacks}}{Total number of sacks taken}
#'  \item{\code{sack_yards}}{Total yards taken from sacks}
#'  \item{\code{sack_fumbles}}{Total fumbles from sacks}
#'  \item{\code{sack_fumbles_lost}}{Total fumbles lost from sacks}
#'  \item{\code{passing_air_yards}}{
#'    Passing air yards (includes incomplete passes)
#'    }
#'  \item{\code{passing_yards_after_catch}}{
#'    Yards after the catch gained on plays in which player was the passer
#'    (this is an unofficial stat and may differ slightly between different
#'    sources).
#'    }
#'  \item{\code{passing_first_downs}}{First downs on pass attempts}
#'  \item{\code{passing_epa}}{
#'    Total expected points added (EPA) on pass attempts and sacks.
#'    NOTE: This uses the variable `qb_epa`, which gives QB credit for EPA for
#'    up to the point where a receiver lost a fumble after a completed catch
#'    and makes EPA work more like passing yards on plays with fumbles
#'    }
#'  \item{\code{passing_2pt_conversions}}{Two-point conversion passes}
#'  \item{\code{pacr}}{
#'    Passing Air Conversion Ratio. PACR = `passing_yards` / `passing_air_yards`
#'    }
#'  \item{\code{dakota}}{
#'    Adjusted EPA + CPOE composite based on coefficients which best predicts
#'    adjusted EPA/play in the following year
#'    }
#'    }
#'
#' @export
get_rb_pbp_stats <- function(pbp_db = NULL,
                             pbp_db_tbl = NULL,
                             season = NULL,
                             week_min = NULL,
                             week_max = NULL) {
  # Load play-by-play data with database or nflreadr
  pbp <- nuclearff::get_pbp_data(pbp_db, pbp_db_tbl, season, week_min, week_max)

  # Filter data for RB stats and compile
  rb_pbp <- pbp %>%
    nflfastR::calculate_player_stats() %>%
    # Calculate fantasy points for STD/Half PPR/Full PPR scoring
    nuclearff::calc_fpts_common_formats() %>%
    nuclearff::calc_fpts_ppg_common_formats() %>%
    # Calculations
    dplyr::mutate(
      # Completion percentage (CMP\%)
      cmp_pct = (completions / attempts) * 100,
      # Target share percentage (TGT\%)
      tgt_pct = target_share * 100
    ) %>%
    dplyr::inner_join(
      nflfastR::fast_scraper_roster(season) %>%
        # Filter by player position
        dplyr::filter(position == "RB") %>%
        dplyr::select(player_id = gsis_id),
      by = "player_id"
    ) %>%
    # Select columns to keep
    dplyr::select(player_id,
      player_display_name,
      player_name,
      position,
      team = recent_team,
      games,
      # Fantasy points for common scoring formats
      # Calculated from nuclearff functions
      fpts_std_4pt_td, ppg_std_4pt_td,
      fpts_half_ppr_4pt_td, ppg_half_ppr_4pt_td,
      fpts_ppr_4pt_td, ppg_ppr_4pt_td,
      fpts_std_6pt_td, ppg_std_6pt_td,
      fpts_half_ppr_6pt_td, ppg_half_ppr_6pt_td,
      fpts_ppr_6pt_td, ppg_ppr_6pt_td,
      # Play-by-Play Stats
      # Rushing
      carries,
      rushing_yards,
      rushing_tds,
      rushing_fumbles,
      rushing_fumbles_lost,
      rushing_first_downs,
      rushing_epa,
      rushing_2pt_conversions,
      # Receiving
      targets,
      receptions,
      receiving_yards,
      receiving_tds,
      receiving_fumbles,
      receiving_fumbles_lost,
      receiving_air_yards,
      receiving_yards_after_catch,
      receiving_first_downs,
      receiving_epa,
      receiving_2pt_conversions,
      racr,
      target_share,
      tgt_pct,
      air_yards_share,
      wopr,
      # Passing
      completions,
      attempts,
      cmp_pct,
      passing_yards,
      passing_tds,
      interceptions,
      sacks,
      sack_yards,
      sack_fumbles,
      sack_fumbles_lost,
      passing_air_yards,
      passing_yards_after_catch,
      passing_first_downs,
      passing_epa,
      passing_2pt_conversions,
      pacr,
      dakota
    )

  return(rb_pbp)
}

#' WR Stats from NFL Play-by-Play Data
#'
#' @description
#'  Obtain WR stats from NFL play-by-play data for a specified time frame
#'  from either a saved database or `nflreadr::load_pbp()`.
#'
#' @details
#'  The function `get_wr_pbp_stats` can be utilized to obtain player stats
#'  for a specified time frame that can include multiple seasons or even a
#'  range of weeks instead of the entire season.
#'  This includes stats for passing, rushing, and receiving obtained by using
#'  the `nflfastR` function `calculate_player_stats()`.
#'  The player stats are utilized to calculate fantasy points
#'  based on common scoring formats (4/6 point TD, STD, Half PPR, Full PPR).
#'  The function acquires stats by using cumulative play-by-play data
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
#'  Using the default naming scheme, `nuclearff::get_pbp_data` can be defined using the
#'  database with `pbp_db = "data/pbp_db"` and `pbp_db_tbl = "nflfastR_pbp"`.
#'  Note that these two arguments must be defined as strings.
#'  For more information on calculated player stats, refer to the
#'  [nflfastR `calculate_player_stats()`](
#'    https://www.nflfastr.com/reference/calculate_player_stats.html
#'    )
#'
#' @param pbp_dp Play-by-Play database path (optional)
#' @param pbp_db_tbl Play-by-Play database table name (optional)
#' @param season NFL season (required) to obtain play-by-play data. The
#'  season can be defined as a single season, `season = 2024`. For multiple
#'  seasons, use either `season = c(2023,2024)`
#' @param week_min Minimum week (required) to define whether pulling a range
#'  of weeks or the entire season. Use `week_min = 1` for the first week of
#'  the season, must be an integer.
#' @param week_max Maximum week (optional) to define a range of weeks to pull
#'  from an NFL season. If not defined, the data will be pulled for all weeks,
#'  beginning with `week_min`.
#'
#' @return Dataframe with WR stats for user-defined season(s) and week(s)
#'  obtained from NFL play-by-play data
#'
#' @seealso \code{\link[nuclearff]{get_pbp_data}},
#'  \code{\link[nflreadr]{load_pbp}},
#'  \code{\link[nflfastR]{update_db}}
#'
#' @author Nolan MacDonald
#'
#' \itemize{
#'  \item{\code{player_id}}{Player gsis id (e.g., 00-0038120)}
#'  \item{\code{player_display_name}}{Player name (e.g., Breece Hall)}
#'  \item{\code{player_name}}{Player shortened name (e.g., B.Hall)}
#'  \item{\code{position}}{Player position (e.g., RB)}
#'  \item{\code{team}}{Player team (e.g., NYJ)}
#'  \item{\code{games}}{Number of games played}
#'  \item{\code{fpts_std_4pt_td}}{
#'    Fantasy points for standard format with 4 point TD
#'    }
#'  \item{\code{ppg_std_4pt_td}}{
#'    Fantasy points per game for standard format with 4 point TD
#'    }
#'  \item{\code{fpts_half_ppr_4pt_td}}{
#'    Fantasy points for half PPR format with 4 point TD
#'    }
#'  \item{\code{ppg_half_ppr_4pt_td}}{
#'    Fantasy points per game for half PPR format with 4 point TD
#'    }
#'  \item{\code{fpts_ppr_4pt_td}}{
#'    Fantasy points for full PPR format with 4 point TD
#'    }
#'  \item{\code{ppg_ppr_4pt_td}}{
#'    Fantasy points per game for full PPR format with 4 point TD
#'    }
#'  \item{\code{fpts_std_6pt_td}}{
#'    Fantasy points for standard format with 6 point TD
#'    }
#'  \item{\code{ppg_std_6pt_td}}{
#'    Fantasy points per game for standard format with 6 point TD
#'    }
#'  \item{\code{fpts_half_ppr_6pt_td}}{
#'    Fantasy points for half PPR format with 6 point TD
#'    }
#'  \item{\code{ppg_half_ppr_6pt_td}}{
#'    Fantasy points per game for half PPR format with 6 point TD
#'    }
#'  \item{\code{fpts_ppr_6pt_td}}{
#'    Fantasy points for full PPR format with 6 point TD
#'    }
#'  \item{\code{ppg_ppr_6pt_td}}{
#'    Fantasy points per game for full PPR format with 6 point TD
#'    }
#'  \item{\code{targets}}{
#'    Number of pass plays where the player was targeted as a receiver
#'    }
#'  \item{\code{receptions}}{
#'    Number of pass receptions. Lateral receptions don't count as a reception
#'    }
#'  \item{\code{receiving_yards}}{
#'    Yards gained after a pass reception. Includes yards gained after
#'    receiving a lateral on a play that started as a pass play
#'    }
#'  \item{\code{receiving_tds}}{
#'    Number of reception touchdowns, including after receiving a lateral on a
#'    play that began as a pass play
#'    }
#'  \item{\code{receiving_fumbles}}{Number of fumbles after a pass reception}
#'  \item{\code{receiving_fumbles_lost}}{
#'    Number of fumbles lost after a pass reception
#'    }
#'  \item{\code{receiving_air_yards}}{
#'    Receiving air yards including incomplete passes
#'    }
#'  \item{\code{receiving_yards_after_catch}}{
#'    Yards after the catch gained on plays in which player was receiver (this
#'    is an unofficial stat and may differ slightly between different sources)
#'    }
#'  \item{\code{receiving_first_downs}}{
#'    Number of first downs gained on a reception
#'    }
#'  \item{\code{receiving_epa}}{Expected points added on receptions}
#'  \item{\code{receiving_2pt_conversions}}{Two-point conversion receptions}
#'  \item{\code{racr}}{
#'    Receiving Air Conversion Ratio.
#'    RACR = `receiving_yards` / `receiving_air_yards`
#'    }
#'  \item{\code{target_share}}{
#'    Share of targets of player compared to all team targets
#'    }
#'  \item{\code{tgt_pct}}{Share of targets percentage}
#'  \item{\code{air_yards_share}}{
#'    Share of `receiving_air_yards` of the player to all team `air_yards`
#'    }
#'  \item{\code{wopr}}{
#'    Weighted Opportunity Rating.
#'    WOPR = 1.5 x `target_share` + 0.7 x `air_yards_share`
#'    }
#'  \item{\code{carries}}{
#'    Number of rush attempts including scrambles and kneel downs. Rushes after
#'    a lateral reception don't count as a carry
#'    }
#'  \item{\code{rushing_yards}}{
#'    Yards gained when rushing including scrambles and kneel downs. Also
#'    includes yards gained after obtaining a lateral on a play that started
#'    with a rushing attempt
#'    }
#'  \item{\code{rushing_tds}}{
#'    The number of rushing touchdowns (incl. scrambles). Also includes
#'    touchdowns after obtaining a lateral on a play that started with a
#'    rushing attempt
#'    }
#'  \item{\code{rushing_fumbles}}{Number of rushes with a fumble}
#'  \item{\code{rushing_fumbles_lost}}{Number of rushes with a lost fumble}
#'  \item{\code{rushing_first_downs}}{Number of rushing first downs}
#'  \item{\code{rushing_epa}}{
#'    Expected points added (EPA) on rush attempts including scrambles and
#'    kneel downs
#'    }
#'  \item{\code{rushing_2pt_conversions}}{Two-point conversion rushes}
#'  \item{\code{completions}}{Total pass completions (CMP)}
#'  \item{\code{attempts}}{Total pass attempts (ATT)}
#'  \item{\code{cmp_pct}}{Pass completion percentage}
#'  \item{\code{passing_yards}}{Total passing yards}
#'  \item{\code{passing_tds}}{Total passing touchdowns}
#'  \item{\code{interceptions}}{Total pass interceptions (INT)}
#'  \item{\code{sacks}}{Total number of sacks taken}
#'  \item{\code{sack_yards}}{Total yards taken from sacks}
#'  \item{\code{sack_fumbles}}{Total fumbles from sacks}
#'  \item{\code{sack_fumbles_lost}}{Total fumbles lost from sacks}
#'  \item{\code{passing_air_yards}}{
#'    Passing air yards (includes incomplete passes)
#'    }
#'  \item{\code{passing_yards_after_catch}}{
#'    Yards after the catch gained on plays in which player was the passer
#'    (this is an unofficial stat and may differ slightly between different
#'    sources).
#'    }
#'  \item{\code{passing_first_downs}}{First downs on pass attempts}
#'  \item{\code{passing_epa}}{
#'    Total expected points added (EPA) on pass attempts and sacks.
#'    NOTE: This uses the variable `qb_epa`, which gives QB credit for EPA for
#'    up to the point where a receiver lost a fumble after a completed catch
#'    and makes EPA work more like passing yards on plays with fumbles
#'    }
#'  \item{\code{passing_2pt_conversions}}{Two-point conversion passes}
#'  \item{\code{pacr}}{
#'    Passing Air Conversion Ratio. PACR = `passing_yards` / `passing_air_yards`
#'    }
#'  \item{\code{dakota}}{
#'    Adjusted EPA + CPOE composite based on coefficients which best predicts
#'    adjusted EPA/play in the following year
#'    }
#'    }
#'
#' @export
get_wr_pbp_stats <- function(pbp_db = NULL,
                             pbp_db_tbl = NULL,
                             season = NULL,
                             week_min = NULL,
                             week_max = NULL) {
  # Load play-by-play data with database or nflreadr
  pbp <- nuclearff::get_pbp_data(pbp_db, pbp_db_tbl, season, week_min, week_max)

  # Filter data for RB stats and compile
  wr_pbp <- pbp %>%
    nflfastR::calculate_player_stats() %>%
    # Calculate fantasy points for STD/Half PPR/Full PPR scoring
    nuclearff::calc_fpts_common_formats() %>%
    nuclearff::calc_fpts_ppg_common_formats() %>%
    # Calculations
    dplyr::mutate(
      # Completion percentage (CMP\%)
      cmp_pct = (completions / attempts) * 100,
      # Target share percentage (TGT\%)
      tgt_pct = target_share * 100
    ) %>%
    dplyr::inner_join(
      nflfastR::fast_scraper_roster(season) %>%
        # Filter by player position
        dplyr::filter(position == "WR") %>%
        dplyr::select(player_id = gsis_id),
      by = "player_id"
    ) %>%
    # Select columns to keep
    dplyr::select(player_id,
      player_display_name,
      player_name,
      position,
      team = recent_team,
      games,
      # Fantasy points for common scoring formats
      # Calculated from nuclearff functions
      fpts_std_4pt_td, ppg_std_4pt_td,
      fpts_half_ppr_4pt_td, ppg_half_ppr_4pt_td,
      fpts_ppr_4pt_td, ppg_ppr_4pt_td,
      fpts_std_6pt_td, ppg_std_6pt_td,
      fpts_half_ppr_6pt_td, ppg_half_ppr_6pt_td,
      fpts_ppr_6pt_td, ppg_ppr_6pt_td,
      # Play-by-Play Stats
      # Receiving
      targets,
      receptions,
      receiving_yards,
      receiving_tds,
      receiving_fumbles,
      receiving_fumbles_lost,
      receiving_air_yards,
      receiving_yards_after_catch,
      receiving_first_downs,
      receiving_epa,
      receiving_2pt_conversions,
      racr,
      target_share,
      tgt_pct,
      air_yards_share,
      wopr,
      # Rushing
      carries,
      rushing_yards,
      rushing_tds,
      rushing_fumbles,
      rushing_fumbles_lost,
      rushing_first_downs,
      rushing_epa,
      rushing_2pt_conversions,
      # Passing
      completions,
      attempts,
      cmp_pct,
      passing_yards,
      passing_tds,
      interceptions,
      sacks,
      sack_yards,
      sack_fumbles,
      sack_fumbles_lost,
      passing_air_yards,
      passing_yards_after_catch,
      passing_first_downs,
      passing_epa,
      passing_2pt_conversions,
      pacr,
      dakota
    )

  return(wr_pbp)
}

#' TE Stats from NFL Play-by-Play Data
#'
#' @description
#' Obtain TE stats from NFL play-by-play data for a specified time frame
#' from either a saved database or `nflreadr::load_pbp()`.
#'
#' @details
#'  The function `get_te_pbp_stats` can be utilized to obtain player stats
#'  for a specified time frame that can include multiple seasons or even a
#'  range of weeks instead of the entire season.
#'  This includes stats for passing, rushing, and receiving obtained by using
#'  the `nflfastR` function `calculate_player_stats()`.
#'  The player stats are utilized to calculate fantasy points
#'  based on common scoring formats (4/6 point TD, STD, Half PPR, Full PPR).
#'  The function acquires stats by using cumulative play-by-play data
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
#'  Using the default naming scheme, `nuclearff::get_pbp_data` can be defined using the
#'  database with `pbp_db = "data/pbp_db"` and `pbp_db_tbl = "nflfastR_pbp"`.
#'  Note that these two arguments must be defined as strings.
#'  For more information on calculated player stats, refer to the
#'  [nflfastR `calculate_player_stats()`](
#'    https://www.nflfastr.com/reference/calculate_player_stats.html
#'    )
#'
#' @seealso \code{\link[nuclearff]{nuclearff::get_pbp_data}}:
#'  Obtain play-by-play data for a specified time frame from either a saved
#'  database or if not defined, using `nflreadr::load_pbp()`,
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
#' @return Dataframe with TE stats for user-defined season(s) and week(s)
#'  obtained from NFL play-by-play data
#'
#' @author Nolan MacDonald
#'
#' \itemize{
#'  \item{\code{player_id}}{Player gsis id (e.g., 00-0038120)}
#'  \item{\code{player_display_name}}{Player name (e.g., Breece Hall)}
#'  \item{\code{player_name}}{Player shortened name (e.g., B.Hall)}
#'  \item{\code{position}}{Player position (e.g., RB)}
#'  \item{\code{team}}{Player team (e.g., NYJ)}
#'  \item{\code{games}}{Number of games played}
#'  \item{\code{fpts_std_4pt_td}}{
#'    Fantasy points for standard format with 4 point TD
#'    }
#'  \item{\code{ppg_std_4pt_td}}{
#'    Fantasy points per game for standard format with 4 point TD
#'    }
#'  \item{\code{fpts_half_ppr_4pt_td}}{
#'    Fantasy points for half PPR format with 4 point TD
#'    }
#'  \item{\code{ppg_half_ppr_4pt_td}}{
#'    Fantasy points per game for half PPR format with 4 point TD
#'    }
#'  \item{\code{fpts_ppr_4pt_td}}{
#'    Fantasy points for full PPR format with 4 point TD
#'    }
#'  \item{\code{ppg_ppr_4pt_td}}{
#'    Fantasy points per game for full PPR format with 4 point TD
#'    }
#'  \item{\code{fpts_std_6pt_td}}{
#'    Fantasy points for standard format with 6 point TD
#'    }
#'  \item{\code{ppg_std_6pt_td}}{
#'    Fantasy points per game for standard format with 6 point TD
#'    }
#'  \item{\code{fpts_half_ppr_6pt_td}}{
#'    Fantasy points for half PPR format with 6 point TD
#'    }
#'  \item{\code{ppg_half_ppr_6pt_td}}{
#'    Fantasy points per game for half PPR format with 6 point TD
#'    }
#'  \item{\code{fpts_ppr_6pt_td}}{
#'    Fantasy points for full PPR format with 6 point TD
#'    }
#'  \item{\code{ppg_ppr_6pt_td}}{
#'    Fantasy points per game for full PPR format with 6 point TD
#'    }
#'  \item{\code{targets}}{
#'    Number of pass plays where the player was targeted as a receiver
#'    }
#'  \item{\code{receptions}}{
#'    Number of pass receptions. Lateral receptions don't count as a reception
#'    }
#'  \item{\code{receiving_yards}}{
#'    Yards gained after a pass reception. Includes yards gained after
#'    receiving a lateral on a play that started as a pass play
#'    }
#'  \item{\code{receiving_tds}}{
#'    Number of reception touchdowns, including after receiving a lateral on a
#'    play that began as a pass play
#'    }
#'  \item{\code{receiving_fumbles}}{Number of fumbles after a pass reception}
#'  \item{\code{receiving_fumbles_lost}}{
#'    Number of fumbles lost after a pass reception
#'    }
#'  \item{\code{receiving_air_yards}}{
#'    Receiving air yards including incomplete passes
#'    }
#'  \item{\code{receiving_yards_after_catch}}{
#'    Yards after the catch gained on plays in which player was receiver (this
#'    is an unofficial stat and may differ slightly between different sources)
#'    }
#'  \item{\code{receiving_first_downs}}{
#'    Number of first downs gained on a reception
#'    }
#'  \item{\code{receiving_epa}}{Expected points added on receptions}
#'  \item{\code{receiving_2pt_conversions}}{Two-point conversion receptions}
#'  \item{\code{racr}}{
#'    Receiving Air Conversion Ratio.
#'    RACR = `receiving_yards` / `receiving_air_yards`
#'    }
#'  \item{\code{target_share}}{
#'    Share of targets of player compared to all team targets
#'    }
#'  \item{\code{tgt_pct}}{Share of targets percentage}
#'  \item{\code{air_yards_share}}{
#'    Share of `receiving_air_yards` of the player to all team `air_yards`
#'    }
#'  \item{\code{wopr}}{
#'    Weighted Opportunity Rating.
#'    WOPR = 1.5 x `target_share` + 0.7 x `air_yards_share`
#'    }
#'  \item{\code{carries}}{
#'    Number of rush attempts including scrambles and kneel downs. Rushes after
#'    a lateral reception don't count as a carry
#'    }
#'  \item{\code{rushing_yards}}{
#'    Yards gained when rushing including scrambles and kneel downs. Also
#'    includes yards gained after obtaining a lateral on a play that started
#'    with a rushing attempt
#'    }
#'  \item{\code{rushing_tds}}{
#'    The number of rushing touchdowns (incl. scrambles). Also includes
#'    touchdowns after obtaining a lateral on a play that started with a
#'    rushing attempt
#'    }
#'  \item{\code{rushing_fumbles}}{Number of rushes with a fumble}
#'  \item{\code{rushing_fumbles_lost}}{Number of rushes with a lost fumble}
#'  \item{\code{rushing_first_downs}}{Number of rushing first downs}
#'  \item{\code{rushing_epa}}{
#'    Expected points added (EPA) on rush attempts including scrambles and
#'    kneel downs
#'    }
#'  \item{\code{rushing_2pt_conversions}}{Two-point conversion rushes}
#'  \item{\code{completions}}{Total pass completions (CMP)}
#'  \item{\code{attempts}}{Total pass attempts (ATT)}
#'  \item{\code{cmp_pct}}{Pass completion percentage}
#'  \item{\code{passing_yards}}{Total passing yards}
#'  \item{\code{passing_tds}}{Total passing touchdowns}
#'  \item{\code{interceptions}}{Total pass interceptions (INT)}
#'  \item{\code{sacks}}{Total number of sacks taken}
#'  \item{\code{sack_yards}}{Total yards taken from sacks}
#'  \item{\code{sack_fumbles}}{Total fumbles from sacks}
#'  \item{\code{sack_fumbles_lost}}{Total fumbles lost from sacks}
#'  \item{\code{passing_air_yards}}{
#'    Passing air yards (includes incomplete passes)
#'    }
#'  \item{\code{passing_yards_after_catch}}{
#'    Yards after the catch gained on plays in which player was the passer
#'    (this is an unofficial stat and may differ slightly between different
#'    sources).
#'    }
#'  \item{\code{passing_first_downs}}{First downs on pass attempts}
#'  \item{\code{passing_epa}}{
#'    Total expected points added (EPA) on pass attempts and sacks.
#'    NOTE: This uses the variable `qb_epa`, which gives QB credit for EPA for
#'    up to the point where a receiver lost a fumble after a completed catch
#'    and makes EPA work more like passing yards on plays with fumbles
#'    }
#'  \item{\code{passing_2pt_conversions}}{Two-point conversion passes}
#'  \item{\code{pacr}}{
#'    Passing Air Conversion Ratio. PACR = `passing_yards` / `passing_air_yards`
#'    }
#'  \item{\code{dakota}}{
#'    Adjusted EPA + CPOE composite based on coefficients which best predicts
#'    adjusted EPA/play in the following year
#'    }
#'    }
#'
#' @export
get_te_pbp_stats <- function(pbp_db = NULL,
                             pbp_db_tbl = NULL,
                             season = NULL,
                             week_min = NULL,
                             week_max = NULL) {
  # Load play-by-play data with database or nflreadr
  pbp <- nuclearff::get_pbp_data(pbp_db, pbp_db_tbl, season, week_min, week_max)

  # Filter data for RB stats and compile
  te_pbp <- pbp %>%
    nflfastR::calculate_player_stats() %>%
    # Calculate fantasy points for STD/Half PPR/Full PPR scoring
    nuclearff::calc_fpts_common_formats() %>%
    nuclearff::calc_fpts_ppg_common_formats() %>%
    # Calculations
    dplyr::mutate(
      # Completion percentage (CMP\%)
      cmp_pct = (completions / attempts) * 100,
      # Target share percentage (TGT\%)
      tgt_pct = target_share * 100
    ) %>%
    dplyr::inner_join(
      nflfastR::fast_scraper_roster(season) %>%
        # Filter by player position
        dplyr::filter(position == "TE") %>%
        dplyr::select(player_id = gsis_id),
      by = "player_id"
    ) %>%
    # Select columns to keep
    dplyr::select(player_id,
      player_display_name,
      player_name,
      position,
      team = recent_team,
      games,
      # Fantasy points for common scoring formats
      # Calculated from nuclearff functions
      fpts_std_4pt_td, ppg_std_4pt_td,
      fpts_half_ppr_4pt_td, ppg_half_ppr_4pt_td,
      fpts_ppr_4pt_td, ppg_ppr_4pt_td,
      fpts_std_6pt_td, ppg_std_6pt_td,
      fpts_half_ppr_6pt_td, ppg_half_ppr_6pt_td,
      fpts_ppr_6pt_td, ppg_ppr_6pt_td,
      # Play-by-Play Stats
      # Receiving
      targets,
      receptions,
      receiving_yards,
      receiving_tds,
      receiving_fumbles,
      receiving_fumbles_lost,
      receiving_air_yards,
      receiving_yards_after_catch,
      receiving_first_downs,
      receiving_epa,
      receiving_2pt_conversions,
      racr,
      target_share,
      tgt_pct,
      air_yards_share,
      wopr,
      # Rushing
      carries,
      rushing_yards,
      rushing_tds,
      rushing_fumbles,
      rushing_fumbles_lost,
      rushing_first_downs,
      rushing_epa,
      rushing_2pt_conversions,
      # Passing
      completions,
      attempts,
      cmp_pct,
      passing_yards,
      passing_tds,
      interceptions,
      sacks,
      sack_yards,
      sack_fumbles,
      sack_fumbles_lost,
      passing_air_yards,
      passing_yards_after_catch,
      passing_first_downs,
      passing_epa,
      passing_2pt_conversions,
      pacr,
      dakota
    )

  return(te_pbp)
}
