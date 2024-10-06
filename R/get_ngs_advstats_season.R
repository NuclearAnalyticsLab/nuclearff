################################################################################
# Author: Nolan MacDonald
# Purpose:
#   Obtain Positional Cumulative Season Advanced Statistics from NFL
#   Next Gen Stats (NGS).
# Code Style Guide:
#   styler::tidyverse_style(), lintr::use_lintr(type = "tidyverse")
################################################################################

#' QB Cumulative Season NFL Next Gen Stats (NGS)
#'
#' @description
#' Obtain QB cumulative season stats from `nflreadr`,
#' to acquire Next Gen Stats (NGS) data.
#'
#' @details
#' `get_qb_ngs_advstats_season` is a function to obtain NFL Next Gen
#' Stats (NGS) for an entire season or multiple seasons.
#' The function utilizes the `nflreadr` function `load_nextgen_stats` to load
#' player level weekly stats starting with the 2016 season.
#' For quarterbacks, the `stat_type` is defined as `passing` and is updated
#' every night.
#' NGS will only provide data for players above a minimum number of receptions.
#' More information is available about NGS stats from
#' [NGS glossary](https://nextgenstats.nfl.com/glossary).
#'
#' @param seasons NFL Season(s) where `seasons = 2024` or
#'  `seasons = c(2023, 2024)`
#'
#' @return Dataframe with QB Next Gen Stats for a defined season
#'
#' @seealso \code{\link[nflreadr]{load_nextgen_stats}}
#'  Load player level weekly NFL Next Gen Stats
#'
#' @author Nolan MacDonald
#'
#' \itemize{
#'  \item{\code{player_id}}{Player gsis id (e.g., 00-0034796)}
#'  \item{\code{player_display_name}}{Player name (e.g., Lamar Jackson)}
#'  \item{\code{player_name}}{Player shortened name (e.g., L.Jackson)}
#'  \item{\code{position}}{Player position}
#'  \item{\code{pass_yards}}{Total pass yards}
#'  \item{\code{pass_touchdowns}}{Total pass touchdowns}
#'  \item{\code{interceptions}}{Total interceptions}
#'  \item{\code{passer_rating}}{Average passer rating}
#'  \item{\code{attempts}}{Total pass attempts}
#'  \item{\code{completions}}{Total pass completions}
#'  \item{\code{completion_percentage}}{Average completion percentage (%)}
#'  \item{\code{expected_completion_percentage}}{}
#'  \item{\code{completion_percentage_above_expectation}}{
#'      Average expected completion percentage (xCOMP)
#'      }
#'  \item{\code{avg_air_distance}}{Average air distance}
#'  \item{\code{max_air_distance}}{Maximum or longest air distance}
#'  \item{\code{avg_time_to_throw}}{Average time to throw (TT)}
#'  \item{\code{avg_completed_air_yards}}{Average completed air yards (CAY)}
#'  \item{\code{avg_intended_air_yards}}{Average intended air yards (IAY)}
#'  \item{\code{avg_air_yards_differential}}{
#'      Average air yards differential (AYD)
#'      }
#'  \item{\code{aggressiveness}}{Average aggressiveness (AGG%)}
#'  \item{\code{max_completed_air_distance}}{
#'      Maximum or longest completed air distance (LCAD)}
#'  \item{\code{avg_air_yards_to_sticks}}{
#'      Average air yards to the sticks (AYTS)
#'      }
#'      }
#'
#' @export
get_qb_ngs_advstats_season <- function(seasons = NULL) {
  # NFL Next Gen Stats (NGS) Advanced Stats
  # https://nextgenstats.nfl.com/glossary

  # Check if season is provided as int (e.g., 2024 or c(2023, 2024))
  # Ensure seasons is using 2016 or later for NGS advanced stats
  nuclearff::validate_ngs_season(seasons)

  qb_ngs <- nflreadr::load_nextgen_stats(
    seasons,
    stat_type = "passing"
  ) %>%
    # Filter by RB Position
    # Define week as 0 for the cumulative season stats
    dplyr::filter(week == 0,
                  player_position == "QB") %>%
    # Columns to keep - names match nflfastR pbp
    dplyr::group_by(
      player_id = player_gsis_id, # e.g., 00-0034796
      player_display_name = player_display_name, # e.g., Lamar Jackson
      player_name = player_short_name, # e.g., L.Jackson
      position = player_position, # e.g., QB
      team = team_abbr, # e.g., BAL
      # Total pass yards
      pass_yards,
      # Total pass TD
      pass_touchdowns,
      # Total INT
      interceptions,
      # Passer rating
      passer_rating,
      # Total pass attempts
      attempts,
      # Total completions
      completions,
      # Average completion percentage
      completion_percentage,
      # Average expected completion percentage (xCOMP)
      expected_completion_percentage,
      # Average completion percentage above expectation (CPOE)
      completion_percentage_above_expectation,
      # Average air distance
      avg_air_distance,
      # Maximum or longest air distance
      max_air_distance,
      # Average time to throw (TT)
      avg_time_to_throw,
      # Average completed air yards (CAY)
      avg_completed_air_yards,
      # Average intended air yards (IAY)
      avg_intended_air_yards,
      # Average air yards differential (AYD)
      avg_air_yards_differential,
      # Average aggressiveness (AGG%)
      aggressiveness,
      # Maximum or longest completed air distance (LCAD)
      max_completed_air_distance,
      # Average air yards to the sticks (AYTS)
      avg_air_yards_to_sticks

    )

  return(qb_ngs)
}

#' RB Cumulative Season NFL Next Gen Stats (NGS)
#'
#' @description
#' Obtain RB cumulative season stats from `nflreadr`,
#' to acquire Next Gen Stats (NGS) data.
#'
#' @details
#' `get_rb_ngs_advstats_season` is a function to obtain NFL Next Gen
#' Stats (NGS) for an entire season or multiple seasons.
#' The function utilizes the `nflreadr` function `load_nextgen_stats` to load
#' player level weekly stats starting with the 2016 season.
#' For running backs, the `stat_type` is defined as `rushing` and is updated
#' every night.
#' NGS will only provide data for players above a minimum number of receptions.
#' More information is available about NGS stats from
#' [NGS glossary](https://nextgenstats.nfl.com/glossary).
#'
#' @param seasons NFL Season(s) where `seasons = 2024` or
#'  `seasons = c(2023, 2024)`
#'
#' @return Dataframe with RB Next Gen Stats for a defined season
#'
#' @seealso \code{\link[nflreadr]{load_nextgen_stats}}
#'  Load player level weekly NFL Next Gen Stats
#'
#' @author Nolan MacDonald
#'
#' \itemize{
#'  \item{\code{player_id}}{Player gsis id (e.g., 00-0038120)}
#'  \item{\code{player_display_name}}{Player name (e.g., Breece Hall)}
#'  \item{\code{player_name}}{Player shortened name (e.g., B.Hall)}
#'  \item{\code{position}}{Player position}
#'  \item{\code{rush_attempts}}{Total rush attempts}
#'  \item{\code{rush_yards}}{Total rush yards}
#'  \item{\code{avg_rush_yards}}{Average rush yards per attempt}
#'  \item{\code{rush_touchdowns}}{Total rush touchdowns}
#'  \item{\code{efficiency}}{Average efficiency (EFF)}
#'  \item{\code{percent_attempts_gte_eight_defenders }}{
#'      Average % attempts with 8+ Defenders in the Box (8+D%)
#'      }
#'  \item{\code{avg_time_to_los}}{Average time behind line of scrimmage (TLOS)}
#'  \item{\code{expected_rush_yards}}{Average expected rush yards}
#'  \item{\code{rush_yards_over_expected}}{Average rush yards over expected}
#'  \item{\code{rush_yards_over_expected_per_att}}{
#'      Average rush yards over expected per attempt
#'      }
#'  \item{\code{rush_pct_over_expected}}{Average rush % over expected}
#'  }
#'
#' @export
get_rb_ngs_advstats_season <- function(seasons = NULL) {
  # NFL Next Gen Stats (NGS) Advanced Stats
  # https://nextgenstats.nfl.com/glossary

  # Check if season is provided as int (e.g., 2024 or c(2023, 2024))
  # Ensure seasons is using 2016 or later for NGS advanced stats
  nuclearff::validate_ngs_season(seasons)

  rb_ngs <- nflreadr::load_nextgen_stats(
    seasons,
    stat_type = "rushing"
  ) %>%
    # Filter by RB Position
    # Define week as 0 for the cumulative season stats
    dplyr::filter(week == 0,
                  player_position == "RB"
    ) %>%
    # Columns to keep - names match nflfastR pbp
    dplyr::select(
      player_id = player_gsis_id, # e.g., 00-0038120
      player_display_name,        # e.g., Breece Hall
      player_name = player_short_name, # e.g., B.Hall
      team = team_abbr, # e.g., NYJ
      rush_attempts, # Total rush attempts
      rush_yards, # Total rush yards
      avg_rush_yards, # Avg. rush yards per attempt
      rush_touchdowns, # Total rush TD
      efficiency, # Avg. efficiency (EFF)
      # Average % attempts with 8+ Defenders in the Box (8+D%)
      percent_attempts_gte_eight_defenders,
      # Average time behind line of scrimmage (TLOS)
      avg_time_to_los,
      # Average expected rush yards
      expected_rush_yards,
      # Average rush yards over expected
      rush_yards_over_expected,
      # Average rush yards over expected per attempt
      rush_yards_over_expected_per_att,
      # Average rush % over expected
      rush_pct_over_expected
    )

  return(rb_ngs)
}

#' WR Cumulative Season NFL Next Gen Stats (NGS)
#'
#' @description
#' Obtain WR cumulative season stats from `nflreadr`,
#' to acquire Next Gen Stats (NGS) data.
#'
#' @details
#' `get_wr_ngs_advstats_season` is a function to obtain NFL Next Gen
#' Stats (NGS) for an entire season or multiple seasons.
#' The function utilizes the `nflreadr` function `load_nextgen_stats` to load
#' player level weekly stats starting with the 2016 season.
#' For receivers, the `stat_type` is defined as `receiving` and is updated
#' every night.
#' NGS will only provide data for players above a minimum number of receptions.
#' More information is available about NGS stats from
#' [NGS glossary](https://nextgenstats.nfl.com/glossary).
#'
#' @param seasons NFL Season(s) where `seasons = 2024` or
#'  `seasons = c(2023, 2024)`
#'
#' @return Dataframe with WR Next Gen Stats for a defined season
#'
#' @seealso \code{\link[nflreadr]{load_nextgen_stats}}
#'  Load player level weekly NFL Next Gen Stats
#'
#' @author Nolan MacDonald
#'
#' \itemize{
#'  \item{\code{player_id}}{Player gsis id (e.g., 00-0039337)}
#'  \item{\code{player_display_name}}{Player name (e.g., Malik Nabers)}
#'  \item{\code{player_name}}{Player shortened name (e.g., M.Nabers)}
#'  \item{\code{position}}{Player position}
#'  \item{\code{targets}}{Total receiving targets}
#'  \item{\code{receptions}}{Total receptions}
#'  \item{\code{yards}}{Total receiving yards}
#'  \item{\code{rec_touchdowns}}{Total reception touchdowns}
#'  \item{\code{avg_cushion}}{Average cushion (CUSH)}
#'  \item{\code{avg_separation}}{Average separation (SEP)}
#'  \item{\code{avg_intended_air_yards}}{Average targeted air yards (TAY)}
#'  \item{\code{avg_percent_share_of_intended_air_yards}}{
#'      Average % share of team's air yards (TAY%)
#'      }
#'  \item{\code{avg_catch_percentage}}{Average catch percentage}
#'  \item{\code{avg_yac}}{Average yards after catch (YAC)}
#'  \item{\code{avg_expected_yac}}{Average expected yards after catch (xYAC)}
#'  \item{\code{avg_yac_above_expectation}}{
#'      Average yards after catch above expectation (+/-)
#'      }
#'      }
#'
#' @export
get_wr_ngs_advstats_season <- function(seasons = NULL) {
  # NFL Next Gen Stats (NGS) Advanced Stats
  # https://nextgenstats.nfl.com/glossary

  # Check if season is provided as int (e.g., 2024 or c(2023, 2024))
  # Ensure seasons is using 2016 or later for NGS advanced stats
  nuclearff::validate_ngs_season(seasons)

  wr_ngs <- nflreadr::load_nextgen_stats(
    seasons,
    stat_type = "receiving"
  ) %>%
    # Filter by WR Position
    # Define week as 0 for the cumulative season stats
    dplyr::filter(week == 0,
                  player_position == "WR"
                  ) %>%
    # Columns to keep - names match nflfastR pbp
    dplyr::select(
      player_id = player_gsis_id,
      player_display_name,
      player_name = player_short_name,
      team = team_abbr,
      targets, # Total targets
      receptions, # Total receptions
      yards, # Total reception yards
      rec_touchdowns, # Total reception touchdowns
      catch_percentage, # Avg catch percentage
      avg_cushion, # Avg. cushion (CUSH)
      avg_separation, # Avg. separation (SEP)
      avg_intended_air_yards, # Avg. targeted air yards (TAY)
      # avg % share of team air yards (TAY%)
      percent_share_of_intended_air_yards,
      avg_yac, # Avg. yards after catch (YAC)
      avg_expected_yac, # Avg. expected yards after catch (xYAC)
      # Avg. yards after catch above expectation (+/-)
      avg_yac_above_expectation
    )
  return(wr_ngs)
}

#' TE Cumulative Season NFL Next Gen Stats (NGS)
#'
#' @description
#' Obtain WR cumulative season stats from `nflreadr`,
#' to acquire Next Gen Stats (NGS) data.
#'
#' @details
#' `get_te_ngs_advstats_season` is a function to obtain NFL Next Gen
#' Stats (NGS) for an entire season or multiple seasons.
#' The function utilizes the `nflreadr` function `load_nextgen_stats` to load
#' player level weekly stats starting with the 2016 season.
#' For receivers, the `stat_type` is defined as `receiving` and is updated
#' every night.
#' NGS will only provide data for players above a minimum number of receptions.
#' More information is available about NGS stats from
#' [NGS glossary](https://nextgenstats.nfl.com/glossary).
#'
#' @param seasons NFL Season(s) where `seasons = 2024` or
#'  `seasons = c(2023, 2024)`
#'
#' @return Dataframe with TE Next Gen Stats for a defined season
#'
#' @seealso \code{\link[nflreadr]{load_nextgen_stats}}
#'  Load player level weekly NFL Next Gen Stats
#'
#' @author Nolan MacDonald
#'
#' \itemize{
#'  \item{\code{player_id}}{Player gsis id (e.g., 00-0037744)}
#'  \item{\code{player_display_name}}{Player name (e.g., Trey McBride)}
#'  \item{\code{player_name}}{Player shortened name (e.g., T.McBride)}
#'  \item{\code{position}}{Player position}
#'  \item{\code{targets}}{Total receiving targets}
#'  \item{\code{receptions}}{Total receptions}
#'  \item{\code{yards}}{Total receiving yards}
#'  \item{\code{rec_touchdowns}}{Total reception touchdowns}
#'  \item{\code{avg_cushion}}{Average cushion (CUSH)}
#'  \item{\code{avg_separation}}{Average separation (SEP)}
#'  \item{\code{avg_intended_air_yards}}{Average targeted air yards (TAY)}
#'  \item{\code{avg_percent_share_of_intended_air_yards}}{
#'      Average % share of team's air yards (TAY%)
#'      }
#'  \item{\code{avg_catch_percentage}}{Average catch percentage}
#'  \item{\code{avg_yac}}{Average yards after catch (YAC)}
#'  \item{\code{avg_expected_yac}}{Average expected yards after catch (xYAC)}
#'  \item{\code{avg_yac_above_expectation}}{
#'      Average yards after catch above expectation (+/-)
#'      }
#'      }
#'
#' @export
get_te_ngs_advstats_season <- function(seasons = NULL) {
  # NFL Next Gen Stats (NGS) Advanced Stats
  # https://nextgenstats.nfl.com/glossary

  # Check if season is provided as int (e.g., 2024 or c(2023, 2024))
  # Ensure seasons is using 2016 or later for NGS advanced stats
  nuclearff::validate_ngs_season(seasons)

  te_ngs <- nflreadr::load_nextgen_stats(
    seasons,
    stat_type = "receiving"
  ) %>%
    # Filter by TE Position
    # Define week as 0 for the cumulative season stats
    dplyr::filter(week == 0,
                  player_position == "TE"
    ) %>%
    # Columns to keep - names match nflfastR pbp
    dplyr::select(
      player_id = player_gsis_id,
      player_display_name,
      player_name = player_short_name,
      team = team_abbr,
      targets, # Total targets
      receptions, # Total receptions
      yards, # Total reception yards
      rec_touchdowns, # Total reception touchdowns
      catch_percentage, # Avg catch percentage
      avg_cushion, # Avg. cushion (CUSH)
      avg_separation, # Avg. separation (SEP)
      avg_intended_air_yards, # Avg. targeted air yards (TAY)
      # avg % share of team air yards (TAY%)
      percent_share_of_intended_air_yards,
      avg_yac, # Avg. yards after catch (YAC)
      avg_expected_yac, # Avg. expected yards after catch (xYAC)
      # Avg. yards after catch above expectation (+/-)
      avg_yac_above_expectation
    )

  return(te_ngs)
}
