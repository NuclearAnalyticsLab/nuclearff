################################################################################
# Author: Nolan MacDonald
# Purpose: Obtain Positional Cumulative Season Advanced Stats
# Code Style Guide: styler::tidyverse_style()
################################################################################

#' \strong{RB Cumulative Season Advanced Stats}
#'
#' Obtain RB cumulative season stats from `nflfastR`, and using `nflreadr`
#' to acquire Pro Football Reference (PFR) and Next Gen Stats (NGS) data
#'
#' @details
#' This is a function to obtain NFL advanced statistics for an entire
#' season. The function utilizes `nflfastR` for play-by-play data to determine
#' cumulative season statistics. The `nflreadr` package is also used to obtain
#' advanced statistics from Pro Football Reference (PFR) and
#' NFL Next Gen Stats (NGS). To use the function, you will need to save
#' play-by-play data to a database using the `nflfastR` function, `update_db()`.
#' For example, the database is saved by default as `pbp_db` with a table that
#' is stored containing all play-by-play information called `nflfastR_pbp`.
#' By defining the argument `pbp_db` (str) the user is specifying the path to the
#' database as well as the name it was saved as. To load the table inside the
#' database, `pbp_db_tbl` (str) can be defined if the naming is different,
#' however, the default argument follows the naming scheme provided by the
#' `nflfastR` package. The final requirement is the argument `season` (int)
#' to indicate the NFL season that the user wants to obtain advanced
#' statistics for.
#' More information is available on `update_db()` in the
#' [nflfastR documentation](https://www.nflfastr.com/reference/update_db.html).
#'
#' @seealso \code{\link{get_snap_pct}}: Documentation on obtaining snap
#' share used in this function
#' @seealso \code{\link[nflfastR]{update_db}}: Create or update play-by-play
#' database
#' @seealso {\link[nflreadr]{load_pfr_advstats}}: PFR advanced statistics
#' @seealso {\link[nflreadr]{load_nextgen_stats}}: NFL Next Gen Stats
#'
#' @param pbp_db Path to play-by-play database and database name
#' @param pbp_db_tbl Play-by-play table name
#' @param season NFL Season
#'
#' @return Dataframe with RB advanced statistics for a defined season
#'
#' \describe{
#'   \item{\code{player}}{Player name}
#'   \item{\code{team}}{Player team in defined season}
#'   \item{\code{games}}{Total games played in defined season}
#'   \item{\code{fantasy_points}}{Total fantasy points in a standard format (0 points per reception)}
#'   \item{\code{fantasy_points_ppr}}{Total fantasy points in a full PPR format (1 point per reception)}
#'   \item{\code{ppg}}{Average fantasy points per game}
#' }
#'
#' @author Nolan MacDonald
#'
#' @export
get_rb_adv_stats_season <- function(pbp_db = "./data/pbp_db/pbp_db",
                                    pbp_db_tbl = "nflfastR_pbp",
                                    season = 2024) {
  # Connect to the database ----------------------------------------------------
  connect <- DBI::dbConnect(RSQLite::SQLite(), pbp_db)

  # Load the play-by-play database table ---------------------------------------
  pbp_db <- dplyr::tbl(connect, pbp_db_tbl)

  # Play-by-play data for the specified season ---------------------------------
  pbp <- pbp_db %>%
    dplyr::filter(season == !!season) %>%
    dplyr::collect()

  # Filter data for RB stats and compile ---------------------------------------
  rb_pbp <- pbp %>%
    dplyr::filter(week >= 1) %>%
    nflfastR::calculate_player_stats() %>%
    dplyr::mutate(
      ppg_std = fantasy_points / games,
      ppg_ppr = fantasy_points_ppr / games
    ) %>%
    dplyr::inner_join(
      nflfastR::fast_scraper_roster(season) %>%
        dplyr::filter(position == "RB") %>%
        dplyr::select(player_id = gsis_id),
      by = "player_id"
    ) %>%
    dplyr::arrange(-fantasy_points_ppr) %>%
    dplyr::select(
      player_id, player_name,
      player = player_display_name, position,
      team = recent_team, games,
      tot_fpts = fantasy_points,
      tot_fpts_ppr = fantasy_points_ppr,
      ppg_ppr,
      carries, rushing_yards, rushing_tds,
      rushing_fumbles, rushing_fumbles_lost, rushing_first_downs, rushing_epa, rushing_2pt_conversions,
      receptions, targets, receiving_yards, receiving_tds, receiving_fumbles, receiving_fumbles_lost,
      receiving_air_yards, receiving_yards_after_catch, receiving_first_downs, receiving_epa,
      receiving_2pt_conversions, racr, target_share, air_yards_share, wopr
    )

  # Add snap percentage
  snap_pct <- nuclearff::get_snap_pct(szn = season, pos = "RB")
  rb_snap <- dplyr::left_join(rb_pbp, snap_pct, by = c("player_id", "player", "position", "team"))

  # Pro Football Reference Advanced Stats
  rb_pfr <- nflreadr::load_pfr_advstats(seasons = season, stat_type = "rush", summary_level = "season") %>%
    dplyr::select(
      player,
      pfr_player_id = pfr_id, x1d, ybc, ybc_att, yac, yac_att, brk_tkl, att_br
    )

  # Combine with previous data
  rb_expanded <- dplyr::left_join(rb_snap, rb_pfr %>% dplyr::select(-player), by = "pfr_player_id")

  # NextGenStats
  rb_ngs <- nflreadr::load_nextgen_stats(seasons = season, stat_type = "rushing") %>%
    dplyr::filter(player_position == "RB") %>%
    dplyr::group_by(player_id = player_gsis_id, player = player_display_name) %>%
    dplyr::summarise(
      avg_efficiency = mean(efficiency),
      percent_attempts_gte_eight_defenders = mean(percent_attempts_gte_eight_defenders),
      avg_time_to_los = mean(avg_time_to_los),
      avg_rush_yards = mean(avg_rush_yards),
      expected_rush_yards = mean(expected_rush_yards),
      rush_yards_over_expected = mean(rush_yards_over_expected),
      rush_yards_over_expected_per_att = mean(rush_yards_over_expected_per_att),
      rush_pct_over_expected = mean(rush_pct_over_expected)
    )

  # Final RB advanced stats
  rb_adv <- dplyr::left_join(rb_expanded, rb_ngs, by = c("player_id", "player"))

  # Close the database connection
  DBI::dbDisconnect(connect)

  # Return the final RB advanced stats
  return(rb_adv)
}
