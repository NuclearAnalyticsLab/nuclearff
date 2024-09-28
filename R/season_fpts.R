#' QB Season Fantasy Points and Statistics
#'
#' @param pbp_db Play-by-play database table
#' @param szn Season
#'
#' @return
#' Obtain QB cumulative statistics and fantasy points for season
#' @export
get_fpts_qb_season <- function(pbp_db, szn = 2024)
{
  # Cumulative QB play-by-play data
  qb_pbp <- pbp_db %>%
    # Filter for the season and all weeks
    dplyr::filter(season == szn, week >= 1) %>%
    # Query into memory before more actions
    dplyr::collect() %>%
    # Pull up all player stats
    nflfastR::calculate_player_stats() %>%
    # Calculate ppg
    dplyr::mutate(
      # QB TD is worth 4 points (4PT PASS TD) -2 INT
      fpts_4pt_td = fantasy_points_ppr,
      # QB 4PT PASS TD PPG
      ppg_4pt_td = fantasy_points_ppr / games,
      # Completion percentage %
      cmp_pct = attempts / completions,
      # Calculate FPTS when QB TD is worth 6 points (6PT PASS TD) -2 INT
      # Assuming they do not have any catches
      fpts_6pt_td = (0.04 * passing_yards) + (6 * passing_tds) +
        (-2 * interceptions) + (-2 * sack_fumbles_lost) +
        (0.1 * rushing_yards) + (6 * rushing_tds) +
        (-2 * rushing_fumbles_lost),
      # QB 6PT PASS TD PPG
      ppg_6pt_td = fpts_6pt_td / games
    ) %>%
    # Get roster information and filter for QB
    dplyr::inner_join(
      nflfastR::fast_scraper_roster(szn) %>%
        # Filter by position
        dplyr::filter(position == "QB") %>%
        dplyr::select(player_id = gsis_id),
      by = "player_id"
    ) %>%
    # Negative value means descending order!
    # Arrange by total fantasy points
    dplyr::arrange(-fantasy_points_ppr) %>%
    # Only keep these columns
    dplyr::select(player_id, player_name,
                  player = player_display_name,
                  position,
                  team = recent_team, games,
                  fpts_4pt_td, ppg_4pt_td, fpts_6pt_td, ppg_6pt_td,
                  completions, attempts, cmp_pct, passing_yards, passing_tds,
                  interceptions, sacks, sack_yards,
                  sack_fumbles, sack_fumbles_lost,
                  passing_air_yards, passing_yards_after_catch,
                  passing_first_downs, passing_epa, passing_2pt_conversions,
                  pacr, dakota,
                  carries, rushing_yards, rushing_tds,
                  rushing_fumbles, rushing_fumbles_lost,
                  rushing_first_downs, rushing_epa, rushing_2pt_conversions,
    )

  # Obtain snap percentage
  snap_pct <- nuclearff::get_snap_pct(szn, pos = "QB")
  # Join QB pbp data and snap % together for dataset
  qb <- left_join(qb_pbp, snap_pct,
                  by = c("player_id", "player", "position", "team")) %>%
    # Arrange cols so season is the 2nd col
    dplyr::select(player_id, season, everything())
}
