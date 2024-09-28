#' Player Snap Percentages
#'
#' @param szn Season
#' @param pos Player position
#'
#' @return
#' Obtain player snap share (%) for positional group
#' @export
get_snap_pct <- function(szn = 2024, pos = "WR")
{
  # nflreadr load all players
  players <- nflreadr::load_players() %>%
    # Filter for position
    # dplyr::filter(position == pos) %>%
    # Only keep player_id and name
    dplyr::select(
      player_id = gsis_id,
      display_name,
    )

  # Pull snap counts from nflreadr
  snaps <- nflreadr::load_snap_counts(seasons = szn) %>%
    # Filter by position
    dplyr::filter(position == pos) %>%
    # Keep this info from snap_counts
    dplyr::group_by(season, player, position, team, pfr_player_id) %>%
    # Calculation number of snaps, snap%
    dplyr::summarize(
      snaps = sum(offense_snaps),
      snap_share = mean(offense_pct),
      snap_pct = snap_share * 100
    ) %>%
    # Arrange by snaps in descending order
    dplyr::arrange(-snaps) %>%
    # Strip first name initials periods
    dplyr::mutate(
      player = case_when(
        position == "RB" & player == "De'Von Achane" ~ "Devon Achane",
        position == "RB" & player == "Kenneth Walker III" ~ "Kenneth Walker",
        position == "RB" & player == "Jeff Wilson" ~ "Jeffery Wilson",
        position == "RB" & player == "Chris Brooks" ~ "Christopher Brooks",
        position == "WR" & player == "Gabriel Davis" ~ "Gabe Davis",
        position == "WR" & player == "D.K. Metcalf" ~ "DK Metcalf",
        position == "TE" & player == "AJ Barner" ~ "A.J. Barner",
        TRUE ~ player # Leave all other names unchanged
      ),
      # Remove Jr. and Sr. from names to match pbp
      player = str_replace(player, "\\s*(Jr\\.\\s*|Sr\\.\\s*)$", ""),
      # Remove II from names to match pbp
      player = str_replace(player, "\\s*(II\\s*|III\\s*)$", "")
    )

  # Combine player data and snap percentage
  snap_pct <- left_join(snaps, players,
                        by = c("player" = "display_name")) %>%
    # Move pfr_player_id to end, easier to read
    relocate(pfr_player_id, .after = last_col())
}
