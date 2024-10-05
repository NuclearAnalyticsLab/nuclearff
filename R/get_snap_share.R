################################################################################
# Author: Nolan MacDonald
# Purpose:
#   Obtain player snap share and percentage stats
# Code Style Guide:
#   styler::tidyverse_style(), lintr::use_lintr(type = "tidyverse")
################################################################################

#' Player Snap Share
#'
#' @description
#' Obtain player snap shares provided by Pro Football Reference (PFR),
#' starting with the 2012 season.
#'
#' @details
#' This function loads game level snap counts by PFR starting with the 2012
#' season. Snap count data is then utilized to filter by season and player
#' position. Additional data wrangling is performed to format player names
#' in order to merge easily with play-by-play data.
#' More information is available about snap counts from
#' [Load Snap Counts from PFR](
#'  https://nflreadr.nflverse.com/reference/load_snap_counts.html
#'  ).
#'
#' @seealso \code{\link[nflreadr]{load_players}}
#'  Obtain player information from `nflreadr`
#' @seealso \code{\link[nflreadr]{load_snap_counts}}
#'  Load game level snap counts stats provided by PFR
#'
#' @param seasons NFL Season(s) (int) where `seasons = 2024` or
#'  `seasons = c(2023, 2024)`
#' @param pos Player position (str)
#'
#' @return Obtain player snap share percentage for positional group
#'
#' @author Nolan MacDonald
#'
#' \itemize{
#'  \item{\code{player}}{Player name (e.g., Darnell Mooney)}
#'  \item{\code{position}}{Player name (e.g., WR)}
#'  \item{\code{team}}{Player team}
#'  \item{\code{pfr_player_id}}{Pro Football Reference (PFR) player ID}
#'  \item{\code{snaps}}{Number of snaps}
#'  \item{\code{snap_share}}{Fraction of total snaps}
#'  \item{\code{snap_pct}}{Percentage of total snaps}
#'  }
#'
#' @export
get_snap_share <- function(season = NULL, pos = NULL)
{

  # Pull snap counts from nflreadr
  snaps <- nflreadr::load_snap_counts(season) %>%
    # Filter by position
    dplyr::filter(position %in% pos) %>%
    # Keep this info from snap_counts seasons,
    dplyr::group_by(player, position, team, pfr_player_id) %>%
    # Calculation number of snaps, snap%
    dplyr::summarise(
      # Total number of snaps
      snaps = sum(offense_snaps),
      # Average snap share over season
      snap_share = mean(offense_pct),
      # Convert to percentage e.g. 99.1
      snap_pct = round(snap_share * 100, 1)
    ) %>%
    # Arrange by snaps in descending order
    dplyr::arrange(-snaps) %>%
    # Clean names for continuity (players and teams)
    nuclearff::replace_player_names() %>%
    nuclearff::replace_team_names()

  return(snaps)
}
