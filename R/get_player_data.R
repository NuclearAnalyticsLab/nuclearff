################################################################################
# Author: Nolan MacDonald
# Purpose:
#   Obtain player data and IDs for different platforms
# Code Style Guide:
#   styler::tidyverse_style(), lintr::use_lintr(type = "tidyverse")
################################################################################

#' Player Identification Data
#'
#' @description
#' Obtain player identification data for various platforms.
#'
#' @details
#' Use the function `get_player_data` to obtain player information for a
#' defined season and position. Player information includes several IDs for
#' platforms such as ESPN, Rotowire, ESPN, etc.
#'
#' @param season NFL season or year (int)
#' @param pos Player position (str) such as "WR"
#'
#' @return Dataframe that lists player data for a specified season and position
#'
#' @seealso \code{\link[nflreadr]{load_rosters}}
#'  Load roster information,
#' @seealso \code{\link[nuclearff]{replace_player_names}}
#'  Cleanup player names for continuity
#' @seealso \code{\link[nuclearff]{replace_team_names}}
#'  Cleanup team abbreviations for continuity
#'
#'  @author Nolan MacDonald
#'
#' @export
get_player_data <- function(season = NULL, pos = NULL) {
  rosters <- nflreadr::load_rosters(seasons = season) %>%
    dplyr::filter(position == pos) %>%
    dplyr::select(
      player_id = gsis_id,
      player = full_name,
      position,
      team,
      espn_id,
      sportradar_id,
      yahoo_id,
      rotowire_id,
      pff_id,
      pfr_id,
      fantasy_data_id,
      sleeper_id,
      esb_id,
      gsis_it_id,
      smart_id
    ) %>%
    # Move the player_id (gsis_id) to first col
    dplyr::relocate(player_id, .before = everything())

  player_data <- rosters %>%
    nuclearff::replace_player_names() %>%
    nuclearff::replace_team_names()

  return(player_data)
}
