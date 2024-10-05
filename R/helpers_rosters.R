################################################################################
# Author: Nolan MacDonald
# Purpose:
#   Helper functions to clean player names and teams for continuity
# Code Style Guide:
#   styler::tidyverse_style(), lintr::use_lintr(type = "tidyverse")
################################################################################

#' Helper - Cleanup Player Names
#'
#' @description
#' String parsing helper to cleanup player names for continuity.
#'
#' @details
#'  The function `replace_player_names` reads a dataframe that includes player
#'  names and uses `str_replace` to clean up names. This can be used across the
#'  package when joining data such that all player names are the same.
#'  The function strips names of suffixes like Jr., Sr., II, III. Also, the
#'  first names that are abbreviations like D.J. Moore and D.K. Metcalf have
#'  the periods stripped from the initials. All other player names are
#'  specified to say the same. Some specific cases are defined explicitly.
#'
#' @param df Dataframe with player column to clean up names
#'
#' @return Dataframe with cleaned up player names
#'
#' @author Nolan MacDonald
#'
#' @export
replace_player_names <- function(df) {
  df %>%
    dplyr::mutate(
      player = dplyr::case_when(
        position == "RB" & player == "De'Von Achane" ~ "Devon Achane",
        position == "RB" & player == "Kenneth Walker III" ~ "Kenneth Walker",
        position == "RB" & player == "Jeff Wilson" ~ "Jeffery Wilson",
        position == "RB" & player == "Chris Brooks" ~ "Christopher Brooks",
        position == "WR" & player == "Gabriel Davis" ~ "Gabe Davis",
        position == "WR" & player == "D.K. Metcalf" ~ "DK Metcalf",
        position == "TE" & player == "AJ Barner" ~ "A.J. Barner",
        # Remove Jr. or Sr. when detected
        stringr::str_detect(player, "\\s*(Jr\\.|Sr\\.)$") ~ stringr::str_replace(player, "\\s*(Jr\\.|Sr\\.)$", ""),
        # Remove II or III when detected
        stringr::str_detect(player, "\\s*(II|III)$") ~ stringr::str_replace(player, "\\s*(II|III)$", ""),
        # Strip initials
        TRUE ~ stringr::str_replace_all(player, "\\b([A-Z])\\.", "\\1") %>%
          stringr::str_replace_all("\\s+", " ") %>%
          stringr::str_trim(),
        # Leave all other names unchanged
        TRUE ~ player
      )
    )
}

#' Helper - Cleanup Team Abbreviations
#'
#' @description
#' String parsing helper to cleanup team abbreviations for continuity.
#'
#' @details
#'  The function `team_player_names` reads a dataframe that includes team
#'  names as `team` and uses `str_replace` to replace the team abbreviation.
#'  This can be used across the package when joining data such that all team
#'  names are the same. Team specific cases are defined explicitly, to
#'  alleviate the issue of some data differences. For example, `nflreadr` roster
#'  data lists the Los Angeles Rams as LA, while `nflfastR` play-by-play data
#'  lists the abbreviation as LAR.
#'
#' @param df Dataframe with team column to clean up abbreviations
#'
#' @return Dataframe with cleaned up team abbreviations
#'
#' @author Nolan MacDonald
#'
#' @export
replace_team_names <- function(df) {
  df %>%
    dplyr::mutate(team = dplyr::case_when(
      team == "LA" ~ "LAR",
      TRUE ~ team  # Leave all other names unchanged
    ))
}
