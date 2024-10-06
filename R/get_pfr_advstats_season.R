################################################################################
# Author: Nolan MacDonald
# Purpose:
#   Obtain Positional Cumulative Season Advanced Statistics from Pro Football
#   Reference (PFR).
# Code Style Guide:
#   styler::tidyverse_style(), lintr::use_lintr(type = "tidyverse")
################################################################################

#' QB Advanced Season Stats from Pro Football Reference (PFR)
#'
#' @description
#' Obtain QB advanced season stats from `nflreadr`,
#' to acquire Pro Football Reference (PFR) data.
#'
#' @details
#' `get_qb_pfr_advstats_season` is a function to obtain advanced stats from
#' Pro Football Reference (PFR).
#' The function utilizes the `nflreadr` function `load_pfr_advstats` to load
#' player level season stats starting with the 2018 season.
#' For quarterbacks, the `stat_type` is defined as `pass`.
#' More information is available about QB PFR advanced stats from
#' [PFR passing data dictionary](
#'  https://nflreadr.nflverse.com/articles/dictionary_pfr_passing.html
#'  ).
#'
#' @param seasons NFL Season(s) where `seasons = 2024`
#'
#' @return
#'  Dataframe with QB advanced stats from Pro Football Reference (PFR)
#'  for a defined season
#'
#' @seealso \code{\link[nflreadr]{load_pfr_advstats}}
#'  Load advanced stats from PFR
#'
#' @author Nolan MacDonald
#'
#' \itemize{
#'  \item{\code{player_display_name}}{Player name (e.g., Lamar Jackson)}
#'  \item{\code{pass_attempts}}{Total pass attempts}
#'  \item{\code{throwaways}}{Number of throwaways}
#'  \item{\code{spikes}}{Number of spikes}
#'  \item{\code{drops}}{Number of throws dropped}
#'  \item{\code{drop_pct}}{Percentage of dropped throws}
#'  \item{\code{bad_throws}}{Number of bad throws}
#'  \item{\code{bad_throw_pct}}{Percentage of bad throws}
#'  \item{\code{pocket_time}}{Average time in pocket}
#'  \item{\code{times_blitzed}}{Number of times blitzed}
#'  \item{\code{times_hurried}}{Number of times hurried}
#'  \item{\code{times_hit}}{Number of times hit}
#'  \item{\code{times_pressured}}{Number of times pressured}
#'  \item{\code{pressure_pct}}{Percent of time pressured}
#'  \item{\code{batted_balls}}{Number of batted balls}
#'  \item{\code{on_tgt_throws}}{Number of throws on target}
#'  \item{\code{on_tgt_pct}}{Percent of throws on target}
#'  \item{\code{rpo_plays}}{Run-pass-option (RPO) number of plays}
#'  \item{\code{rpo_yards}}{Run-pass-option (RPO) total yards}
#'  \item{\code{rpo_pass_att}}{Run-pass-option (RPO) pass attempts}
#'  \item{\code{rpo_pass_yards}}{Run-pass-option (RPO) pass yards}
#'  \item{\code{rpo_rush_att}}{Run-pass-option (RPO) rush attempts}
#'  \item{\code{rpo_rush_yards}}{Run-pass-option (RPO) rush yards}
#'  \item{\code{pa_pass_att}}{Play action pass attempts}
#'  \item{\code{pa_pass_yards}}{Play action pass yards}
#'  \item{\code{pfr_player_id}}{
#'      Pro Football Reference player ID (e.g., JackLa00)
#'      }
#'      }
#'
#' @export
get_qb_pfr_advstats_season <- function(seasons = NULL) {
  # Pro Football Reference (PFR) Advanced Stats
  # https://www.pro-football-reference.com/years/2024/rushing_advanced.htm

  # Check if season is provided as int (e.g., 2024 or c(2023, 2024))
  # Ensure seasons is using 2018 or later for PFR advanced stats
  nuclearff::validate_pfr_season(seasons)

  qb_pfr <- nflreadr::load_pfr_advstats(
    seasons,
    stat_type = "pass",
    summary_level = "season"
  ) %>%
    # NOTE: There is no position listed in PFR QB advanced stats data
    # No need to filter for pos == "QB"
    # Get the stats for season
    dplyr::select(
      player_display_name = player,
      pass_attempts, # Pass attempts
      throwaways, # Throwaways
      spikes, # Spikes
      drops, # Throws dropped
      drop_pct, # Percentage of throws dropped
      bad_throws, # Bad throws
      bad_throw_pct, # Percentage of bad throws
      pocket_time, # Average time in pocket
      times_blitzed, # Number of times blitzed
      times_hurried, # Number of times hurried
      times_hit, # Number of times hit
      times_pressured, # Number of times pressured
      pressure_pct, # Percent of the time pressured
      batted_balls, # Number of batted balls
      on_tgt_throws, # Number of on target throws
      on_tgt_pct, # Percentage of on target throws
      rpo_plays, # Number of RPO plays
      rpo_yards, # Pass yards on RPO plays
      rpo_pass_att, # Pass attempts on RPO plays
      rpo_pass_yards, # Pass yards on RPO plays
      rpo_rush_att, # Rush attempts on RPO plays
      rpo_rush_yards, # Rush yards on RPO plays
      pa_pass_att, # Play action pass attempts
      pa_pass_yards, # Play action pass yards
      pfr_player_id = pfr_id # Pro Football Reference (PFR) player ID
    )

  return(qb_pfr)
}

#' RB Advanced Season Stats from Pro Football Reference (PFR)
#'
#' @description
#' Obtain RB advanced season stats from `nflreadr`,
#' to acquire Pro Football Reference (PFR) data.
#'
#' @details
#' `get_rb_pfr_advstats_season` is a function to obtain advanced stats from
#' Pro Football Reference (PFR).
#' The function utilizes the `nflreadr` function `load_pfr_advstats` to load
#' player level season stats starting with the 2018 season.
#' For running backs, the `stat_type` is defined as `rush`.
#' More information is available about RB PFR advanced stats from seasons on
#' the PFR site such as
#' [PFR 2024 NFL Advanced Rushing](
#'  https://www.pro-football-reference.com/years/2024/rushing_advanced.htm
#'  ).
#'
#' @param seasons NFL Season(s) where `seasons = 2024`
#'
#' @return
#'  Dataframe with RB advanced stats from Pro Football Reference (PFR)
#'  for a defined season
#'
#' @seealso \code{\link[nflreadr]{load_pfr_advstats}}
#'  Load advanced stats from PFR
#'
#' @author Nolan MacDonald
#'
#' \itemize{
#'  \item{\code{player_display_name}}{Player name (e.g., Lamar Jackson)}
#'  \item{\code{age}}{}
#'  \item{\code{position}}{}
#'  \item{\code{g}}{Number of games played}
#'  \item{\code{gs}}{Number of games started}
#'  \item{\code{att}}{Total rush attempts}
#'  \item{\code{yds}}{Total rush yards}
#'  \item{\code{td}}{Total rush touchdowns}
#'  \item{\code{x1d}}{Rushing first downs}
#'  \item{\code{ybc}}{Rushing yards before contact}
#'  \item{\code{ybc_att}}{Rushing yards before contact per rushing attempt}
#'  \item{\code{yac}}{Rushing yards after contact}
#'  \item{\code{yac_att}}{Rushing yards after contact per attempt}
#'  \item{\code{brk_tkl}}{Broken tackles on rushes}
#'  \item{\code{att_br}}{Rush attempts per broken tackle}
#'  \item{\code{pfr_player_id}}{
#'      Pro Football Reference player ID (e.g., JackLa00)
#'      }
#'      }
#'
#' @export
get_rb_pfr_advstats_season <- function(seasons = NULL) {
  # Pro Football Reference (PFR) Advanced Stats
  # https://www.pro-football-reference.com/years/2024/rushing_advanced.htm

  # Check if season is provided as int (e.g., 2024 or c(2023, 2024))
  # Ensure seasons is using 2018 or later for PFR advanced stats
  nuclearff::validate_pfr_season(seasons)

  rb_pfr <- nflreadr::load_pfr_advstats(
    seasons,
    stat_type = "rush",
    summary_level = "season"
  ) %>%
    # Filter by RB position
    dplyr::filter(pos == "RB") %>%
  # Get the stats for season
  dplyr::select(
    player_display_name = player,
    age, # Age
    position = pos, # Position
    g, # Games
    gs, # Games started
    att, # Rush attempts
    yds, # Rush yards
    td, # Rush touchdowns
    x1d, # First downs rushing
    ybc, # Rushing yards before contact
    ybc_att, # Rushing yards before contact per rushing attempt
    yac, # Rushing yards after contact
    yac_att, # Rushing yards after contact per attempt
    brk_tkl, # Broken tackles on rushes
    att_br, # Rush attempts per broken tackle
    pfr_player_id = pfr_id # Pro Football Reference (PFR) player ID
  )

  return(rb_pfr)
}

#' WR Advanced Season Stats from Pro Football Reference (PFR)
#'
#' @description
#' Obtain WR advanced season stats from `nflreadr`,
#' to acquire Pro Football Reference (PFR) data.
#'
#' @details
#' `get_wr_pfr_advstats_season` is a function to obtain advanced stats from
#' Pro Football Reference (PFR).
#' The function utilizes the `nflreadr` function `load_pfr_advstats` to load
#' player level season stats starting with the 2018 season.
#' For receivers, the `stat_type` is defined as `rec`.
#' More information is available about WR PFR advanced stats from seasons on
#' the PFR site such as
#' [PFR 2024 NFL Advanced Receiving](
#'  https://www.pro-football-reference.com/years/2024/receiving_advanced.htm
#'  ).
#'
#' @param seasons NFL Season(s) where `seasons = 2024`
#'
#' @return
#'  Dataframe with WR advanced stats from Pro Football Reference (PFR)
#'  for a defined season
#'
#' @seealso \code{\link[nflreadr]{load_pfr_advstats}}
#'  Load advanced stats from PFR
#'
#' @author Nolan MacDonald
#'
#' \itemize{
#'  \item{\code{player_display_name}}{Player name (e.g., Malik Nabers)}
#'  \item{\code{age}}{Player age}
#'  \item{\code{position}}{Player position}
#'  \item{\code{g}}{Number of games played}
#'  \item{\code{gs}}{Number of games started}
#'  \item{\code{tgt}}{Total targets}
#'  \item{\code{rec}}{Total receptions}
#'  \item{\code{yds}}{Total receiving yards}
#'  \item{\code{td}}{Total receiving touchdowns}
#'  \item{\code{x1d}}{First downs receiving}
#'  \item{\code{ybc}}{
#'      Total yards passes traveled in the air before being caught or yards
#'      before catch (YBC)}
#'  \item{\code{ybc_r}}{Yards before catch per reception}
#'  \item{\code{yac}}{Yards after catch (YAC)}
#'  \item{\code{yac_r}}{Yards after catch (YAC) per reception}
#'  \item{\code{adot}}{
#'      Average depth of target (ADOT) when targeted, whether completed or not.
#'      }
#'  \item{\code{brk_tkl}}{Number of broken tackles}
#'  \item{\code{rec_br}}{Receptions per broken tackle}
#'  \item{\code{drop}}{Number of dropped passes}
#'  \item{\code{drop_percent}}{Dropped pass percentage when targeted}
#'  \item{\code{int_tgt}}{Interceptions on passes where targeted}
#'  \item{\code{rat}}{Passer rating on passes when targeted}
#'  \item{\code{pfr_player_id}}{
#'      Pro Football Reference player ID (e.g., NabeMa00)
#'      }
#'      }
#'
#' @export
get_wr_pfr_advstats_season <- function(seasons = NULL) {
  # Pro Football Reference (PFR) Advanced Stats
  # https://www.pro-football-reference.com/years/2024/receiving_advanced.htm

  # Check if season is provided as int (e.g., 2024 or c(2023, 2024))
  # Ensure seasons is using 2018 or later for PFR advanced stats
  nuclearff::validate_pfr_season(seasons)

  wr_pfr <- nflreadr::load_pfr_advstats(
    seasons,
    stat_type = "rec",
    summary_level = "season"
  ) %>%
  # Filter by WR position
  dplyr::filter(pos == "WR") %>%
  # Get the stats for season
  dplyr::select(
    player_display_name = player,
    age, # Age
    position = pos, # Position
    g, # Games
    gs, # Games started
    tgt, # Targets
    rec, # Receptions
    yds, # Receiving yards
    td, # Receiving touchdowns
    x1d, # First downs receiving
    ybc, # Total yards passes traveled in the air before being caught
    ybc_r, # yards before catch per reception
    yac, # yards after catch
    yac_r, # yards after catch per reception
    adot, # Average depth of target when targeted, completed or not.
    brk_tkl, # Broken tackles on receptions
    rec_br, # Rec per broken tackle
    drop, # Dropped passes
    drop_percent, # Dropped passes per target
    int_tgt = int, # Interceptions on passes where targeted
    rat, # Passer rating on passes when tarketed
    pfr_player_id = pfr_id # Pro Football Reference (PFR) player ID
  )

  return(wr_pfr)
}

#' TE Advanced Season Stats from Pro Football Reference (PFR)
#'
#' @description
#' Obtain TE advanced season stats from `nflreadr`,
#' to acquire Pro Football Reference (PFR) data.
#'
#' @details
#' `get_te_pfr_advstats_season` is a function to obtain advanced stats from
#' Pro Football Reference (PFR).
#' The function utilizes the `nflreadr` function `load_pfr_advstats` to load
#' player level season stats starting with the 2018 season.
#' For receivers, the `stat_type` is defined as `rec`.
#' More information is available about TE PFR advanced stats from seasons on
#' the PFR site such as
#' [PFR 2024 NFL Advanced Receiving](
#'  https://www.pro-football-reference.com/years/2024/receiving_advanced.htm
#'  ).
#'
#' @param seasons NFL Season(s) where `seasons = 2024`
#'
#' @return
#'  Dataframe with TE advanced stats from Pro Football Reference (PFR)
#'  for a defined season
#'
#' @seealso \code{\link[nflreadr]{load_pfr_advstats}}
#'  Load advanced stats from PFR
#'
#' @author Nolan MacDonald
#'
#' \itemize{
#'  \item{\code{player_display_name}}{Player name (e.g., George Kittle)}
#'  \item{\code{age}}{Player age}
#'  \item{\code{position}}{Player position}
#'  \item{\code{g}}{Number of games played}
#'  \item{\code{gs}}{Number of games started}
#'  \item{\code{tgt}}{Total targets}
#'  \item{\code{rec}}{Total receptions}
#'  \item{\code{yds}}{Total receiving yards}
#'  \item{\code{td}}{Total receiving touchdowns}
#'  \item{\code{x1d}}{First downs receiving}
#'  \item{\code{ybc}}{
#'      Total yards passes traveled in the air before being caught or yards
#'      before catch (YBC)}
#'  \item{\code{ybc_r}}{Yards before catch per reception}
#'  \item{\code{yac}}{Yards after catch (YAC)}
#'  \item{\code{yac_r}}{Yards after catch (YAC) per reception}
#'  \item{\code{adot}}{
#'      Average depth of target (ADOT) when targeted, whether completed or not.
#'      }
#'  \item{\code{brk_tkl}}{Number of broken tackles}
#'  \item{\code{rec_br}}{Receptions per broken tackle}
#'  \item{\code{drop}}{Number of dropped passes}
#'  \item{\code{drop_percent}}{Dropped pass percentage when targeted}
#'  \item{\code{int_tgt}}{Interceptions on passes where targeted}
#'  \item{\code{rat}}{Passer rating on passes when targeted}
#'  \item{\code{pfr_player_id}}{
#'      Pro Football Reference player ID (e.g., KittGe00)
#'      }
#'      }
#'
#' @export
get_te_pfr_advstats_season <- function(seasons = NULL) {
  # Pro Football Reference (PFR) Advanced Stats
  # https://www.pro-football-reference.com/years/2024/receiving_advanced.htm

  # Check if season is provided as int (e.g., 2024 or c(2023, 2024))
  # Ensure seasons is using 2018 or later for PFR advanced stats
  nuclearff::validate_pfr_season(seasons)

  te_pfr <- nflreadr::load_pfr_advstats(
    seasons,
    stat_type = "rec",
    summary_level = "season"
  ) %>%
    # Filter by TE position
    dplyr::filter(pos == "TE") %>%
  # Get the stats for season
  dplyr::select(
    player_display_name = player,
    age, # Age
    position = pos, # Position
    g, # Games
    gs, # Games started
    tgt, # Targets
    rec, # Receptions
    yds, # Receiving yards
    td, # Receiving touchdowns
    x1d, # First downs receiving
    ybc, # Total yards passes traveled in the air before being caught
    ybc_r, # yards before catch per reception
    yac, # yards after catch
    yac_r, # yards after catch per reception
    adot, # Average depth of target when targeted, completed or not.
    brk_tkl, # Broken tackles on receptions
    rec_br, # Rec per broken tackle
    drop, # Dropped passes
    drop_percent, # Dropped passes per target
    int_tgt = int, # Interceptions on passes where targeted
    rat, # Passer rating on passes when targeted
    pfr_player_id = pfr_id # Pro Football Reference (PFR) player ID
  )

  return(te_pfr)
}
