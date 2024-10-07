################################################################################
# Author: Nolan MacDonald
# Purpose:
#   Obtain Positional Cumulative Season Advanced Statistics from NFL
#   Next Gen Stats (NGS).
# Code Style Guide:
#   styler::tidyverse_style(), lintr::use_lintr(type = "tidyverse")
################################################################################

#' QB Cumulative Season Advanced Stats
#'
#' @description
#' Obtain QB stats from NFL cumulative season stats for a specified time frame
#' from either a saved database or `nflreadr::load_pbp()`. The stats are
#' obtained using play-by-play data, NFL Next Gen Stats (NGS) and Pro Football
#' Reference (PFR).
#'
#' @details
#'  The function `get_qb_combined_stats_season` can be utilized to obtain
#'  player stats for a season, including advanced stats.
#'  This includes stats for passing, rushing, and receiving obtained by using
#'  the `nflfastR` function `calculate_player_stats()`.
#'  The player stats are utilized to calculate fantasy points
#'  based on common scoring formats (4/6 point TD, STD, Half PPR, Full PPR).
#'  The function acquires stats by using cumulative play-by-play data
#'  by either loading from a saved database or using `nflreadr::load_pbp()`.
#'  The data is obtained for a user-defined season.
#'  Play-by-play data is merged with NFL Next Gen Stats (NGS) utilizing the
#'  `nflreadr` function `load_nextgen_stats` to load player level weekly stats
#'  starting with the 2016 season.
#'  Play-by-play data is also merged with advanced stats from
#'  Pro Football Reference (PFR), beginning from 2018.
#'  Note that to use this function, `seasons` must be 2018 or later.
#'
#' @param pbp_dp Play-by-Play database path (optional)
#' @param pbp_db_tbl Play-by-Play database table name (optional)
#' @param seasons NFL season (required) to obtain play-by-play data. The
#'  season can be defined as a single season, `season = 2024`. For multiple
#'  seasons, use either `season = c(2023,2024)` or `season = 2022:2024`.
#'
#' @return Dataframe with QB stats for user-defined season(s) obtained from NFL
#'  play-by-play data, Next Gen Stats (NGS) and Pro Football Reference (PFR)
#'
#' @seealso \code{\link[nuclearff]{nuclearff::get_pbp_data}}
#'  Obtain play-by-play data for a specified time frame from either a saved
#'  database or if not defined, using `nflreadr::load_pbp()`
#' @seealso \code{\link[nflreadr]{load_pbp}}
#'  Load play-by-play data
#' @seealso \code{\link[nflfastR]{update_db}}
#'  Update or Create a nflfastR Play-by-Play Database
#' @seealso \code{\link[nflreadr]{load_nextgen_stats}}
#'  Load player level weekly NFL Next Gen Stats
#' @seealso \code{\link[nflreadr]{load_pfr_advstats}}
#'  Load advanced stats from PFR
#'
#' @author Nolan MacDonald
#'
#' @format A data frame with 79 variables that are described below.
#' \describe{
#'  \item{\code{player_id}}{Player gsis id (e.g., 00-0034796)}
#'  \item{\code{player_display_name}}{Player name (e.g., Lamar Jackson)}
#'  \item{\code{player_name}}{Player shortened name (e.g., L.Jackson)}
#'  \item{\code{pos}}{Player position (e.g., QB)}
#'  \item{\code{tm}}{Player team (e.g., BAL)}
#'  \item{\code{g}}{Number of games played}
#'  \item{\code{completions}}{Total pass completions (CMP)from PBP data}
#'  \item{\code{attempts}}{Total pass attempts (ATT)from PBP data}
#'  \item{\code{cmp_pct}}{Pass completion percentage from PBP data}
#'  \item{\code{expected_completion_percentage}}{
#'    Expected completion percentage from NGS
#'    }
#'  \item{\code{completion_percentage_above_expectation}}{
#'      Average expected completion percentage (xCOMP) from NGS
#'      }
#'  \item{\code{passing_yards}}{Total passing yards from PBP data}
#'  \item{\code{passing_tds}}{Total passing touchdowns from PBP data}
#'  \item{\code{interceptions}}{Total pass interceptions (INT) from PBP data}
#'  \item{\code{passing_epa}}{
#'    Total expected points added (EPA) on pass attempts and sacks.
#'    NOTE: This uses the variable `qb_epa`, which gives QB credit for EPA for
#'    up to the point where a receiver lost a fumble after a completed catch
#'    and makes EPA work more like passing yards on plays with fumbles
#'    }
#'  \item{\code{passer_rating}}{Average passer rating from NGS}
#'  \item{\code{pacr}}{
#'    Passing Air Conversion Ratio.
#'    PACR = `passing_yards` / `passing_air_yards`  from PBP data
#'    }
#'  \item{\code{dakota}}{
#'    Adjusted EPA + CPOE composite based on coefficients which best predicts
#'    adjusted EPA/play in the following year from PBP data
#'    }
#'  \item{\code{aggressiveness}}{Average aggressiveness (AGG%) from NGS}
#'  \item{\code{passing_air_yards}}{
#'    Passing air yards (includes incomplete passes) from PBP data
#'    }
#'  \item{\code{passing_yards_after_catch}}{
#'    Yards after the catch gained on plays in which player was the passer
#'    (this is an unofficial stat and may differ slightly between different
#'    sources) from PBP data
#'    }
#'  \item{\code{avg_air_distance}}{Average air distance from NGS}
#'  \item{\code{max_air_distance}}{Maximum or longest air distance from NGS}
#'  \item{\code{avg_time_to_throw}}{Average time to throw (TT) from NGS}
#'  \item{\code{avg_completed_air_yards}}{
#'    Average completed air yards (CAY) from NGS
#'    }
#'  \item{\code{avg_intended_air_yards}}{
#'    Average intended air yards (IAY) from NGS
#'    }
#'  \item{\code{avg_air_yards_differential}}{
#'      Average air yards differential (AYD) from NGS
#'      }
#'  \item{\code{max_completed_air_distance}}{
#'      Maximum or longest completed air distance (LCAD) from NGS
#'      }
#'  \item{\code{avg_air_yards_to_sticks}}{
#'      Average air yards to the sticks (AYTS) from NGS
#'      }
#'  \item{\code{passing_first_downs}}{
#'    First downs on pass attempts from PBP data
#'    }
#'  \item{\code{throwaways}}{Number of throwaways from PFR}
#'  \item{\code{spikes}}{Number of spikes from PFR}
#'  \item{\code{drops}}{Number of throws dropped from PFR}
#'  \item{\code{drop_pct}}{Percentage of dropped throws from PFR}
#'  \item{\code{bad_throws}}{Number of bad throws from PFR}
#'  \item{\code{bad_throw_pct}}{Percentage of bad throws from PFR}
#'  \item{\code{pocket_time}}{Average time in pocket from PFR}
#'  \item{\code{times_blitzed}}{Number of times blitzed from PFR}
#'  \item{\code{times_hurried}}{Number of times hurried from PFR}
#'  \item{\code{times_hit}}{Number of times hit from PFR}
#'  \item{\code{times_pressured}}{Number of times pressured from PFR}
#'  \item{\code{pressure_pct}}{Percent of time pressured from PFR}
#'  \item{\code{batted_balls}}{Number of batted balls from PFR}
#'  \item{\code{on_tgt_throws}}{Number of throws on target from PFR}
#'  \item{\code{on_tgt_pct}}{Percent of throws on target from PFR}
#'  \item{\code{rpo_plays}}{Run-pass-option (RPO) number of plays from PFR}
#'  \item{\code{rpo_yards}}{Run-pass-option (RPO) total yards from PFR}
#'  \item{\code{rpo_pass_att}}{Run-pass-option (RPO) pass attempts from PFR}
#'  \item{\code{rpo_pass_yards}}{Run-pass-option (RPO) pass yards from PFR}
#'  \item{\code{rpo_rush_att}}{Run-pass-option (RPO) rush attempts from PFR}
#'  \item{\code{rpo_rush_yards}}{Run-pass-option (RPO) rush yards from PFR}
#'  \item{\code{pa_pass_att}}{Play action pass attempts from PFR}
#'  \item{\code{pa_pass_yards}}{Play action pass yards from PFR}
#'  \item{\code{passing_2pt_conversions}}{
#'    Two-point conversion passes from PBP data
#'    }
#'  \item{\code{sacks}}{Total number of sacks taken from PBP data}
#'  \item{\code{sack_yards}}{Total yards taken from sacks from PBP data}
#'  \item{\code{sack_fumbles}}{Total fumbles from sacks from PBP data}
#'  \item{\code{sack_fumbles_lost}}{Total fumbles lost from sacks from PBP data}
#'  \item{\code{carries}}{
#'    Number of rush attempts including scrambles and kneel downs. Rushes after
#'    a lateral reception don't count as a carry from PBP data
#'    }
#'  \item{\code{rushing_yards}}{
#'    Yards gained when rushing including scrambles and kneel downs. Also
#'    includes yards gained after obtaining a lateral on a play that started
#'    with a rushing attempt from PBP data
#'    }
#'  \item{\code{rushing_tds}}{
#'    The number of rushing touchdowns (incl. scrambles). Also includes
#'    touchdowns after obtaining a lateral on a play that started with a
#'    rushing attempt from PBP data
#'    }
#'  \item{\code{rushing_fumbles}}{Number of rushes with a fumble from PBP data}
#'  \item{\code{rushing_fumbles_lost}}{
#'    Number of rushes with a lost fumble from PBP data
#'    }
#'  \item{\code{rushing_first_downs}}{
#'    Number of rushing first downs from PBP data
#'    }
#'  \item{\code{rushing_epa}}{
#'    Expected points added (EPA) on rush attempts including scrambles and
#'    kneel downs from PBP data
#'    }
#'  \item{\code{rushing_2pt_conversions}}{
#'    Two-point conversion rushes from PBP data
#'    }
#'  \item{\code{fpts_std_4pt_td}}{
#'    Total fantasy points for standard scoring with 4 point touchdowns
#'    }
#'  \item{\code{ppg_std_4pt_td}}{
#'    Points per game (PPG) for standard scoring with 4 point touchdowns
#'    }
#'  \item{\code{fpts_half_ppr_4pt_td}}{
#'    Total fantasy points for half point per reception (Half PPR) scoring
#'    with 4 point touchdowns
#'    }
#'  \item{\code{ppg_half_ppr_4pt_td}}{
#'    Points per game (PPG) for half point per reception (Half PPR) scoring
#'    with 4 point touchdowns
#'    }
#'  \item{\code{fpts_ppr_4pt_td}}{
#'    Total fantasy points for point per reception (full PPR) scoring with
#'    4 point touchdowns
#'    }
#'  \item{\code{ppg_ppr_4pt_td}}{
#'    Points per game (PPG) for point per reception (Full PPR) scoring
#'    with 4 point touchdowns
#'    }
#'  \item{\code{fpts_std_6pt_td}}{
#'    Total fantasy points for standard scoring with 6 point touchdowns
#'    }
#'  \item{\code{ppg_std_6pt_td}}{
#'    Points per game (PPG) for standard scoring with 6 point touchdowns
#'    }
#'  \item{\code{fpts_half_ppr_6pt_td}}{
#'    Total fantasy points for half point per reception (Half PPR) scoring
#'    with 6 point touchdowns
#'    }
#'  \item{\code{ppg_half_ppr_6pt_td}}{
#'    Points per game (PPG) for half point per reception (Half PPR) scoring
#'    with 6 point touchdowns
#'    }
#'  \item{\code{fpts_ppr_6pt_td}}{
#'    Total fantasy points for point per reception (full PPR) scoring with
#'    6 point touchdowns
#'    }
#'  \item{\code{ppg_ppr_6pt_td}}{
#'    Points per game (PPG) for point per reception (Full PPR) scoring
#'    with 6 point touchdowns
#'    }
#'  \item{\code{pfr_id}}{
#'      Pro Football Reference player ID (e.g., JackLa00)
#'      }
#'  }
#'
#' @export
get_qb_combined_stats_season <- function(pbp_db = NULL,
                                         pbp_db_tbl = NULL,
                                         seasons = NULL) {

  # Combined stats require PBP/NGS/PFR
  # PFR only has data from 2018 and later (1999 PBP, 2016 NGS)
  # Check if seasons is 2018 or later and integer
  nuclearff::validate_pfr_season(seasons)

  # Pull RB PBP data
  qb_pbp <- get_qb_pbp_stats(
    pbp_db,
    pbp_db_tbl,
    seasons,
    week_min = 1 # Pulling entire season starting from week 1
  ) %>%
    # Clean up player names in defined player column
    nuclearff::replace_player_names(player_col = "player_display_name")

  # Pull QB roster information and player IDs
  qb_data <- get_player_data(seasons = 2024,
                             position = "QB"
  ) %>%
    dplyr::select(
      player_id,
      player_display_name = player,
      position,
      team,
      pfr_id
    )

  # Combine PBP data with roster information and IDs
  qb_stats <- qb_pbp %>%
    left_join(qb_data,
              by = c("player_id",
                     "player_display_name",
                     "position",
                     "team")
    )

  # Pull PFR Data
  qb_pfr <- get_qb_pfr_advstats_season(seasons = 2024) %>%
    # Clean up player names in defined player column
    nuclearff::replace_player_names(player_col = "player_display_name")

  # Common variables to combine from PBP and PFR
  common_vars_pbp_pfr <- c("player_display_name",
                           "attempts" = "pass_attempts",
                           "pfr_id" = "pfr_player_id"
  )

  # Common variables to combine from PBP/PFR with NGS
  qb_pbp_pfr <- qb_stats %>%
    left_join(qb_pfr, by = common_vars_pbp_pfr)

  # Pull NGS data
  qb_ngs <- get_qb_ngs_advstats_season(seasons = 2024) %>%
    # Clean up player names in defined player column
    nuclearff::replace_player_names(player_col = "player_display_name")

  # Common variables to combine from PBP/PFR with NGS
  common_vars_pbp_pfr_ngs <- c("player_id",
                               "player_display_name",
                               "player_name",
                               "position" = "player_position",
                               "team",
                               "attempts",
                               "completions",
                               "passing_yards" = "pass_yards",
                               "passing_tds" = "pass_touchdowns",
                               "interceptions",
                               "cmp_pct" = "completion_percentage"
  )


  # Combine QB play-by-play data, PFR data, and NGS data
  qb_combined_stats <- qb_pbp_pfr %>%
    left_join(qb_ngs, by = common_vars_pbp_pfr_ngs) %>%
    dplyr::select(
      player_id, # pbp
      player_display_name, # pbp
      player_name, # pbp
      pos = position, # pbp
      tm = team, # pbp
      g = games, # pbp
      # PBP
      # Passing
      completions, # PBP
      attempts, # PBP
      cmp_pct, # PBP
      # NGS Average expected completion percentage (xCOMP)
      expected_completion_percentage,
      # NGS Average completion percentage above expectation (CPOE)
      completion_percentage_above_expectation,
      passing_yards, # PBP
      passing_tds, # PBP
      interceptions, # PBP
      passing_epa, # PBP
      passer_rating, # NGS passer rating
      pacr, # PBP
      dakota, # PBP
      # NGS Average aggressiveness (AGG%)
      aggressiveness,

      passing_air_yards, # PBP
      passing_yards_after_catch, # PBP
      # NGS Average air distance
      avg_air_distance,
      # NGS Maximum or longest air distance
      max_air_distance,
      # NGS Average time to throw (TT)
      avg_time_to_throw,
      # NGS Average completed air yards (CAY)
      avg_completed_air_yards,
      # NGS Average intended air yards (IAY)
      avg_intended_air_yards,
      # NGS Average air yards differential (AYD)
      avg_air_yards_differential,
      # NGS Maximum or longest completed air distance (LCAD)
      max_completed_air_distance,
      # NGS Average air yards to the sticks (AYTS)
      avg_air_yards_to_sticks,
      passing_first_downs, # PBP
      # PFR
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
      # PBP
      passing_2pt_conversions, # PBP
      sacks, # PBP
      sack_yards, # PBP
      sack_fumbles, # PBP
      sack_fumbles_lost, # PBP
      # PBP Rush
      carries,
      rushing_yards,
      rushing_tds,
      rushing_fumbles,
      rushing_fumbles_lost,
      rushing_first_downs,
      rushing_epa,
      rushing_2pt_conversions,
      # Fantasy Points
      fpts_std_4pt_td,
      ppg_std_4pt_td,
      fpts_half_ppr_4pt_td,
      ppg_half_ppr_4pt_td,
      fpts_ppr_4pt_td,
      ppg_ppr_4pt_td,
      fpts_std_6pt_td,
      ppg_std_6pt_td,
      fpts_half_ppr_6pt_td,
      ppg_half_ppr_6pt_td,
      fpts_ppr_6pt_td,
      ppg_ppr_6pt_td,
      # PFR
      pfr_id # Pro Football Reference (PFR) player ID
    )

  return(qb_combined_stats)
}

#' RB Cumulative Season Advanced Stats
#'
#' @description
#' Obtain RB stats from NFL cumulative season stats for a specified time frame
#' from either a saved database or `nflreadr::load_pbp()`. The stats are
#' obtained using play-by-play data, NFL Next Gen Stats (NGS) and Pro Football
#' Reference (PFR).
#'
#' @details
#'  The function `get_rb_combined_stats_season` can be utilized to obtain
#'  player stats for a season, including advanced stats.
#'  This includes stats for passing, rushing, and receiving obtained by using
#'  the `nflfastR` function `calculate_player_stats()`.
#'  The player stats are utilized to calculate fantasy points
#'  based on common scoring formats (4/6 point TD, STD, Half PPR, Full PPR).
#'  The function acquires stats by using cumulative play-by-play data
#'  by either loading from a saved database or using `nflreadr::load_pbp()`.
#'  The data is obtained for a user-defined season.
#'  Play-by-play data is merged with NFL Next Gen Stats (NGS) utilizing the
#'  `nflreadr` function `load_nextgen_stats` to load player level weekly stats
#'  starting with the 2016 season.
#'  Play-by-play data is also merged with advanced stats from
#'  Pro Football Reference (PFR), beginning from 2018.
#'  Note that to use this function, `seasons` must be 2018 or later.
#'
#' @param pbp_dp Play-by-Play database path (optional)
#' @param pbp_db_tbl Play-by-Play database table name (optional)
#' @param seasons NFL season (required) to obtain play-by-play data. The
#'  season can be defined as a single season, `season = 2024`. For multiple
#'  seasons, use either `season = c(2023,2024)` or `season = 2022:2024`.
#'
#' @return Dataframe with QB stats for user-defined season(s) obtained from NFL
#'  play-by-play data, Next Gen Stats (NGS) and Pro Football Reference (PFR)
#'
#' @seealso \code{\link[nuclearff]{nuclearff::get_pbp_data}}
#'  Obtain play-by-play data for a specified time frame from either a saved
#'  database or if not defined, using `nflreadr::load_pbp()`
#' @seealso \code{\link[nflreadr]{load_pbp}}
#'  Load play-by-play data
#' @seealso \code{\link[nflfastR]{update_db}}
#'  Update or Create a nflfastR Play-by-Play Database
#' @seealso \code{\link[nflreadr]{load_nextgen_stats}}
#'  Load player level weekly NFL Next Gen Stats
#' @seealso \code{\link[nflreadr]{load_pfr_advstats}}
#'  Load advanced stats from PFR
#'
#' @author Nolan MacDonald
#'
#' @format A data frame with 57 variables that are described below.
#' \describe{
#'  \item{\code{player_id}}{Player gsis id (e.g., 00-0034796)}
#'  \item{\code{player_display_name}}{Player name (e.g., Lamar Jackson)}
#'  \item{\code{player_name}}{Player shortened name (e.g., L.Jackson)}
#'  \item{\code{pos}}{Player position (e.g., QB)}
#'  \item{\code{tm}}{Player team (e.g., BAL)}
#'  \item{\code{g}}{Number of games played}
#'  \item{\code{carries}}{
#'    Number of rush attempts including scrambles and kneel downs. Rushes after
#'    a lateral reception don't count as a carry from PBP data
#'    }
#'  \item{\code{rushing_yards}}{
#'    Yards gained when rushing including scrambles and kneel downs. Also
#'    includes yards gained after obtaining a lateral on a play that started
#'    with a rushing attempt from PBP data
#'    }
#'  \item{\code{rushing_tds}}{
#'    The number of rushing touchdowns (incl. scrambles). Also includes
#'    touchdowns after obtaining a lateral on a play that started with a
#'    rushing attempt from PBP data
#'    }
#'  \item{\code{rushing_fumbles}}{Number of rushes with a fumble from PBP data}
#'  \item{\code{rushing_fumbles_lost}}{
#'    Number of rushes with a lost fumble from PBP data
#'    }
#'  \item{\code{rushing_first_downs}}{
#'    Number of rushing first downs from PBP data
#'    }
#'  \item{\code{rushing_epa}}{
#'    Expected points added (EPA) on rush attempts including scrambles and
#'    kneel downs from PBP data
#'    }
#'  \item{\code{rushing_2pt_conversions}}{
#'    Two-point conversion rushes from PBP data
#'    }
#'  \item{\code{efficiency}}{Average efficiency (EFF) from NGS}
#'  \item{\code{percent_attempts_gte_eight_defenders }}{
#'      Average percent attempts with 8+ Defenders in the Box (8+D) from NGS
#'      }
#'  \item{\code{avg_time_to_los}}{
#'    Average time behind line of scrimmage (TLOS) from NGS
#'    }
#'  \item{\code{avg_rush_yards}}{Average rush yards per attempt}
#'  \item{\code{expected_rush_yards}}{Average expected rush yards from NGS}
#'  \item{\code{rush_yards_over_expected}}{
#'    Average rush yards over expected from NGS
#'    }
#'  \item{\code{rush_yards_over_expected_per_att}}{
#'      Average rush yards over expected per attempt from NGS
#'      }
#'  \item{\code{rush_pct_over_expected}}{
#'    Average rush percent over expected from NGS
#'    }
#'  \item{\code{ybc}}{Rushing yards before contact from PFR}
#'  \item{\code{ybc_att}}{
#'    Rushing yards before contact per rushing attempt from PFR
#'    }
#'  \item{\code{yac}}{Rushing yards after contact from PFR}
#'  \item{\code{yac_att}}{Rushing yards after contact per attempt from PFR}
#'  \item{\code{brk_tkl}}{Broken tackles on rushes from PFR}
#'  \item{\code{att_br}}{Rush attempts per broken tackle from PFR}
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
#'  \item{\code{tgt}}{
#'    Number of pass plays where the player was targeted as a receiver from
#'    PBP data
#'    }
#'  \item{\code{rec}}{
#'    Number of pass receptions. Lateral receptions don't count as a reception
#'    from PBP data
#'    }
#'  \item{\code{rec_yds}}{
#'    Yards gained after a pass reception. Includes yards gained after
#'    receiving a lateral on a play that started as a pass play from PBP data
#'    }
#'  \item{\code{rec_td}}{
#'    Number of reception touchdowns, including after receiving a lateral on a
#'    play that began as a pass play from PBP data
#'    }
#'  \item{\code{receiving_fumbles}}{Number of fumbles after a pass reception
#'    from PBP data
#'    }
#'  \item{\code{receiving_fumbles_lost}}{
#'    Number of fumbles lost after a pass reception from PBP data
#'    }
#'  \item{\code{receiving_air}}{
#'    Receiving air yards including incomplete passes from PBP data
#'    }
#'  \item{\code{yac_rec}}{
#'    Yards after the catch gained on plays in which player was receiver (this
#'    is an unofficial stat and may differ slightly between different sources)
#'     from PBP data
#'    }
#'  \item{\code{x1d_rec}}{
#'    Number of first downs gained on a reception from PBP data
#'    }
#'  \item{\code{receiving_epa}}{Expected points added on receptions
#'    from PBP data
#'    }
#'  \item{\code{pfr_id}}{
#'      Pro Football Reference player ID (e.g., JackLa00)
#'      }
#'  }
#'
#' @export
get_rb_combined_stats_season <- function(pbp_db = NULL,
                                         pbp_db_tbl = NULL,
                                         seasons = NULL) {

  # Combined stats require PBP/NGS/PFR
  # PFR only has data from 2018 and later (1999 PBP, 2016 NGS)
  # Check if seasons is 2018 or later and integer
  nuclearff::validate_pfr_season(seasons)

  # Pull RB PBP data
  rb_pbp <- nuclearff::get_rb_pbp_stats(
    pbp_db,
    pbp_db_tbl,
    seasons,
    week_min = 1 # Pulling entire season starting from week 1
  ) %>%
    # Clean up player names in defined player column
    nuclearff::replace_player_names(player_col = "player_display_name")

  # Pull RB roster information and player IDs
  rb_data <- nuclearff::get_player_data(seasons = 2024,
                             position = "RB"
  ) %>%
    dplyr::select(
      player_id,
      player_display_name = player,
      position,
      team,
      pfr_id
    ) %>%
    # Clean up player names in defined player column
    nuclearff::replace_player_names(player_col = "player_display_name")

  # Combine PBP data with roster information and IDs
  rb_stats <- rb_pbp %>%
    left_join(rb_data,
              by = c("player_id",
                     "player_display_name",
                     "position",
                     "team")
    )

  # Pull PFR data
  rb_pfr <- nuclearff::get_rb_pfr_advstats_season(seasons = 2024) %>%
    # Clean up player names in defined player column
    nuclearff::replace_player_names(player_col = "player_display_name")

  # Common variables to combine from PBP and PFR
  common_vars_pbp_pfr <- c("player_display_name",
                           "position",
                           "games" = "g",
                           "carries" = "att",
                           "rushing_yards" = "yds",
                           "rushing_tds" = "td",
                           "rushing_first_downs" = "x1d",
                           "pfr_id" = "pfr_player_id"
  )

  # Common variables to combine from PBP/PFR with NGS
  rb_pbp_pfr <- rb_stats %>%
    left_join(rb_pfr, by = common_vars_pbp_pfr)

  # Pull NGS data
  rb_ngs <- nuclearff::get_rb_ngs_advstats_season(seasons = 2024) %>%
    # Clean up player names in defined player column
    nuclearff::replace_player_names(player_col = "player_display_name")

  # Common variables to combine from PBP/PFR with NGS
  common_vars_pbp_pfr_ngs <- c("player_id",
                               "player_display_name",
                               "player_name",
                               "team",
                               "carries" = "rush_attempts",
                               "rushing_yards" = "rush_yards",
                               "rushing_tds" = "rush_touchdowns"
  )

  # Combine RB play-by-play data, PFR data, and NGS data
  rb_combined_stats <- rb_pbp_pfr %>%
    left_join(rb_ngs, by = common_vars_pbp_pfr_ngs) %>%
  dplyr::select(
    player_id,
    player_display_name,
    player_name,
    pos = position,
    tm = team,
    g = games,
    # PBP RUSH
    carries,
    rushing_yards,
    rushing_tds,
    rushing_fumbles,
    rushing_fumbles_lost,
    rushing_first_downs,
    rushing_epa,
    rushing_2pt_conversions,
    # NGS
    efficiency,
    percent_attempts_gte_eight_defenders,
    avg_time_to_los,
    avg_rush_yards,
    expected_rush_yards,
    rush_yards_over_expected,
    rush_yards_over_expected_per_att,
    rush_pct_over_expected,
    # PFR
    ybc,
    ybc_att,
    yac,
    yac_att,
    brk_tkl,
    att_br,
    # Fantasy Points
    fpts_std_4pt_td,
    ppg_std_4pt_td,
    fpts_half_ppr_4pt_td,
    ppg_half_ppr_4pt_td,
    fpts_ppr_4pt_td,
    ppg_ppr_4pt_td,
    fpts_std_6pt_td,
    ppg_std_6pt_td,
    fpts_half_ppr_6pt_td,
    ppg_half_ppr_6pt_td,
    fpts_ppr_6pt_td,
    ppg_ppr_6pt_td,
    # PBP REC
    tgt = targets,
    rec = receptions,
    rec_yds = receiving_yards,
    rec_td = receiving_tds,
    receiving_fumbles,
    receiving_fumbles_lost,
    rec_air = receiving_air_yards,
    yac_rec = receiving_yards_after_catch, # yards after catch (YAC)
    x1d_rec = receiving_first_downs, # Receiving first downs (x1D)
    receiving_epa, # Receiving expected points added (EPA)
    receiving_2pt_conversions,
    racr,
    target_share, # Target share fraction for team
    tgt_pct, # Target share percentage for team (%TM)
    air_yards_share,
    wopr,
    # PFR
    pfr_id
  )

  return(rb_combined_stats)
}

#' WR Cumulative Season Advanced Stats
#'
#' @description
#' Obtain WR stats from NFL cumulative season stats for a specified time frame
#' from either a saved database or `nflreadr::load_pbp()`. The stats are
#' obtained using play-by-play data, NFL Next Gen Stats (NGS) and Pro Football
#' Reference (PFR).
#'
#' @details
#'  The function `get_wr_combined_stats_season` can be utilized to obtain
#'  player stats for a season, including advanced stats.
#'  This includes stats for passing, rushing, and receiving obtained by using
#'  the `nflfastR` function `calculate_player_stats()`.
#'  The player stats are utilized to calculate fantasy points
#'  based on common scoring formats (4/6 point TD, STD, Half PPR, Full PPR).
#'  The function acquires stats by using cumulative play-by-play data
#'  by either loading from a saved database or using `nflreadr::load_pbp()`.
#'  The data is obtained for a user-defined season.
#'  Play-by-play data is merged with NFL Next Gen Stats (NGS) utilizing the
#'  `nflreadr` function `load_nextgen_stats` to load player level weekly stats
#'  starting with the 2016 season.
#'  Play-by-play data is also merged with advanced stats from
#'  Pro Football Reference (PFR), beginning from 2018.
#'  Note that to use this function, `seasons` must be 2018 or later.
#'
#' @param pbp_dp Play-by-Play database path (optional)
#' @param pbp_db_tbl Play-by-Play database table name (optional)
#' @param seasons NFL season (required) to obtain play-by-play data. The
#'  season can be defined as a single season, `season = 2024`. For multiple
#'  seasons, use either `season = c(2023,2024)` or `season = 2022:2024`.
#' @return Dataframe with QB stats for user-defined season(s) obtained from NFL
#'  play-by-play data, Next Gen Stats (NGS) and Pro Football Reference (PFR)
#'
#' @seealso \code{\link[nuclearff]{nuclearff::get_pbp_data}}
#'  Obtain play-by-play data for a specified time frame from either a saved
#'  database or if not defined, using `nflreadr::load_pbp()`
#' @seealso \code{\link[nflreadr]{load_pbp}}
#'  Load play-by-play data
#' @seealso \code{\link[nflfastR]{update_db}}
#'  Update or Create a nflfastR Play-by-Play Database
#' @seealso \code{\link[nflreadr]{load_nextgen_stats}}
#'  Load player level weekly NFL Next Gen Stats
#' @seealso \code{\link[nflreadr]{load_pfr_advstats}}
#'  Load advanced stats from PFR
#'
#' @author Nolan MacDonald
#'
#' @format A data frame with 61 variables that are described below.
#' \describe{
#'  \item{\code{player_id}}{Player gsis id (e.g., 00-0034796)}
#'  \item{\code{player_display_name}}{Player name (e.g., Lamar Jackson)}
#'  \item{\code{player_name}}{Player shortened name (e.g., L.Jackson)}
#'  \item{\code{pos}}{Player position (e.g., QB)}
#'  \item{\code{tm}}{Player team (e.g., BAL)}
#'  \item{\code{g}}{Number of games played}
#'  \item{\code{tgt}}{
#'    Number of pass plays where the player was targeted as a receiver from
#'    PBP data
#'    }
#'  \item{\code{rec}}{
#'    Number of pass receptions. Lateral receptions don't count as a reception
#'    from PBP data
#'    }
#'  \item{\code{rec_yds}}{
#'    Yards gained after a pass reception. Includes yards gained after
#'    receiving a lateral on a play that started as a pass play from PBP data
#'    }
#'  \item{\code{rec_td}}{
#'    Number of reception touchdowns, including after receiving a lateral on a
#'    play that began as a pass play from PBP data
#'    }
#'  \item{\code{avg_yac}}{Average yards after catch (YAC) from NGS}
#'  \item{\code{avg_expected_yac}}{Average expected yards after catch (xYAC) from NGS}
#'  \item{\code{avg_yac_above_expectation}}{
#'      Average yards after catch above expectation (+/-) from NGS
#'      }
#'  \item{\code{yac}}{Yards after catch (YAC) from PFR}
#'  \item{\code{yac_r}}{Yards after catch (YAC) per reception from PFR}
#'  \item{\code{adot}}{
#'      Average depth of target (ADOT) when targeted, whether completed or not,
#'      from PFR
#'      }
#'  \item{\code{receiving_epa}}{Expected points added on receptions
#'    from PBP data
#'    }
#'  \item{\code{racr}}{
#'    Receiving Air Conversion Ratio.
#'    RACR = `receiving_yards` / `receiving_air_yards` from PBP
#'    }
#'  \item{\code{wopr}}{
#'    Weighted Opportunity Rating.
#'    WOPR = 1.5 x `target_share` + 0.7 x `air_yards_share` from PBP data
#'    }
#'  \item{\code{avg_cushion}}{Average cushion (CUSH) from NGS}
#'  \item{\code{avg_separation}}{Average separation (SEP) from NGS}
#'  \item{\code{target_share}}{
#'    Share of targets of player compared to all team targets from PBP
#'    }
#'  \item{\code{tgt_pct}}{Share of targets percentage from PBP}
#'  \item{\code{ybc}}{
#'      Total yards passes traveled in the air before being caught or yards
#'      before catch (YBC) from PFR}
#'  \item{\code{ybc_r}}{Yards before catch per reception from PFR}
#'  \item{\code{receiving_air}}{
#'    Receiving air yards including incomplete passes from PBP data
#'    }
#'  \item{\code{avg_intended_air_yards}}{Average targeted air yards (TAY) from NGS}
#'  \item{\code{percent_share_of_intended_air_yards}}{
#'      Average % share of team's air yards (TAY%) from NGS
#'      }
#'  \item{\code{air_yards_share}}{
#'    Share of `receiving_air_yards` of the player to all team `air_yards`
#'    from PBP data
#'    }
#'  \item{\code{x1d_rec}}{
#'    Number of first downs gained on a reception from PBP data
#'    }
#'  \item{\code{brk_tkl}}{Number of broken tackles from PFR}
#'  \item{\code{rec_br}}{Receptions per broken tackle from PFR}
#'  \item{\code{drop}}{Number of dropped passes from PFR}
#'  \item{\code{drop_percent}}{Dropped pass percentage when targeted from PFR}
#'  \item{\code{catch_percentage}}{Average catch percentage from NGS}
#'  \item{\code{receiving_fumbles}}{Number of fumbles after a pass reception
#'    from PBP data
#'    }
#'  \item{\code{receiving_fumbles_lost}}{
#'    Number of fumbles lost after a pass reception from PBP data
#'    }
#'  \item{\code{receiving_2pt_conversions}}{
#'    Two-point conversion receptions from PBP data
#'    }
#'  \item{\code{int_tgt}}{Interceptions on passes where targetedfrom PFR}
#'  \item{\code{rat}}{Passer rating on passes when targeted from PFR}
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
#'    a lateral reception don't count as a carry from PBP data
#'    }
#'  \item{\code{rushing_yards}}{
#'    Yards gained when rushing including scrambles and kneel downs. Also
#'    includes yards gained after obtaining a lateral on a play that started
#'    with a rushing attempt from PBP data
#'    }
#'  \item{\code{rushing_tds}}{
#'    The number of rushing touchdowns (incl. scrambles). Also includes
#'    touchdowns after obtaining a lateral on a play that started with a
#'    rushing attempt from PBP data
#'    }
#'  \item{\code{rushing_fumbles}}{Number of rushes with a fumble from PBP data}
#'  \item{\code{rushing_fumbles_lost}}{
#'    Number of rushes with a lost fumble from PBP data
#'    }
#'  \item{\code{rushing_first_downs}}{
#'    Number of rushing first downs from PBP data
#'    }
#'  \item{\code{rushing_epa}}{
#'    Expected points added (EPA) on rush attempts including scrambles and
#'    kneel downs from PBP data
#'    }
#'  \item{\code{rushing_2pt_conversions}}{
#'    Two-point conversion rushes from PBP data
#'    }
#'  \item{\code{pfr_id}}{
#'      Pro Football Reference player ID (e.g., JackLa00)
#'      }
#'  }
#'
#' @export
get_wr_combined_stats_season <- function(pbp_db = NULL,
                                         pbp_db_tbl = NULL,
                                         seasons = NULL) {

  # Combined stats require PBP/NGS/PFR
  # PFR only has data from 2018 and later (1999 PBP, 2016 NGS)
  # Check if seasons is 2018 or later and integer
  nuclearff::validate_pfr_season(seasons)

  # Pull WR PBP data
  wr_pbp <- nuclearff::get_wr_pbp_stats(
    pbp_db,
    pbp_db_tbl,
    seasons,
    week_min = 1 # Pulling entire season starting from week 1
  ) %>%
    # Clean up player names in defined player column
    nuclearff::replace_player_names(player_col = "player_display_name")

  # Pull WR roster information and player IDs
  wr_data <- nuclearff::get_player_data(seasons = 2024,
                             position = "WR"
  ) %>%
    dplyr::select(
      player_id,
      player_display_name = player,
      position,
      team,
      pfr_id
    ) %>%
    # Clean up player names in defined player column
    nuclearff::replace_player_names(player_col = "player_display_name")

  # Combine PBP data with roster information and IDs
  wr_stats <- wr_pbp %>%
    left_join(wr_data,
              by = c("player_id",
                     "player_display_name",
                     "position",
                     "team")
    )

  # Pull PFR data
  wr_pfr <- nuclearff::get_wr_pfr_advstats_season(seasons = 2024) %>%
    # Clean up player names in defined player column
    nuclearff::replace_player_names(player_col = "player_display_name")

  # Common variables to combine from PBP and PFR
  common_vars_pbp_pfr <- c("player_display_name",
                           "position",
                           "games" = "g",
                           "targets" = "tgt",
                           "receptions" = "rec",
                           "receiving_yards" = "yds",
                           "receiving_tds" = "td",
                           "receiving_first_downs" = "x1d",
                           "receiving_yards_after_catch" = "yac",
                           "pfr_id" = "pfr_player_id"
  )

  # Combine PBP with PFR
  wr_pbp_pfr <- wr_stats %>%
    left_join(wr_pfr, by = common_vars_pbp_pfr)

  # Pull NGS data
  wr_ngs <- nuclearff::get_wr_ngs_advstats_season(seasons = 2024) %>%
    # Clean up player names in defined player column
    nuclearff::replace_player_names(player_col = "player_display_name")

  # Common variables to combine from PBP/PFR with NGS
  common_vars_pbp_pfr_ngs <- c("player_id",
                               "player_display_name",
                               "player_name",
                               "team",
                               "targets",
                               "receptions",
                               "receiving_yards" = "yards",
                               "receiving_tds" = "rec_touchdowns"
  )

  # Combine WR play-by-play data, PFR data, and NGS data
  # NOTE: adot and rat 25 targets/16 game pace required for leaderboards
  wr_combined_stats <- wr_pbp_pfr %>%
    left_join(wr_ngs, by = common_vars_pbp_pfr_ngs) %>%
    dplyr::select(
      player_id, # pbp
      player_display_name, # pbp
      player_name, # pbp
      pos = position, # pbp
      tm = team, # pbp
      g = games, # pbp
      # PBP
      tgt = targets, # pbp
      rec = receptions, # pbp
      rec_yds = receiving_yards, # pbp
      rec_td = receiving_tds, # pbp
      avg_yac, # NGS
      avg_expected_yac, # NGS
      avg_yac_above_expectation, # NGS
      yac = receiving_yards_after_catch, # pbp yards after catch (YAC)
      yac_r, # PFR - Yards after catch per reception (YAC/R)
      adot, # PFR Average depth of target (ADOT)
      receiving_epa, # pbp Receiving expected points added (EPA)
      racr, # PBP
      wopr, # PBP
      avg_cushion, # NGS
      avg_separation, # NGS
      target_share, # pbp Target share fraction for team
      tgt_pct, # pbp Target share percentage for team (%TM)
      ybc, # pfr Total yards passes traveled in air before caught (YBC)
      ybc_r, # pfr Total yards passes traveled in air before caught per rec (YBC/R)
      rec_air = receiving_air_yards, # PBP
      avg_intended_air_yards, # NGS
      percent_share_of_intended_air_yards, # NGS
      air_yards_share, # pbp
      x1d = receiving_first_downs, # pbp Receiving first downs (x1D)
      brk_tkl, # pfr Number of broken tackles on receptions
      rec_br, # pfr Receptions per broken tackle
      drop, # pfr Dropped passes (DROP)
      drop_percent, # pfr Percent of dropped passes (DROP%)
      catch_percentage, # NGS
      receiving_fumbles, # pbp
      receiving_fumbles_lost, # pbp
      receiving_2pt_conversions, # pbp
      int_tgt, # pfr Number of targets where the pass was intercepted
      rat, # pfr Passer rating on passes when targeted.
      # Fantasy Points
      fpts_std_4pt_td,
      ppg_std_4pt_td,
      fpts_half_ppr_4pt_td,
      ppg_half_ppr_4pt_td,
      fpts_ppr_4pt_td,
      ppg_ppr_4pt_td,
      fpts_std_6pt_td,
      ppg_std_6pt_td,
      fpts_half_ppr_6pt_td,
      ppg_half_ppr_6pt_td,
      fpts_ppr_6pt_td,
      ppg_ppr_6pt_td,
      # PBP Rush
      carries,
      rushing_yards,
      rushing_tds,
      rushing_fumbles,
      rushing_fumbles_lost,
      rushing_first_downs,
      rushing_epa,
      rushing_2pt_conversions,
      # PFR
      pfr_id
    )

  return(wr_combined_stats)
}

#' TE Cumulative Season Advanced Stats
#'
#' @description
#' Obtain TE stats from NFL cumulative season stats for a specified time frame
#' from either a saved database or `nflreadr::load_pbp()`. The stats are
#' obtained using play-by-play data, NFL Next Gen Stats (NGS) and Pro Football
#' Reference (PFR).
#'
#' @details
#'  The function `get_te_combined_stats_season` can be utilized to obtain
#'  player stats for a season, including advanced stats.
#'  This includes stats for passing, rushing, and receiving obtained by using
#'  the `nflfastR` function `calculate_player_stats()`.
#'  The player stats are utilized to calculate fantasy points
#'  based on common scoring formats (4/6 point TD, STD, Half PPR, Full PPR).
#'  The function acquires stats by using cumulative play-by-play data
#'  by either loading from a saved database or using `nflreadr::load_pbp()`.
#'  The data is obtained for a user-defined season.
#'  Play-by-play data is merged with NFL Next Gen Stats (NGS) utilizing the
#'  `nflreadr` function `load_nextgen_stats` to load player level weekly stats
#'  starting with the 2016 season.
#'  Play-by-play data is also merged with advanced stats from
#'  Pro Football Reference (PFR), beginning from 2018.
#'  Note that to use this function, `seasons` must be 2018 or later.
#'
#' @param pbp_dp Play-by-Play database path (optional)
#' @param pbp_db_tbl Play-by-Play database table name (optional)
#' @param seasons NFL season (required) to obtain play-by-play data. The
#'  season can be defined as a single season, `season = 2024`. For multiple
#'  seasons, use either `season = c(2023,2024)` or `season = 2022:2024`.
#'
#' @return Dataframe with QB stats for user-defined season(s) obtained from NFL
#'  play-by-play data, Next Gen Stats (NGS) and Pro Football Reference (PFR)
#'
#' @seealso \code{\link[nuclearff]{nuclearff::get_pbp_data}}
#'  Obtain play-by-play data for a specified time frame from either a saved
#'  database or if not defined, using `nflreadr::load_pbp()`
#' @seealso \code{\link[nflreadr]{load_pbp}}
#'  Load play-by-play data
#' @seealso \code{\link[nflfastR]{update_db}}
#'  Update or Create a nflfastR Play-by-Play Database
#' @seealso \code{\link[nflreadr]{load_nextgen_stats}}
#'  Load player level weekly NFL Next Gen Stats
#' @seealso \code{\link[nflreadr]{load_pfr_advstats}}
#'  Load advanced stats from PFR
#'
#' @author Nolan MacDonald
#'
#' @format A data frame with 61 variables that are described below.
#' \describe{
#'  \item{\code{player_id}}{Player gsis id (e.g., 00-0034796)}
#'  \item{\code{player_display_name}}{Player name (e.g., Lamar Jackson)}
#'  \item{\code{player_name}}{Player shortened name (e.g., L.Jackson)}
#'  \item{\code{pos}}{Player position (e.g., QB)}
#'  \item{\code{tm}}{Player team (e.g., BAL)}
#'  \item{\code{g}}{Number of games played}
#'  \item{\code{tgt}}{
#'    Number of pass plays where the player was targeted as a receiver from
#'    PBP data
#'    }
#'  \item{\code{rec}}{
#'    Number of pass receptions. Lateral receptions don't count as a reception
#'    from PBP data
#'    }
#'  \item{\code{rec_yds}}{
#'    Yards gained after a pass reception. Includes yards gained after
#'    receiving a lateral on a play that started as a pass play from PBP data
#'    }
#'  \item{\code{rec_td}}{
#'    Number of reception touchdowns, including after receiving a lateral on a
#'    play that began as a pass play from PBP data
#'    }
#'  \item{\code{avg_yac}}{Average yards after catch (YAC) from NGS}
#'  \item{\code{avg_expected_yac}}{Average expected yards after catch (xYAC) from NGS}
#'  \item{\code{avg_yac_above_expectation}}{
#'      Average yards after catch above expectation (+/-) from NGS
#'      }
#'  \item{\code{yac}}{Yards after catch (YAC) from PFR}
#'  \item{\code{yac_r}}{Yards after catch (YAC) per reception from PFR}
#'  \item{\code{adot}}{
#'      Average depth of target (ADOT) when targeted, whether completed or not,
#'      from PFR
#'      }
#'  \item{\code{receiving_epa}}{Expected points added on receptions
#'    from PBP data
#'    }
#'  \item{\code{racr}}{
#'    Receiving Air Conversion Ratio.
#'    RACR = `receiving_yards` / `receiving_air_yards` from PBP
#'    }
#'  \item{\code{wopr}}{
#'    Weighted Opportunity Rating.
#'    WOPR = 1.5 x `target_share` + 0.7 x `air_yards_share` from PBP data
#'    }
#'  \item{\code{avg_cushion}}{Average cushion (CUSH) from NGS}
#'  \item{\code{avg_separation}}{Average separation (SEP) from NGS}
#'  \item{\code{target_share}}{
#'    Share of targets of player compared to all team targets from PBP
#'    }
#'  \item{\code{tgt_pct}}{Share of targets percentage from PBP}
#'  \item{\code{ybc}}{
#'      Total yards passes traveled in the air before being caught or yards
#'      before catch (YBC) from PFR}
#'  \item{\code{ybc_r}}{Yards before catch per reception from PFR}
#'  \item{\code{receiving_air}}{
#'    Receiving air yards including incomplete passes from PBP data
#'    }
#'  \item{\code{avg_intended_air_yards}}{Average targeted air yards (TAY) from NGS}
#'  \item{\code{percent_share_of_intended_air_yards}}{
#'      Average % share of team's air yards (TAY%) from NGS
#'      }
#'  \item{\code{air_yards_share}}{
#'    Share of `receiving_air_yards` of the player to all team `air_yards`
#'    from PBP data
#'    }
#'  \item{\code{x1d_rec}}{
#'    Number of first downs gained on a reception from PBP data
#'    }
#'  \item{\code{brk_tkl}}{Number of broken tackles from PFR}
#'  \item{\code{rec_br}}{Receptions per broken tackle from PFR}
#'  \item{\code{drop}}{Number of dropped passes from PFR}
#'  \item{\code{drop_percent}}{Dropped pass percentage when targeted from PFR}
#'  \item{\code{catch_percentage}}{Average catch percentage from NGS}
#'  \item{\code{receiving_fumbles}}{Number of fumbles after a pass reception
#'    from PBP data
#'    }
#'  \item{\code{receiving_fumbles_lost}}{
#'    Number of fumbles lost after a pass reception from PBP data
#'    }
#'  \item{\code{receiving_2pt_conversions}}{
#'    Two-point conversion receptions from PBP data
#'    }
#'  \item{\code{int_tgt}}{Interceptions on passes where targetedfrom PFR}
#'  \item{\code{rat}}{Passer rating on passes when targeted from PFR}
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
#'    a lateral reception don't count as a carry from PBP data
#'    }
#'  \item{\code{rushing_yards}}{
#'    Yards gained when rushing including scrambles and kneel downs. Also
#'    includes yards gained after obtaining a lateral on a play that started
#'    with a rushing attempt from PBP data
#'    }
#'  \item{\code{rushing_tds}}{
#'    The number of rushing touchdowns (incl. scrambles). Also includes
#'    touchdowns after obtaining a lateral on a play that started with a
#'    rushing attempt from PBP data
#'    }
#'  \item{\code{rushing_fumbles}}{Number of rushes with a fumble from PBP data}
#'  \item{\code{rushing_fumbles_lost}}{
#'    Number of rushes with a lost fumble from PBP data
#'    }
#'  \item{\code{rushing_first_downs}}{
#'    Number of rushing first downs from PBP data
#'    }
#'  \item{\code{rushing_epa}}{
#'    Expected points added (EPA) on rush attempts including scrambles and
#'    kneel downs from PBP data
#'    }
#'  \item{\code{rushing_2pt_conversions}}{
#'    Two-point conversion rushes from PBP data
#'    }
#'  \item{\code{pfr_id}}{
#'      Pro Football Reference player ID (e.g., JackLa00)
#'      }
#'  }
#'
#' @export
get_te_combined_stats_season <- function(pbp_db = NULL,
                                         pbp_db_tbl = NULL,
                                         seasons = NULL) {

  # Combined stats require PBP/NGS/PFR
  # PFR only has data from 2018 and later (1999 PBP, 2016 NGS)
  # Check if seasons is 2018 or later and integer
  nuclearff::validate_pfr_season(seasons)

  # Pull TE PBP data
  te_pbp <- nuclearff::get_te_pbp_stats(
    pbp_db,
    pbp_db_tbl,
    seasons,
    week_min = 1 # Pulling entire season starting from week 1
  ) %>%
    # Clean up player names in defined player column
    nuclearff::replace_player_names(player_col = "player_display_name")

  # Pull TE roster information and player IDs
  te_data <- nuclearff::get_player_data(seasons = 2024,
                             position = "WR"
  ) %>%
    dplyr::select(
      player_id,
      player_display_name = player,
      position,
      team,
      pfr_id
    ) %>%
    # Clean up player names in defined player column
    nuclearff::replace_player_names(player_col = "player_display_name")

  # Combine PBP data with roster information and IDs
  te_stats <- te_pbp %>%
    left_join(te_data,
              by = c("player_id",
                     "player_display_name",
                     "position",
                     "team")
    )

  # Pull PFR data
  te_pfr <- nuclearff::get_te_pfr_advstats_season(seasons = 2024) %>%
    # Clean up player names in defined player column
    nuclearff::replace_player_names(player_col = "player_display_name")

  # Common variables to combine from PBP and PFR
  common_vars_pbp_pfr <- c("player_display_name",
                           "position",
                           "games" = "g",
                           "targets" = "tgt",
                           "receptions" = "rec",
                           "receiving_yards" = "yds",
                           "receiving_tds" = "td",
                           "receiving_first_downs" = "x1d",
                           "receiving_yards_after_catch" = "yac",
                           "pfr_id" = "pfr_player_id"
  )

  # Combine PBP with PFR
  te_pbp_pfr <- te_stats %>%
    left_join(te_pfr, by = common_vars_pbp_pfr)

  # Pull NGS data
  te_ngs <- nuclearff::get_te_ngs_advstats_season(seasons = 2024) %>%
    # Clean up player names in defined player column
    nuclearff::replace_player_names(player_col = "player_display_name")

  # Common variables to combine from PBP/PFR with NGS
  common_vars_pbp_pfr_ngs <- c("player_id",
                               "player_display_name",
                               "player_name",
                               "team",
                               "targets",
                               "receptions",
                               "receiving_yards" = "yards",
                               "receiving_tds" = "rec_touchdowns"
  )

  # Combine WR play-by-play data, PFR data, and NGS data
  # NOTE: adot and rat 25 targets/16 game pace required for leaderboards
  te_combined_stats <- te_pbp_pfr %>%
    left_join(te_ngs, by = common_vars_pbp_pfr_ngs) %>%
    dplyr::select(
      player_id, # pbp
      player_display_name, # pbp
      player_name, # pbp
      pos = position, # pbp
      tm = team, # pbp
      g = games, # pbp
      # PBP
      tgt = targets, # pbp
      rec = receptions, # pbp
      rec_yds = receiving_yards, # pbp
      rec_td = receiving_tds, # pbp
      avg_yac, # NGS
      avg_expected_yac, # NGS
      avg_yac_above_expectation, # NGS
      yac = receiving_yards_after_catch, # pbp yards after catch (YAC)
      yac_r, # PFR - Yards after catch per reception (YAC/R)
      adot, # PFR Average depth of target (ADOT)
      receiving_epa, # pbp Receiving expected points added (EPA)
      racr, # PBP
      wopr, # PBP
      avg_cushion, # NGS
      avg_separation, # NGS
      target_share, # pbp Target share fraction for team
      tgt_pct, # pbp Target share percentage for team (%TM)
      ybc, # pfr Total yards passes traveled in air before caught (YBC)
      ybc_r, # pfr Total yards passes traveled in air before caught per rec (YBC/R)
      rec_air = receiving_air_yards, # PBP
      avg_intended_air_yards, # NGS
      percent_share_of_intended_air_yards, # NGS
      air_yards_share, # pbp
      x1d = receiving_first_downs, # pbp Receiving first downs (x1D)
      brk_tkl, # pfr Number of broken tackles on receptions
      rec_br, # pfr Receptions per broken tackle
      drop, # pfr Dropped passes (DROP)
      drop_percent, # pfr Percent of dropped passes (DROP%)
      catch_percentage, # NGS
      receiving_fumbles, # pbp
      receiving_fumbles_lost, # pbp
      receiving_2pt_conversions, # pbp
      int_tgt, # pfr Number of targets where the pass was intercepted
      rat, # pfr Passer rating on passes when targeted.
      # Fantasy Points
      fpts_std_4pt_td,
      ppg_std_4pt_td,
      fpts_half_ppr_4pt_td,
      ppg_half_ppr_4pt_td,
      fpts_ppr_4pt_td,
      ppg_ppr_4pt_td,
      fpts_std_6pt_td,
      ppg_std_6pt_td,
      fpts_half_ppr_6pt_td,
      ppg_half_ppr_6pt_td,
      fpts_ppr_6pt_td,
      ppg_ppr_6pt_td,
      # PBP Rush
      carries,
      rushing_yards,
      rushing_tds,
      rushing_fumbles,
      rushing_fumbles_lost,
      rushing_first_downs,
      rushing_epa,
      rushing_2pt_conversions,
      # PFR
      pfr_id
    )

  return(te_combined_stats)
}
