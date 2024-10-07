################################################################################
# Author: Nolan MacDonald
# Purpose:
#   Obtain Positional Cumulative Season Advanced Statistics from NFL
#   Next Gen Stats (NGS).
# Code Style Guide:
#   styler::tidyverse_style(), lintr::use_lintr(type = "tidyverse")
################################################################################

get_qb_combined_stats_season <- function(pbp_db = NULL,
                                         pbp_db_tbl = NULL,
                                         seasons = NULL) {

  # Combined stats require PBP/NGS/PFR
  # PFR only has data from 2018 and later (1999 PBP, 2016 NGS)
  # Check if seasons is 2018 or later and integer
  nuclearff::validate_pfr_season(seasons)

  # Pull RB PBP data
  qb_pbp <- get_qb_pbp_stats(
    pbp_db = "./data/pbp_db",
    pbp_db_tbl = "nflfastR_pbp",
    season = 2024,
    week_min = 1
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
get_rb_combined_stats_season <- function(pbp_db = NULL,
                                         pbp_db_tbl = NULL,
                                         seasons = NULL) {

  # Combined stats require PBP/NGS/PFR
  # PFR only has data from 2018 and later (1999 PBP, 2016 NGS)
  # Check if seasons is 2018 or later and integer
  nuclearff::validate_pfr_season(seasons)

  # Pull RB PBP data
  rb_pbp <- nuclearff::get_rb_pbp_stats(
    pbp_db = "./data/pbp_db",
    pbp_db_tbl = "nflfastR_pbp",
    season = 2024,
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
    left_join(rb_ngs, by = common_vars_pbp_pfr_ngs) # %>%
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
    avg_rush_yards,
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
    yac = receiving_yards_after_catch, # yards after catch (YAC)
    yac_r, # Yards after catch per reception (YAC/R) PFR
    x1d = receiving_first_downs, # Receiving first downs (x1D)
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


get_wr_combined_stats_season <- function(pbp_db = NULL,
                                         pbp_db_tbl = NULL,
                                         seasons = NULL) {

  # Combined stats require PBP/NGS/PFR
  # PFR only has data from 2018 and later (1999 PBP, 2016 NGS)
  # Check if seasons is 2018 or later and integer
  nuclearff::validate_pfr_season(seasons)

  # Pull WR PBP data
  wr_pbp <- nuclearff::get_wr_pbp_stats(
    pbp_db = "./data/pbp_db",
    pbp_db_tbl = "nflfastR_pbp",
    season = 2024,
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

get_te_combined_stats_season <- function(pbp_db = NULL,
                                         pbp_db_tbl = NULL,
                                         seasons = NULL) {

  # Combined stats require PBP/NGS/PFR
  # PFR only has data from 2018 and later (1999 PBP, 2016 NGS)
  # Check if seasons is 2018 or later and integer
  nuclearff::validate_pfr_season(seasons)

  # Pull TE PBP data
  te_pbp <- nuclearff::get_te_pbp_stats(
    pbp_db = "./data/pbp_db",
    pbp_db_tbl = "nflfastR_pbp",
    season = 2024,
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
    left_join(wr_data,
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
