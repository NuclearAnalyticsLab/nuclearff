################################################################################
# Author: Nolan MacDonald
# Purpose:
#   Obtain Positional Cumulative Season Advanced Statistics from NFL
#   Next Gen Stats (NGS).
# Code Style Guide:
#   styler::tidyverse_style(), lintr::use_lintr(type = "tidyverse")
################################################################################

get_wr_combined_stats_season <- function(pbp_db = NULL,
                                         pbp_db_tbl = NULL,
                                         seasons = NULL) {
  wr_pbp <- nuclearff::get_wr_pbp_stats(
    pbp_db,
    pbp_db_tbl,
    season,
    week_min = 1
  )

  wr_data <- nuclearff::get_player_data(seasons,
    position = "WR"
  ) %>%
    dplyr::select(
      player_id,
      player_display_name = player,
      position,
      team,
      pfr_id
    )

  wr_stats <- wr_pbp %>%
    dplyr::left_join(wr_data,
      by = c(
        "player_id",
        "player_display_name",
        "position",
        "team"
      )
    )

  # Pull WR Pro Football Reference (PFR) advanced stats
  wr_pfr <- nuclearff::get_wr_pfr_advstats_season(seasons)

  # Common between wr_stats and wr_pfr
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

  wr_pbp_pfr <- wr_stats %>%
    dplyr::left_join(wr_pfr, by = common_vars_pbp_pfr)

  # Pull WR Nexg Gen Stats (NGS)
  wr_ngs <- nuclearff::get_wr_ngs_advstats_season(seasons)

  # Common between wr_pbp_pfr and wr_ngs
  common_vars_pbp_pfr_ngs <- c("player_id",
    "player_display_name",
    "player_name",
    "team",
    "targets",
    "receptions",
    "receiving_yards" = "yards",
    "receiving_tds" = "rec_touchdowns"
  )

  wr_combined <- wr_pbp_pfr %>%
    dplyr::left_join(wr_ngs, by = common_vars_pbp_pfr_ngs) %>%
    dplyr::select(
      player_id,
      player_display_name,
      player_name,
      pos = position,
      tm = team,
      g = games,
      # PBP
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
      # NGS
      avg_cushion,
      avg_separation,
      avg_intended_air_yards,
      percent_share_of_intended_air_yards,
      catch_percentage,
      avg_yac,
      avg_expected_yac,
      avg_yac_above_expectation,
      # PFR
      ybc, # Total yards passes traveled in air before caught (YBC)
      ybc_r, # Total yards passes traveled in air before caught per rec (YBC/R)
      # yac_r,
      adot, # Average depth of target (ADOT) 25 targets/16 game pace required for leaderboards.
      brk_tkl, # Number of broken tackles on receptions
      rec_br, # Receptions per broken tackle
      drop, # Dropped passes (DROP)
      drop_percent, # Percent of dropped passes (DROP%)
      int_tgt, # Number of targets where the pass was intercepted
      rat, # Passer rating on passes when targeted. 25 targets/16 game pace required for leaderboards.
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

  return(wr_combined)
}
