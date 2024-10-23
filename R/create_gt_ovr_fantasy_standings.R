################################################################################
# Author: Nolan MacDonald
# Purpose:
#   Use gt Tables to create figures for fantasy football positional standings
# Code Style Guide:
#   styler::tidyverse_style(), lintr::use_lintr(type = "tidyverse")
################################################################################

#' QB Overall Fantasy Football Standings Table
#'
#' @description
#' Create a table using `gt` of overall player standings in fantasy football.
#' The table is colored by fantasy points per game (PPG) and ordered by total
#' fantasy points.
#'
#' @details
#' Using `nflfastR` play-by-play data, stats for a user-defined season can
#' be obtained to create a table with overall standings for a season.
#' The function automatically includes the entire season, defined by year as
#' an integer. The table is colored by fantasy points per game (PPG) using the
#' `gtExtras` hulk theme and ordered by total fantasy points.
#' The table is saved as a `.png` figure.
#'
#' @param pbp_db Path to play-by-play database (str)
#' @param pbp_db_tbl Table name stored in the play-by-play database (str)
#' @param scoring Fantasy scoring format (str). Can be defined as standard
#'  scoring, half PPR scoring, full PPR scoring, 4PT PASS TD or 6PT PASS TD by
#'  using: `"std_4pt_td"`, `"half_ppr_4pt_td"`, `"ppr_4pt_td"`,
#'  `"std_6pt_td"`, `"half_ppr_6pt_td"`, `"ppr_6pt_td"`
#' @param seasons NFL season or year (int) such as `seasons = 2024`.
#' @param num_players Number of players to list in table (int)
#' @param output_dir Directory to save the table
#' @param file_name File name to save table
#'
#' @return Rendered table using `gt` and a saved `.png` if `output_dir` and
#'  `file_name` are defined
#'
#' @seealso \code{\link[nuclearff]{nuclearff::get_qb_pbp_stats}}
#'  QB cumulative season stats obtained from play-by-play data
#' @seealso \code{\link[nuclearff]{nuclearff::get_player_data}}
#'  Get player data and IDs to merge dataframes
#'
#' @author Nolan MacDonald
#'
#' @export
table_ovr_qb_fantasy <- function(num_players = NULL,
                                 scoring = NULL,
                                 seasons = NULL,
                                 pbp_db = NULL,
                                 pbp_db_tbl = NULL,
                                 file_name = NULL,
                                 output_dir = NULL
) {

  qb_pbp_stats <- nuclearff::get_qb_pbp_stats(pbp_db,
                                              pbp_db_tbl,
                                              seasons,
                                              week_min = 1
  ) %>%
    # Clean up player names in defined player column
    nuclearff::replace_player_names(player_col = "player_display_name")

  # Define the columns to select based on `scoring`
  scoring_cols <- nuclearff::get_scoring_columns(scoring)

  # Format data to include in table
  qb_data_tbl <- qb_pbp_stats %>%
    dplyr::select(
      player_id, player_name, team, games,
      # Rename default scoring column names
      ppg = dplyr::all_of(scoring_cols[1]),
      fpts = dplyr::all_of(scoring_cols[2]),
      attempts, completions, cmp_pct, passing_yards, passing_tds, interceptions,
      carries, rushing_yards, rushing_tds
    ) %>%
    mutate(
      # Round completion % to 1 sig fig
      cmp_pct = round(cmp_pct, 1),
    ) %>%
    # Arrange by total fantasy points descending
    dplyr::arrange(dplyr::desc(fpts)) %>%
    # Only keep the top 16 WR
    utils::head(num_players) %>%
    # mutate(OVR = 1:16) %>%    # Add the OVERALL column
    mutate(OVR = paste0("QB", 1:num_players)) %>%  # Combine "WR" with the numbers 1 to 16
    select(OVR, everything()) # Move OVERALL to the first column

  # Get the max week - current week
  week_number <- max(qb_pbp_stats$games)
  # Get Scoring Format for subtitle
  format <- nuclearff::get_scoring_format(scoring,
                                          position = "QB")

  # Create table
  qb_tbl <- qb_data_tbl %>%
    gt() %>%
    # Get player headshots
    nflplotR::gt_nfl_headshots(columns = gt::everything(), height = 35) %>%
    # Get team logo
    nflplotR::gt_nfl_logos(columns = gt::starts_with("team")) %>%
    # Rename the columns
    cols_label(
      player_id = "",
      player_name = "PLAYER",
      team = "TEAM",
      games = "G",
      ppg = "PPG",
      fpts = "FPTS",
      attempts = "ATT",
      completions = "CMP",
      cmp_pct = "CMP%",
      passing_yards = "PASS YDS",
      passing_tds = "PASS TD",
      interceptions = "INT",
      carries = "CAR",
      rushing_yards = "RUSH YDS",
      rushing_tds = "RUSH TD"
    ) %>%
    # Align column TEAM to center
    cols_align(
      align = "center",
      columns = c(team, games,
                  ppg, fpts,
                  completions, attempts, cmp_pct,
                  passing_yards, passing_tds, interceptions,
                  carries, rushing_yards, rushing_tds
      )
    ) %>%
    # Align the "OVR" column values to the right
    cols_align(
      align = "right",
      columns = c(OVR)
    ) %>%
    # Make OVR column bold
    tab_style(
      style = cell_text(weight = "bold"),  # Make the text bold
      # Specify bold for all column titles and OVR values
      locations = list(
        cells_body(columns = c(OVR)),
        cells_column_labels(columns = c(OVR, player_name, team, games,
                                        ppg, fpts,
                                        completions, attempts, cmp_pct,
                                        passing_yards, passing_tds,
                                        interceptions,
                                        carries, rushing_yards, rushing_tds))
      )
    ) %>%
    # Add hulk color theme
    # trim = TRUE makes colors light, less pronounced
    gt_hulk_col_numeric(ppg, trim = FALSE) %>%
    # Footer
    gt::tab_source_note(
    md(glue("<div style='display: flex; align-items: center; margin: 0; padding: 0; line-height: 0;'>
    <img src='https://raw.githubusercontent.com/NuclearAnalyticsLab/nuclearff/main/inst/logos/png/logo-500x500.png' style='height:30px; margin: 0; padding: 0; display: inline-block;'>
    <span style='margin-left: 5px; padding: 0; display: inline-block; vertical-align: middle;'>| Table: @nuclearffnolan | Data: @nflfastR |</span>
    </div>"))
  ) %>%
    # Title
    gt::tab_header(
    title = md(glue("
  <div style='display: flex; justify-content: center; align-items: center;'>
    <div style='text-align: center;'>
    <strong>QB OVERALL STANDINGS WEEK {week_number}</strong>
    <div style='font-size: 14px;'>{seasons} Fantasy PPG {format}</div>
    </div>
    <div style='position: absolute; right: 10px;'>
    <img src='https://raw.githubusercontent.com/NuclearAnalyticsLab/nuclearff/main/inst/logos/svg/logos/nuclearff-pink-blue-alt.svg' 
       style='height:80px;'>
    </div>
  </div>
  ")
      )
    ) %>%
    # Add padding between title and subtitle
    gt::tab_options(
      heading.padding = px(0),  # Adjust the padding value as needed
      table.border.top.style = "hidden" # Hide top border above title
    )

  # Fastest way to show a table with nflplotr
  nflplotR::gt_render_image(qb_tbl)

  nuclearff::save_gt_table(qb_tbl, file_name, output_dir)
}

#' RB Overall Fantasy Football Standings Table
#'
#' @description
#' Create a table using `gt` of overall player standings in fantasy football.
#' The table is colored by fantasy points per game (PPG) and ordered by total
#' fantasy points.
#'
#' @details
#' Using `nflfastR` play-by-play data, stats for a user-defined season can
#' be obtained to create a table with overall standings for a season.
#' The function automatically includes the entire season, defined by year as
#' an integer. The table is colored by fantasy points per game (PPG) using the
#' `gtExtras` hulk theme and ordered by total fantasy points.
#' The table is saved as a `.png` figure.
#'
#' @param pbp_db Path to play-by-play database (str)
#' @param pbp_db_tbl Table name stored in the play-by-play database (str)
#' @param scoring Fantasy scoring format (str). Can be defined as standard
#'  scoring, half PPR scoring, full PPR scoring, 4PT PASS TD or 6PT PASS TD by
#'  using: `"std_4pt_td"`, `"half_ppr_4pt_td"`, `"ppr_4pt_td"`,
#'  `"std_6pt_td"`, `"half_ppr_6pt_td"`, `"ppr_6pt_td"`
#' @param seasons NFL season or year (int) such as `seasons = 2024`.
#' @param num_players Number of players to list in table (int)
#' @param output_dir Directory to save the table
#' @param file_name File name to save table
#'
#' @return Rendered table using `gt` and a saved `.png` if `output_dir` and
#'  `file_name` are defined
#'
#' @seealso \code{\link[nuclearff]{nuclearff::get_rb_pbp_stats}}
#'  RB cumulative season stats obtained from play-by-play data
#' @seealso \code{\link[nuclearff]{nuclearff::get_player_data}}
#'  Get player data and IDs to merge dataframes
#' @seealso \code{\link[nuclearff]{nuclearff::get_snap_share}}
#'  Pull player snap shares
#'
#' @author Nolan MacDonald
#'
#' @export
table_ovr_rb_fantasy <- function(num_players = NULL,
                                 scoring = NULL,
                                 seasons = NULL,
                                 pbp_db = NULL,
                                 pbp_db_tbl = NULL,
                                 file_name = NULL,
                                 output_dir = NULL
) {

  rb_pbp_stats <- nuclearff::get_rb_pbp_stats(pbp_db,
                                              pbp_db_tbl,
                                              seasons,
                                              week_min = 1
  ) %>%
    # Clean up player names in defined player column
    nuclearff::replace_player_names(player_col = "player_display_name")

  # Pull WR roster information and player IDs
  rb_data <- nuclearff::get_player_data(seasons,
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
  rb_stats <- rb_pbp_stats %>%
    left_join(rb_data,
              by = c("player_id",
                     "player_display_name",
                     "position",
                     "team")
    )

  # Add snap share to data
  snap_pct <- nuclearff::get_snap_share(season = seasons, pos = "RB") %>%
    # Clean up player names in defined player column
    nuclearff::replace_player_names(player_col = "player")

  rb <- left_join(rb_stats, snap_pct,
                  by = c("player_display_name" = "player",
                         "pfr_id" = "pfr_player_id",
                         "position",
                         "team"))


  # Define the columns to select based on `scoring`
  scoring_cols <- nuclearff::get_scoring_columns(scoring)

  # Format data to include in table
  rb_data_tbl <- rb %>%
    dplyr::select(
      player_id, player_name, team, games,
      # Rename default scoring column names
      ppg = dplyr::all_of(scoring_cols[1]),
      fpts = dplyr::all_of(scoring_cols[2]),
      snap_pct, tgt_pct, carries, rushing_yards, rushing_tds,
      receptions, receiving_yards, receiving_tds
    ) %>%
    mutate(
      # Make snap % and tgt_pct to 1 decimal
      snap_pct = round(snap_pct, 1),
      tgt_pct = round(tgt_pct, 1)
    ) %>%
    # Arrange by total fantasy points descending
    dplyr::arrange(dplyr::desc(fpts)) %>%
    # Only keep the top 16 WR
    utils::head(num_players) %>%
    # mutate(OVR = 1:16) %>%    # Add the OVERALL column
    mutate(OVR = paste0("RB", 1:num_players)) %>%  # Combine "WR" with the numbers 1 to 16
    select(OVR, everything()) # Move OVERALL to the first column

  # Get the max week - current week
  week_number <- max(rb_pbp_stats$games)
  # Get Scoring Format for subtitle
  format <- nuclearff::get_scoring_format(scoring, position = "RB")

  # Create table
  rb_tbl <- rb_data_tbl %>%
    gt() %>%
    # Get player headshots
    nflplotR::gt_nfl_headshots(columns = gt::everything(), height = 35) %>%
    # Get team logo
    nflplotR::gt_nfl_logos(columns = gt::starts_with("team")) %>%
    # Rename the columns
    cols_label(
      player_id = "",
      player_name = "PLAYER",
      team = "TEAM",
      games = "G",
      ppg = "PPG",
      fpts = "FPTS",
      snap_pct = "SNP%",
      tgt_pct = "TGT%",
      carries = "CAR",
      rushing_yards = "RUSH YDS",
      rushing_tds ="RUSH TD",
      receptions = "REC",
      receiving_yards = "REC YDS",
      receiving_tds = "REC TD"
    ) %>%
    # Align column TEAM to center
    cols_align(
      align = "center",
      columns = c(team, games,
                  ppg, fpts,
                  snap_pct, tgt_pct,
                  carries, rushing_yards, rushing_tds,
                  receptions, receiving_yards, receiving_tds
      )
    ) %>%
    # Align the "OVR" column values to the right
    cols_align(
      align = "right",
      columns = c(OVR)
    ) %>%
    # Make OVR column bold
    tab_style(
      style = cell_text(weight = "bold"),  # Make the text bold
      # Specify bold for all column titles and OVR values
      locations = list(
        cells_body(columns = c(OVR)),
        cells_column_labels(columns = c(OVR, player_name, team, games,
                                        ppg, fpts,
                                        snap_pct, tgt_pct,
                                        carries, rushing_yards, rushing_tds,
                                        receptions, receiving_yards,
                                        receiving_tds))
      )
    ) %>%
    # Add hulk color theme
    # trim = TRUE makes colors light, less pronounced
    gtExtras::gt_hulk_col_numeric(ppg, trim = FALSE) %>%
    # Footer
    gt::tab_source_note(
      md(glue("<div style='display: flex; align-items: center; margin: 0; padding: 0; line-height: 0;'>
      <img src='https://raw.githubusercontent.com/NuclearAnalyticsLab/nuclearff/main/inst/logos/png/logo-500x500.png' style='height:30px; margin: 0; padding: 0; display: inline-block;'>
      <span style='margin-left: 5px; padding: 0; display: inline-block; vertical-align: middle;'>| Table: @nuclearffnolan | Data: @nflfastR |</span>
      </div>"))
    ) %>%
    gt::tab_header(
      title = md(glue("
    <div style='display: flex; justify-content: center; align-items: center;'>
      <div style='text-align: center;'>
      <strong>RB OVERALL STANDINGS WEEK {week_number}</strong>
      <div style='font-size: 14px;'>{seasons} Fantasy PPG {format}</div>
      </div>
      <div style='position: absolute; right: 25px;'>
      <img src='https://raw.githubusercontent.com/NuclearAnalyticsLab/nuclearff/main/inst/logos/svg/logos/nuclearff-pink-blue-alt.svg' style='height:80px;'>
      </div>
    </div>
    "))
    ) %>%
    # Add padding between title and subtitle
    gt::tab_options(
      heading.padding = px(0),  # Adjust the padding value as needed
      table.border.top.style = "hidden" # Hide top border above title
    )

  # Fastest way to show a table with nflplotr
  nflplotR::gt_render_image(rb_tbl)

  nuclearff::save_gt_table(rb_tbl, file_name, output_dir)
}


#' WR Overall Fantasy Football Standings Table
#'
#' @description
#' Create a table using `gt` of overall player standings in fantasy football.
#' The table is colored by fantasy points per game (PPG) and ordered by total
#' fantasy points.
#'
#' @details
#' Using `nflfastR` play-by-play data, stats for a user-defined season can
#' be obtained to create a table with overall standings for a season.
#' The function automatically includes the entire season, defined by year as
#' an integer. The table is colored by fantasy points per game (PPG) using the
#' `gtExtras` hulk theme and ordered by total fantasy points.
#' The table is saved as a `.png` figure.
#'
#' @param pbp_db Path to play-by-play database (str)
#' @param pbp_db_tbl Table name stored in the play-by-play database (str)
#' @param scoring Fantasy scoring format (str). Can be defined as standard
#'  scoring, half PPR scoring, full PPR scoring, 4PT PASS TD or 6PT PASS TD by
#'  using: `"std_4pt_td"`, `"half_ppr_4pt_td"`, `"ppr_4pt_td"`,
#'  `"std_6pt_td"`, `"half_ppr_6pt_td"`, `"ppr_6pt_td"`
#' @param seasons NFL season or year (int) such as `seasons = 2024`.
#' @param num_players Number of players to list in table (int)
#' @param output_dir Directory to save the table
#' @param file_name File name to save table
#'
#' @return Rendered table using `gt` and a saved `.png` if `output_dir` and
#'  `file_name` are defined
#'
#' @seealso \code{\link[nuclearff]{nuclearff::get_wr_pbp_stats}}
#'  WR cumulative season stats obtained from play-by-play data
#' @seealso \code{\link[nuclearff]{nuclearff::get_player_data}}
#'  Get player data and IDs to merge dataframes
#' @seealso \code{\link[nuclearff]{nuclearff::get_snap_share}}
#'  Pull player snap shares
#'
#' @author Nolan MacDonald
#'
#' @export
table_ovr_wr_fantasy <- function(num_players = NULL,
                                 scoring = NULL,
                                 seasons = NULL,
                                 pbp_db = NULL,
                                 pbp_db_tbl = NULL,
                                 file_name = NULL,
                                 output_dir = NULL
                                   ) {

  wr_pbp_stats <- nuclearff::get_wr_pbp_stats(pbp_db,
                                              pbp_db_tbl,
                                              seasons,
                                              week_min = 1
  ) %>%
    # Clean up player names in defined player column
    nuclearff::replace_player_names(player_col = "player_display_name")

  # Pull WR roster information and player IDs
  wr_data <- nuclearff::get_player_data(seasons,
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
  wr_stats <- wr_pbp_stats %>%
    left_join(wr_data,
              by = c("player_id",
                     "player_display_name",
                     "position",
                     "team")
    )

  # Add snap share to data
  snap_pct <- nuclearff::get_snap_share(season = seasons, pos = "WR") %>%
    # Clean up player names in defined player column
    nuclearff::replace_player_names(player_col = "player")

  wr <- left_join(wr_stats, snap_pct,
                  by = c("player_display_name" = "player",
                         "pfr_id" = "pfr_player_id",
                         "position",
                         "team"))


  # Define the columns to select based on `scoring`
  scoring_cols <- nuclearff::get_scoring_columns(scoring)

  # Format data to include in table
  wr_data_tbl <- wr %>%
    dplyr::select(
      player_id, player_name, team, games,
      # Rename default scoring column names
      ppg = dplyr::all_of(scoring_cols[1]),
      fpts = dplyr::all_of(scoring_cols[2]),
      snap_pct, tgt_pct, targets, receptions, receiving_yards, receiving_tds
    ) %>%
    mutate(
      # Make snap % and tgt_pct to 1 decimal
      snap_pct = round(snap_pct, 1),
      tgt_pct = round(tgt_pct, 1)
    ) %>%
    # Arrange by total fantasy points descending
    dplyr::arrange(dplyr::desc(fpts)) %>%
    # Only keep the top 16 WR
    utils::head(num_players) %>%
    # mutate(OVR = 1:16) %>%    # Add the OVERALL column
    mutate(OVR = paste0("WR", 1:num_players)) %>%  # Combine "WR" with the numbers 1 to 16
    select(OVR, everything()) # Move OVERALL to the first column

  # Get the max week - current week
  week_number <- max(wr_pbp_stats$games)
  # Get Scoring Format for subtitle
  format <- nuclearff::get_scoring_format(scoring, position = "WR")

  # Create table
  wr_tbl <- wr_data_tbl %>%
    gt() %>%
    # Get player headshots
    nflplotR::gt_nfl_headshots(columns = gt::everything(), height = 35) %>%
    # Get team logo
    nflplotR::gt_nfl_logos(columns = gt::starts_with("team")) %>%
    # Rename the columns
    cols_label(
      player_id = "",
      player_name = "PLAYER",
      team = "TEAM",
      games = "G",
      ppg = "PPG",
      fpts = "FPTS",
      snap_pct = "SNP%",
      tgt_pct = "TGT%",
      targets = "TAR",
      receptions = "REC",
      receiving_yards = "REC YDS",
      receiving_tds = "REC TD"
    ) %>%
    # Align column TEAM to center
    cols_align(
      align = "center",
      columns = c(team, games,
                  ppg, fpts,
                  snap_pct, tgt_pct,
                  targets, receptions, receiving_yards, receiving_tds)
    ) %>%
    # Align the "OVR" column values to the right
    cols_align(
      align = "right",
      columns = c(OVR)
    ) %>%
    # Make OVR column bold
    tab_style(
      style = cell_text(weight = "bold"),  # Make the text bold
      # Specify bold for all column titles and OVR values
      locations = list(
        cells_body(columns = c(OVR)),
        cells_column_labels(columns = c(OVR, player_name, team, games,
                                        ppg, fpts,
                                        snap_pct, tgt_pct,
                                        targets, receptions,
                                        receiving_yards, receiving_tds))
      )
    ) %>%
    # Add hulk color theme
    # trim = TRUE makes colors light, less pronounced
    gt_hulk_col_numeric(ppg, trim = FALSE) %>%
    # Footer
    gt::tab_source_note(
      md(glue("<div style='display: flex; align-items: center; margin: 0; padding: 0; line-height: 0;'>
      <img src='https://raw.githubusercontent.com/NuclearAnalyticsLab/nuclearff/main/inst/logos/png/logo-500x500.png' style='height:30px; margin: 0; padding: 0; display: inline-block;'>
      <span style='margin-left: 5px; padding: 0; display: inline-block; vertical-align: middle;'>| Table: @nuclearffnolan | Data: @nflfastR |</span>
      </div>"))
    ) %>%
    gt::tab_header(
      title = md(glue("
    <div style='display: flex; justify-content: center; align-items: center;'>
      <div style='text-align: center;'>
      <strong>WR OVERALL STANDINGS WEEK {week_number}</strong>
      <div style='font-size: 14px;'>{seasons} Fantasy PPG {format}</div>
      </div>
      <div style='position: absolute; right: 120px;'>
      <img src='https://raw.githubusercontent.com/NuclearAnalyticsLab/nuclearff/main/inst/logos/svg/logos/nuclearff-pink-blue-alt.svg' style='height:80px;'>
      </div>
    </div>
    "))
    ) %>%
    # Add padding between title and subtitle
    gt::tab_options(
      heading.padding = px(0),  # Adjust the padding value as needed
      table.border.top.style = "hidden" # Hide top border above title
      # table.border.bottom.style = "hidden" # Hide bottom footnote border
    )

  # Fastest way to show a table with nflplotr
  nflplotR::gt_render_image(wr_tbl)

  nuclearff::save_gt_table(wr_tbl, file_name, output_dir)
}

#' TE Overall Fantasy Football Standings Table
#'
#' @description
#' Create a table using `gt` of overall player standings in fantasy football.
#' The table is colored by fantasy points per game (PPG) and ordered by total
#' fantasy points.
#'
#' @details
#' Using `nflfastR` play-by-play data, stats for a user-defined season can
#' be obtained to create a table with overall standings for a season.
#' The function automatically includes the entire season, defined by year as
#' an integer. The table is colored by fantasy points per game (PPG) using the
#' `gtExtras` hulk theme and ordered by total fantasy points.
#' The table is saved as a `.png` figure.
#'
#' @param pbp_db Path to play-by-play database (str)
#' @param pbp_db_tbl Table name stored in the play-by-play database (str)
#' @param scoring Fantasy scoring format (str). Can be defined as standard
#'  scoring, half PPR scoring, full PPR scoring, 4PT PASS TD or 6PT PASS TD by
#'  using: `"std_4pt_td"`, `"half_ppr_4pt_td"`, `"ppr_4pt_td"`,
#'  `"std_6pt_td"`, `"half_ppr_6pt_td"`, `"ppr_6pt_td"`
#' @param seasons NFL season or year (int) such as `seasons = 2024`.
#' @param num_players Number of players to list in table (int)
#' @param output_dir Directory to save the table
#' @param file_name File name to save table
#'
#' @return Rendered table using `gt` and a saved `.png` if `output_dir` and
#'  `file_name` are defined
#'
#' @seealso \code{\link[nuclearff]{nuclearff::get_te_pbp_stats}}
#'  TE cumulative season stats obtained from play-by-play data
#' @seealso \code{\link[nuclearff]{nuclearff::get_player_data}}
#'  Get player data and IDs to merge dataframes
#' @seealso \code{\link[nuclearff]{nuclearff::get_snap_share}}
#'  Pull player snap shares
#'
#' @author Nolan MacDonald
#'
#' @export
table_ovr_te_fantasy <- function(num_players = NULL,
                                 scoring = NULL,
                                 seasons = NULL,
                                 pbp_db = NULL,
                                 pbp_db_tbl = NULL,
                                 file_name = NULL,
                                 output_dir = NULL
) {

  te_pbp_stats <- nuclearff::get_te_pbp_stats(pbp_db,
                                              pbp_db_tbl,
                                              seasons,
                                              week_min = 1
  ) %>%
    # Clean up player names in defined player column
    nuclearff::replace_player_names(player_col = "player_display_name") %>%
    # Clean up team names e.g., LA to LAR
    nuclearff::replace_team_names() %>%
    # Redefine position for the strange case of Taysom Hill to "TE"
    dplyr::mutate(position = dplyr::case_when(
      player_display_name == "Taysom Hill" ~ "TE",
      TRUE ~ position
    ))

  # Pull WR roster information and player IDs
  te_data <- nuclearff::get_player_data(seasons,
                                        position = "TE"
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
  te_stats <- te_pbp_stats %>%
    left_join(te_data,
              by = c("player_id",
                     "player_display_name",
                     "position",
                     "team")
    )

  # Add snap share to data
  snap_pct <- nuclearff::get_snap_share(season = seasons, pos = "TE") %>%
    # Clean up player names in defined player column
    nuclearff::replace_player_names(player_col = "player")

  te <- left_join(te_stats, snap_pct,
                  by = c("player_display_name" = "player",
                         "pfr_id" = "pfr_player_id",
                         "position",
                         "team"))


  # Define the columns to select based on `scoring`
  scoring_cols <- nuclearff::get_scoring_columns(scoring)

  # Format data to include in table
  te_data_tbl <- te %>%
    dplyr::select(
      player_id, player_name, team, games,
      # Rename default scoring column names
      ppg = dplyr::all_of(scoring_cols[1]),
      fpts = dplyr::all_of(scoring_cols[2]),
      snap_pct, tgt_pct, targets, receptions, receiving_yards, receiving_tds
    ) %>%
    mutate(
      # Make snap % and tgt_pct to 1 decimal
      snap_pct = round(snap_pct, 1),
      tgt_pct = round(tgt_pct, 1)
    ) %>%
    # Arrange by total fantasy points descending
    dplyr::arrange(dplyr::desc(fpts)) %>%
    # Only keep the top 16 TE
    utils::head(num_players) %>%
    # Combine "TE" with the numbers 1 to 16
    mutate(OVR = paste0("TE", 1:num_players)) %>%
    select(OVR, everything()) # Move OVERALL to the first column

  # Get the max week - current week
  week_number <- max(te_pbp_stats$games)
  # Get Scoring Format for subtitle
  format <- nuclearff::get_scoring_format(scoring, position = "TE")

  # Create table
  te_tbl <- te_data_tbl %>%
    gt() %>%
    # Get player headshots
    nflplotR::gt_nfl_headshots(columns = gt::everything(), height = 35) %>%
    # Get team logo
    nflplotR::gt_nfl_logos(columns = gt::starts_with("team")) %>%
    # Rename the columns
    cols_label(
      player_id = "",
      player_name = "PLAYER",
      team = "TEAM",
      games = "G",
      ppg = "PPG",
      fpts = "FPTS",
      snap_pct = "SNP%",
      tgt_pct = "TGT%",
      targets = "TAR",
      receptions = "REC",
      receiving_yards = "REC YDS",
      receiving_tds = "REC TD"
    ) %>%
    # Align column TEAM to center
    cols_align(
      align = "center",
      columns = c(team, games,
                  ppg, fpts,
                  snap_pct, tgt_pct,
                  targets, receptions, receiving_yards, receiving_tds)
    ) %>%
    # Align the "OVR" column values to the right
    cols_align(
      align = "right",
      columns = c(OVR)
    ) %>%
    # Make OVR column bold
    tab_style(
      style = cell_text(weight = "bold"),  # Make the text bold
      # Specify bold for all column titles and OVR values
      locations = list(
        cells_body(columns = c(OVR)),
        cells_column_labels(columns = c(OVR, player_name, team, games,
                                        ppg, fpts,
                                        snap_pct, tgt_pct,
                                        targets, receptions,
                                        receiving_yards, receiving_tds))
      )
    ) %>%
    # Add hulk color theme
    # trim = TRUE makes colors light, less pronounced
    gtExtras::gt_hulk_col_numeric(ppg, trim = FALSE) %>%
    # Footer
    tab_source_note(
      md(glue("<div style='display: flex; align-items: center; margin: 0; padding: 0; line-height: 0;'>
      <img src='https://raw.githubusercontent.com/NuclearAnalyticsLab/nuclearff/main/inst/logos/png/logo-500x500.png' style='height:30px; margin: 0; padding: 0; display: inline-block;'>
      <span style='margin-left: 5px; padding: 0; display: inline-block; vertical-align: middle;'>| Table: @nuclearffnolan | Data: @nflfastR |</span>
      </div>"))
    ) %>%
    # Title
    tab_header(
      title = md(glue("
    <div style='display: flex; justify-content: center; align-items: center;'>
      <div style='text-align: center;'>
      <strong>TE OVERALL STANDINGS WEEK {week_number}</strong>
      <div style='font-size: 14px;'>{seasons} Fantasy PPG {format}</div>
      </div>
      <div style='position: absolute; right: 120px;'>
      <img src='https://raw.githubusercontent.com/NuclearAnalyticsLab/nuclearff/main/inst/logos/svg/logos/nuclearff-pink-blue-alt.svg' style='height:80px;'>
      </div>
    </div>
    "))
    ) %>%
    # Add padding between title and subtitle
    gt::tab_options(
      heading.padding = px(0),  # Adjust the padding value as needed
      table.border.top.style = "hidden" # Hide top border above title
      # table.border.bottom.style = "hidden" # Hide bottom footnote border
    )

  # Fastest way to show a table with nflplotr
  nflplotR::gt_render_image(te_tbl)

  nuclearff::save_gt_table(te_tbl, file_name, output_dir)
}
