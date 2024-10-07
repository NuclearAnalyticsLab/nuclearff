################################################################################
# Author: Nolan MacDonald
# Purpose:
#   Calculate total fantasy points and points per game (PPG) based on standard
#   scoring formats.
# Code Style Guide:
#   styler::tidyverse_style(), lintr::use_lintr(type = "tidyverse")
################################################################################

#' Calculate Fantasy Points
#'
#' @description
#' Calculate fantasy points using play-by-play and data and user-defined
#' scoring settings (e.g., 6PT PASS TD, -2 PASS INT, Full PPR)
#'
#'
#' @param pbp Play-by-play Dataframe
#' @param pass_td_pts Passing Touchdown Points
#' @param pass_int_pts Passing Interception Points
#' @param rec_pts Reception Points
#'
#' @return
#' Dataframe with total fantasy points using play-by-play data and user-defined
#' scoring settings (e.g., 6PT PASS TD, -2 PASS INT, Full PPR)
#'
#' @author Nolan MacDonald
#'
#' @export
calc_fpts <- function(pbp = pbp,
                      pass_td_pts = 6,
                      pass_int_pts = -2,
                      rec_pts = 1)
{
    pbp %>%
        dplyr::mutate(
            fpts = (0.04 * passing_yards) +  # PASS YDS 1 point every 25 yards
                (pass_td_pts * passing_tds) + # PASS TD
                (2 * passing_2pt_conversions) + # 2PT Conversions (PASS)
                (pass_int_pts * interceptions) + # INT
                (-2 * sack_fumbles_lost) + # Sack Fumbles Lost
                (0.1 * rushing_yards) + # RUSH YD
                (6 * rushing_tds) + # RUSH TD
                (-2 * rushing_fumbles_lost) + # Rush Fumbles Lost
                (2 * rushing_2pt_conversions) + # 2PT Conversions (RUSH)
                (rec_pts * receptions) + # REC (STD, HALF PPR, PPR)
                (0.1 * receiving_yards) + # REC YD
                (6 * receiving_tds) + # REC TD
                (-2 * receiving_fumbles_lost) + # REC Fumble Lost
                (2 * receiving_2pt_conversions) # 2PT Conversions (REC)
        )
}

#' Calculate Fantasy Points for Common Scoring Formats
#'
#' @description
#' Calculate fantasy points using play-by-play data and common
#' scoring settings (i.e., 4/6PT PASS TD, -2 PASS INT, STD/Half PPR/Full PPR)
#'
#' @param pbp Play-by-play Data frame
#'
#' @return
#' Data frame with total fantasy points for common
#' scoring settings (i.e., 4/6PT PASS TD, -2 PASS INT, STD/Half PPR/Full PPR)
#'
#' @seealso \code{\link[nuclearff]{calc_fpts}}
#'  Calculate total fantasy points
#'
#' @author Nolan MacDonald
#'
#' @format A data frame with 79 variables that are described below.
#' \describe{
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
#'  }
#'
#' @export
calc_fpts_common_formats <- function(pbp = pbp)
{
    pbp %>%
        # 4 point passing TD ---------------------------------------------------
        # Calculate fantasy points for standard scoring
        nuclearff::calc_fpts(pass_td_pts = 4,
                            pass_int_pts = -2,
                            rec_pts = 0) %>%
        # Rename to fpts_std
        dplyr::mutate(fpts_std_4pt_td = round(fpts, 2)) %>%
        # Remove the default `fpts` column
        dplyr::select(-fpts) %>%

        # Calculate fantasy points for half PPR scoring
        nuclearff::calc_fpts(pass_td_pts = 4,
                             pass_int_pts = -2,
                             rec_pts = 0.5) %>%
        # Rename to fpts_half_ppr
        dplyr::mutate(fpts_half_ppr_4pt_td = round(fpts, 2)) %>%
        # Remove the default `fpts` column
        dplyr::select(-fpts) %>%

        # Calculate fantasy points for full PPR scoring
        nuclearff::calc_fpts(pass_td_pts = 4,
                             pass_int_pts = -2,
                             rec_pts = 1) %>%
        # Rename to fpts_ppr
        dplyr::mutate(fpts_ppr_4pt_td = round(fpts, 2)) %>%
        # Remove the default `fpts` column
        dplyr::select(-fpts) %>%
        # 6 point passing TD ---------------------------------------------------
        # Calculate fantasy points for standard scoring
        nuclearff::calc_fpts(pass_td_pts = 6,
                                  pass_int_pts = -2,
                                  rec_pts = 0) %>%
        # Rename to fpts_std
        dplyr::mutate(fpts_std_6pt_td = round(fpts, 2)) %>%
        # Remove the default `fpts` column
        dplyr::select(-fpts) %>%

        # Calculate fantasy points for half PPR scoring
        nuclearff::calc_fpts(pass_td_pts = 6,
                                  pass_int_pts = -2,
                                  rec_pts = 0.5) %>%
        # Rename to fpts_half_ppr
        dplyr::mutate(fpts_half_ppr_6pt_td = round(fpts, 2)) %>%
        # Remove the default `fpts` column
        dplyr::select(-fpts) %>%

        # Calculate fantasy points for full PPR scoring
        nuclearff::calc_fpts(pass_td_pts = 6,
                                  pass_int_pts = -2,
                                  rec_pts = 1) %>%
        # Rename to fpts_ppr
        dplyr::mutate(fpts_ppr_6pt_td = round(fpts, 2)) %>%
        # Remove the default `fpts` column
        dplyr::select(-fpts)
}

#' Calculate Fantasy Points Per Game for Common Scoring Formats
#'
#' @description
#' Calculate fantasy points per game using play-by-play and data and common
#' scoring settings (i.e., 4/6PT PASS TD, -2 PASS INT, STD/Half PPR/Full PPR)
#'
#'
#' @param pbp Play-by-play Dataframe
#'
#' @return
#' Dataframe with fantasy points per game (PPG) using common
#' scoring settings (i.e., 4/6PT PASS TD, -2 PASS INT, STD/Half PPR/Full PPR)
#'
#' @author Nolan MacDonald
#'
#' @export
calc_fpts_ppg_common_formats <- function(pbp = pbp)
{
    pbp %>%
        nuclearff::calc_fpts_common_formats() %>%
        dplyr::mutate(
            ppg_std_4pt_td = round(fpts_std_4pt_td / games, 2),
            ppg_half_ppr_4pt_td = round(fpts_half_ppr_4pt_td / games, 2),
            ppg_ppr_4pt_td = round(fpts_ppr_4pt_td / games, 2),
            ppg_std_6pt_td = round(fpts_std_6pt_td / games, 2),
            ppg_half_ppr_6pt_td = round(fpts_half_ppr_6pt_td / games, 2),
            ppg_ppr_6pt_td = round(fpts_ppr_6pt_td / games, 2)
        )
}
