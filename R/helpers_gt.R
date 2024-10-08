################################################################################
# Author: Nolan MacDonald
# Purpose:
#   Helpers for gt Table creation and formatting
# Code Style Guide:
#   styler::tidyverse_style(), lintr::use_lintr(type = "tidyverse")
################################################################################

#' Helper Function - Get Scoring Columns
#'
#' @description
#' Obtain the valid columns to use in `dplyr::select` for a user-defined
#' scoring format.
#'
#' @details
#' Depending on the user-defined argument when creating `gt` tables for
#' fantasy stats, the scoring format columns can be included while neglecting
#' all other calculated fantasy scoring formats.
#'
#' @param scoring Fantasy scoring format (str). Can be defined as standard
#'  scoring, half PPR scoring, full PPR scoring, 4PT PASS TD or 6PT PASS TD by
#'  using: `"std_4pt_td"`, `"half_ppr_4pt_td"`, `"ppr_4pt_td"`,
#'  `"std_6pt_td"`, `"half_ppr_6pt_td"`, `"ppr_6pt_td"`
#'
#' @return Defined column names for fantasy points and points per game (PPG)
#'
#' @seealso \code{\link[nuclearff]{calc_fpts_common_formats}}
#'  Calculate fantasy points based on common scoring formats
#' @seealso \code{\link[nuclearff]{calc_fpts_ppg_common_formats}}
#'  Calculate fantasy points per game (PPG) based on common scoring formats
#' @seealso \code{\link[nuclearff]{table_ovr_qb_fantasy}}
#'  Create table with overall fantasy standings for QB
#'
#' @author Nolan MacDonald
#'
#' @export
get_scoring_columns <- function(scoring = NULL) {

  valid_scoring_options = c("std_4pt_td", "half_ppr_4pt_td", "ppr_4pt_td",
                            "std_6pt_td", "half_ppr_6pt_td", "ppr_6pt_td"
  )

  # Check if `scoring` is NULL or missing, and assign a default if so
  if (is.null(scoring)) {
    paste("No scoring format defined. Default to PPR 6PT PASS TD")
    scoring <- "ppr_6pt_td"  # Default scoring format
  }

  # Ensure `scoring` is a string and a valid option
  if (!is.character(scoring) || length(scoring) != 1) {
    stop("Error: `scoring` must be a single string.")
  }

  if (!scoring %in% valid_scoring_options) {
    stop(paste("Error: Invalid `scoring` value. Must be one of:",
               paste(valid_scoring_options, collapse = ", ")))
  }

  if (scoring == "std_4pt_td") {
    return(c("ppg_std_4pt_td", "fpts_std_4pt_td"))
  } else if (scoring == "half_ppr_4pt_td") {
    return(c("ppg_half_ppr_4pt_td", "fpts_half_ppr_4pt_td"))
  } else if (scoring == "ppr_4pt_td") {
    return(c("ppg_ppr_4pt_td", "fpts_ppr_4pt_td"))
  } else if (scoring == "std_6pt_td") {
    return(c("ppg_std_6pt_td", "fpts_std_6pt_td"))
  } else if (scoring == "half_ppr_6pt_td") {
    return(c("ppg_half_ppr_6pt_td", "fpts_half_ppr_6pt_td"))
  } else if (scoring == "ppr_6pt_td") {
    return(c("ppg_ppr_6pt_td", "fpts_ppr_6pt_td"))
  } else {
    return(c("ppg_ppr_6pt_td", "fpts_ppr_6pt_td"))  # Default
  }
}

#' Helper Function - Get Scoring Format
#'
#' @description
#' Obtain the valid columns to use in `dplyr::select` for a user-defined
#' scoring format.
#'
#' @details
#' Depending on the user-defined argument when creating `gt` tables for
#' fantasy stats, the scoring format columns can be included while neglecting
#' all other calculated fantasy scoring formats.
#'
#' @param scoring Fantasy scoring format (str). Can be defined as standard
#'  scoring, half PPR scoring, full PPR scoring, 4PT PASS TD or 6PT PASS TD by
#'  using: `"std_4pt_td"`, `"half_ppr_4pt_td"`, `"ppr_4pt_td"`,
#'  `"std_6pt_td"`, `"half_ppr_6pt_td"`, `"ppr_6pt_td"`
#' @param position Player position (str) to determine if extra format statement
#'  is necessary (only in the case of QB for PASS TD points).
#'
#' @return String with fantasy scoring format to use in table subtitle
#'
#' @seealso \code{\link[nuclearff]{calc_fpts_common_formats}}
#'  Calculate fantasy points based on common scoring formats
#' @seealso \code{\link[nuclearff]{calc_fpts_ppg_common_formats}}
#'  Calculate fantasy points per game (PPG) based on common scoring formats
#' @seealso \code{\link[nuclearff]{table_ovr_qb_fantasy}}
#'  Create table with overall fantasy standings for QB
#'
#' @author Nolan MacDonald
#'
#' @export
get_scoring_format <- function(scoring = NULL, position = NULL) {

  valid_scoring_options = c("std_4pt_td", "half_ppr_4pt_td", "ppr_4pt_td",
                            "std_6pt_td", "half_ppr_6pt_td", "ppr_6pt_td"
  )

  # Check if `scoring` is NULL or missing, and assign a default if so
  if (is.null(scoring)) {
    paste("No scoring format defined. Default to PPR 6PT PASS TD")
    scoring <- "ppr_6pt_td"  # Default scoring format
  }

  # Ensure `scoring` is a string and a valid option
  if (!is.character(scoring) || length(scoring) != 1) {
    stop("Error: `scoring` must be a single string.")
  }

  if (!scoring %in% valid_scoring_options) {
    stop(paste("Error: Invalid `scoring` value. Must be one of:",
               paste(valid_scoring_options, collapse = ", ")))
  }

  # Base format assignment
  if (startsWith(scoring, "std")) {
    format <- "Standard Scoring"
  } else if (startsWith(scoring, "half_ppr")) {
    format <- "Half PPR Scoring"
  } else if (startsWith(scoring, "ppr")) {
    format <- "Full PPR Scoring"
  } else {
    format <- "Full PPR Scoring"  # Default
  }

  # Only apply the QB-specific logic if position is QB
  if (position == "QB") {
    if (endsWith(scoring, "4pt_td")) {
      format <- paste(format, "4PT PASS TD")
    } else if (endsWith(scoring, "6pt_td")) {
      format <- paste(format, "6PT PASS TD")
    }
  }


  return(format)
}
