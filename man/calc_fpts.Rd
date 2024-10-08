% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/calculate_fpts.R
\name{calc_fpts}
\alias{calc_fpts}
\title{Calculate Fantasy Points}
\usage{
calc_fpts(pbp = pbp, pass_td_pts = 6, pass_int_pts = -2, rec_pts = 1)
}
\arguments{
\item{pbp}{Play-by-play Dataframe}

\item{pass_td_pts}{Passing Touchdown Points}

\item{pass_int_pts}{Passing Interception Points}

\item{rec_pts}{Reception Points}
}
\value{
Dataframe with total fantasy points using play-by-play data and user-defined
scoring settings (e.g., 6PT PASS TD, -2 PASS INT, Full PPR)
}
\description{
Calculate fantasy points using play-by-play and data and user-defined
scoring settings (e.g., 6PT PASS TD, -2 PASS INT, Full PPR)
}
\author{
Nolan MacDonald
}
