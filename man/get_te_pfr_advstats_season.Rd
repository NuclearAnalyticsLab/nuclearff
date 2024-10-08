% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/get_pfr_advstats_season.R
\name{get_te_pfr_advstats_season}
\alias{get_te_pfr_advstats_season}
\title{TE Advanced Season Stats from Pro Football Reference (PFR)}
\format{
A data frame with 22 variables that are described below.
\describe{
\item{\code{player_display_name}}{Player name (e.g., George Kittle)}
\item{\code{age}}{Player age}
\item{\code{position}}{Player position}
\item{\code{g}}{Number of games played}
\item{\code{gs}}{Number of games started}
\item{\code{tgt}}{Total targets}
\item{\code{rec}}{Total receptions}
\item{\code{yds}}{Total receiving yards}
\item{\code{td}}{Total receiving touchdowns}
\item{\code{x1d}}{First downs receiving}
\item{\code{ybc}}{
Total yards passes traveled in the air before being caught or yards
before catch (YBC)}
\item{\code{ybc_r}}{Yards before catch per reception}
\item{\code{yac}}{Yards after catch (YAC)}
\item{\code{yac_r}}{Yards after catch (YAC) per reception}
\item{\code{adot}}{
Average depth of target (ADOT) when targeted, whether completed or not.
}
\item{\code{brk_tkl}}{Number of broken tackles}
\item{\code{rec_br}}{Receptions per broken tackle}
\item{\code{drop}}{Number of dropped passes}
\item{\code{drop_percent}}{Dropped pass percentage when targeted}
\item{\code{int_tgt}}{Interceptions on passes where targeted}
\item{\code{rat}}{Passer rating on passes when targeted}
\item{\code{pfr_player_id}}{
Pro Football Reference player ID (e.g., KittGe00)
}
}
}
\usage{
get_te_pfr_advstats_season(seasons = NULL)
}
\arguments{
\item{seasons}{NFL Season(s) where \code{seasons = 2024}}
}
\value{
Dataframe with TE advanced stats from Pro Football Reference (PFR)
for a defined season
}
\description{
Obtain TE advanced season stats from \code{nflreadr},
to acquire Pro Football Reference (PFR) data.
}
\details{
\code{get_te_pfr_advstats_season} is a function to obtain advanced stats from
Pro Football Reference (PFR).
The function utilizes the \code{nflreadr} function \code{load_pfr_advstats} to load
player level season stats starting with the 2018 season.
For receivers, the \code{stat_type} is defined as \code{rec}.
More information is available about TE PFR advanced stats from seasons on
the PFR site such as
\href{https://www.pro-football-reference.com/years/2024/receiving_advanced.htm}{PFR 2024 NFL Advanced Receiving}.
}
\seealso{
\code{\link[nflreadr]{load_pfr_advstats}}
Load advanced stats from PFR
}
\author{
Nolan MacDonald
}
