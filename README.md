# nuclearff <img src="man/figures/logo.png" align="right" height="139" alt="" />

<!-- badges: start -->
[![Lifecycle: experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
[![Documentation](https://img.shields.io/badge/docs-pkgdown-blue.svg)](https://nuclearanalyticslab.github.io/nuclearff/)
[![License: MIT](https://img.shields.io/badge/License-MIT-yellow.svg)](https://opensource.org/licenses/MIT)
<!-- badges: end -->

**N**FL **U**tilities **C**ore **L**ibrary for **E**valuating **A**nalytics in **R** with **F**antasy **F**ootball (**NUCLEARFF**)

`nuclearff` is used to obtain NFL analytics and statistics using 
`nflverse` packages in conjunction with fantasy football.
Functions are created to parse the play-by-play data and output a dataframe
that contains all the information a user needs.
In addition, additional capabilities are added for fantasy football 
considerations.
More information on `nflverse` and the NFL packages is available in the 
[nflverse documentation](https://nflverse.nflverse.com).

  
For more information on `nuclearff` and how to use this package, refer to the 
[documentation](https://nuclearanalyticslab.github.io/nuclearff/).

## Installation

Installing the `nuclearff` package can be accomplished by using the `remotes`
package. First, install `remotes` by using the following command in R console:
```r
install.packages("remotes")
remotes::install_github("NuclearAnalyticsLab/nuclearff")
```
Installation of the `nuclearff` package can now be performed with the 
`install_github()` function.
```r
remotes::install_github("NuclearAnalyticsLab/nuclearff")
```

If for any reason the package needs to be uninstalled, use the command:
```r
remove.packages("nuclearff")
```

## Data Usage

To use the package, play-by-play data from `nflverse` packages is required.
By default `nuclearff` functions assume there is no database with play-by-play 
data.
Instead, the play-by-play data is pulled using `nflreadr::load_pbp`.

The alternative is to use a database, since it is simple to build a database
and keep it up-to-date. 
Play-by-play data since 1999 takes up a lot of memory, so working with a 
database allows you to store only what is necessary into memory.
Functions allow for using the database with arguments `pbp_db` and `pbp_db_tbl`.
These arguments must be defined as strings, where `pbp_db` is the path to the
database as well as the name. `pbp_db_tbl` is the name of the table in the
database.

For example, you can store the database in the directory `data/` with the
default naming scheme used by `nflfastR`.
This means that `pbp_db="data/pbp_db"` and `pbp_db_tbl="nflfastR_pbp"`.
The database is not included in the repository, and users must download a 
play-by-play database prior to utilizing functions with database options in 
`nuclearff`.

For detailed instructions on setting up the database with `nflfastR`, 
view the vignette using `vignette("setup_pbp_db")` in an R console.
Users may also refer to the article,
[Setting Up the nflfastR Database](https://nuclearanalyticslab.github.io/nuclearff/articles/setup_pbp_db.html).




