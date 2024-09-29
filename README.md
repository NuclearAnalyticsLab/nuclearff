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

To use the package, play-by-play data from `nflfastR` is required.
By default `nuclearff` functions look for a database located in `data/pbp_db`.
The database is not included in the repository, and users must download a 
play-by-play database prior to utilizing functions in `nuclearff`.

The database is named `pbp_db` containing a table `nflfastR_pbp`. 
This is the default naming scheme for `nflfastR`. All functions do allow 
specifying the database location, name, and table name. The key is that to use
`nuclearff`, a database is required.

For detailed instructions on setting up the database with `nflfastR`, 
view the vignette using `vignette("setup_pbp_db")` in an R console.
Users may also refer to the article,
[Setting Up the nflfastR Database](https://nuclearanalyticslab.github.io/nuclearff/articles/setup_pbp_db.html).




