
<!-- README.md is generated from README.Rmd. Please edit the .Rmd file -->

# mort <img src="man/figures/logo.png" align="right" height="139" />

<!-- badges: start -->

[![Codecov test
coverage](https://codecov.io/gh/rosieluain/mort/branch/main/graph/badge.svg)](https://app.codecov.io/gh/rosieluain/mort?branch=main)
<!-- badges: end -->

The goal of mort is to provide a framework to identify potential
mortalities or expelled tags in passive acoustic telemetry arrays with
non-overlapping receivers. The potential mortalities that are flagged by
mort should be reviewed by the user, and considered for removal from the
dataset.

Please note that this method is conservative, and therefore may
overestimate the number of mortalities in the system. It is therefore
not advised to use the results as estimates of survival or tagging
effects. Rather, the purpose is to remove or reduce potential bias
before conducting further analyses.

mort uses thresholds from the dataset itself, use-defined thresholds,
and several customizable options, to allow application to a wide number
of species and acoustic arrays. By providing a standardized framework
for consideration of potential mortalities, we hope this tool will be
useful and encourage greater reproducibility in acoustic telemetry
research.

### Installation

You can install mort from CRAN with the line below.

``` r
install.packages("mort")
```

To install mort directly from GitHub, including any updates that might
not be released on CRAN yet, use the line below. Note that you must have
the package `devtools` installed.

``` r
devtools::install_github("rosieluain/mort")
```

### Package contents

Please see the package vignettes for more details, as well as guidelines
and tips for the following functions.

#### Data preparation and visualization

`residences` condenses detection records into residence events, with a
start time, end time, and duration. Residence events are used as the
input for all other mort functions.

`mortsplot` generates plots of residence events using `ggplot2`. Plots
are automatically formatted to maximize visibility of the dataset, and
can be further modified using `ggplot2` commands. Interactive plots can
also be generated using `plotly`.

#### Identifying potential mortalities or expelled tags

`morts` identifies potential mortalities or expelled tags, based on the
duration of single residence events or cumulative residence events (see
vignettes for a complete explanation). Thresholds are derived from the
input dataset.

`infrequent` identifies potential mortalities or expelled tags from
infrequent or intermittent detections. Thresholds and timeframes are
defined by the user.

`review` examines new data to determine if an animal that was previously
flagged as a mortality has moved, and may therefore be alive.

#### Data and process exploration

These are functions that may be called by `morts` and/or `infrequent`,
depending on the options that are selected. These functions are fully
documented so the user can explore their data and the process used by
mort.

`stationchange` identifies the most recent station or location change
for each animal (i.e., the last time each animal moved, and therefore
was assumed to be alive).

`resmax` extracts the residence events that occurred prior to the most
recent station change for each individual.

`resmaxcml` generates cumulative residence events (from the first time
an animal was detected at a given station to the last time an animal was
detected at the same station, ignoring gaps in detection) that occurred
prior to the most recent station change for each individual.

`drift` creates drift events from sequential residence events, where
detected movement between stations may be due to drifting of a dead
animal or an expelled tag.

`season` selects residence events from user-specified seasons or periods
of interest.

`backwards` shifts the start time of a flagged mortality earlier, if the
residence event that triggered the flag was not the earliest consecutive
residence event at that station/location.

### Disclaimer

mort is pretty new. Although it is has been tested extensively on a
complex dataset, we expect that issues will arise as mort is applied to
other datasets and systems. If you run into any issues or have any
suggestions for improvements, please post an issue on
[GitHub](https://github.com/rosieluain/mort/issues), and we’ll see what
we can do!
