
<!-- README.md is generated from README.Rmd. Please edit the .Rmd file -->

# mort <img src="man/figures/logo.png" align="right" height="139" />

<!-- badges: start -->
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

You can install mort from [GitHub](https://github.com/) with the line
below. Note that you must have the package `devtools` installed.

``` r
devtools::install_github("rosieluain/mort")
```

### Package contents

Please see the package vignettes for more details, as well as guidelines
and tips for the following functions.

**Note:** vignettes are under development! We are currently developing a
sample dataset, so there are some obvious gaps where there should be
figures, tables, etc. These will be completed as soon as the sample
dataset is done.

#### Data preparation and visualization

`residences` condenses detection records into residence events, with a
start time, end time, and duration. Residence events are used as the
input for all other mort functions.

`mortsplot` generates plots of residence events using `ggplot2`. Plots
are automatically formatted to maximize visibility of the dataset, and
can be further modified using `ggplot2` commands. Interactive plots can
also be generated using `plotly`.

<!-- ### Should make an example plot from example dataset when ready -->

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

mort is brand new. Although it is has been tested extensively on a
complex dataset, we expect that issues will arise as mort is applied to
other datasets and systems. If you run into any issues or have any
suggestions for improvements, please post an issue, and we’ll see what
we can do!

<!-- ## Example -->
<!-- This is a basic example which shows you how to solve a common problem: -->
<!-- ```{r example} -->
<!-- library(mort) -->
<!-- ## basic example code -->
<!-- ``` -->
<!-- What is special about using `README.Rmd` instead of just `README.md`? You can include R chunks like so: -->
<!-- ```{r cars} -->
<!-- summary(cars) -->
<!-- ``` -->
<!-- You'll still need to render `README.Rmd` regularly, to keep `README.md` up-to-date. `devtools::build_readme()` is handy for this. You could also use GitHub Actions to re-render `README.Rmd` every time you push. An example workflow can be found here: <https://github.com/r-lib/actions/tree/v1/examples>. -->
<!-- You can also embed plots, for example: -->
<!-- ```{r pressure, echo = FALSE} -->
<!-- plot(pressure) -->
<!-- ``` -->
<!-- In that case, don't forget to commit and push the resulting figure files, so they display on GitHub and CRAN. -->
