
## sportscraper

<!-- badges: start -->
[![Lifecycle: experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://www.tidyverse.org/lifecycle/#experimental)
[![Build Status](https://travis-ci.com/kmacdon/sportscraper.svg?branch=master)](https://travis-ci.com/kmacdon/sportscraper)
<!-- badges: end -->

This package provides a set of simple yet convinient functions for
scraping sports data for the NBA, MLB, NFL, and NHL for players and teams.

## Installation

``` r
devtools::install_github("kmacdon/sportscraper")
```

## Usage

This package provides two functions for scraping sports data, one for players and one for teams.

``` r
df <- player_data("Kobe Bryant", "NBA")
```

The player function accepts a player name and league than returns their
per game statistics. For NBA and MLB players, the argument `advanced`
can be set to true to return advanced statistics instead. If there are
multiple players with the same name, a list will be presented with the
years that each player played, and you will be prompted to select one to
continue with.

``` r
df <- team_data("Boston Bruins", "NHL")
```

The team function accepts a team and a league and returns the season
stats for their entire history. Currently this does not work for NFL
teams as I am still implementing that. Please report any issues or
suggestions on the Issues tab.
