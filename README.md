<!-- README.md is generated from README.Rmd. Please edit that file -->
sportscraper
============

This package provides a set of simple yet convinient functions for
scraping sports data for the NBA, MLB, NFL, and NHL for players, teams,
and seasons.

Installation
------------

Example
-------

``` r
devtools::install_github("kmacdon/sportscraper")
```

\#\#Usage

This package provides three functions for scraping data one each for
players, teams and seasons.

``` r
df <- player_stats("Kobe Bryant", "NBA")
```

The player function accepts a player name and league than returns their
per game statistics. For NBA and MLB players, the argument `advanced`
can be set to true to return advanced statistics instead.

``` r
df <- team_stats("Boston Bruins", "NHL")
```

The team function accepts a team and a league and returns the season
stats for their entire history. Currently this does not work for NFL
teams as I am still implementing that.
