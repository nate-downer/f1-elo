## globals ----

# libraries
library(dplyr)
library(ggplot2)
library(lubridate)
library(gt)
library(gtExtras)
library(openxlsx)
library(tidyverse)
library(DBI)
library(lubridate)
library(dbplyr)
library(glue)
library(RColorBrewer)
library(cowplot)
library(highcharter)


## to do ----

#' [x] Write faster algorithm that computes all winnings at the same time
#' [x] Refine programmatic matchmaking (to throttle top end growth)
#' [ ] Create method for fixing team performance to race pace
#' [x] Scale k values by number of races in a season (for parity across time)
#' [x] Create interactive outputs using high charts
#' [x] Slowly decrease ELO for races the teams are not in (Accounts for DNFs and Missing Seasons)
#' [ ] Apply different decay rates to active and non-active drivers
#' [ ] Try nudging all drivers scores so that the mean stays at 1000
#' [ ] Update data through the end of 2023 Season
#' [x] Remove Indy 500
#' [ ] Give teams higher k values (to account for faster shifting car performance)



## read in data ----

df_path <- "local_data/championship_data_2023_01/"

drivers_df <- read_csv(paste0(df_path, "drivers.csv"))
constructors_df <- read_csv(paste0(df_path, "constructors.csv"))
races_df <- read_csv(paste0(df_path, "races.csv"))
results_df <- read_csv(paste0(df_path, "results.csv"))
qualifying_df <- read_csv(paste0(df_path, "qualifying.csv"))
seasons_df <- read_csv(paste0(df_path, "seasons.csv"))
circuits_df <- read_csv(paste0(df_path, "circuits.csv"))


# build master results table
race_res_master <- results_df %>%
  select(resultId, raceId, driverId, constructorId,
         racePositionOrder = positionOrder, racePosition = position,
         gridPosition = grid) %>%
  merge(
    drivers_df %>%
      select(driverId, driver = driverRef, driverCode = code)
  ) %>%
  merge(
    constructors_df %>%
      select(constructorId, team = constructorRef)
  ) %>%
  # merge(
  #   qualifying_df %>%
  #     select(qualifyId, raceId, driverId, constructorId,
  #            qualiPosition = position)
  # ) %>%
  merge(
    races_df %>%
      select(raceId, circuitId, year, round)
  ) %>%
  merge(
    circuits_df %>%
      select(circuitId, country)
  ) %>%
  mutate(round_text = if_else(as.numeric(round) < 10, paste0(0, round), paste(round)),
         race = paste(year, round_text, tolower(country), sep = "_"),
         raceOrder = (year * 100) + round) %>%
  select(-round_text) %>%
  arrange(raceOrder, racePositionOrder)


## run elo funcitons ----

source("elo_functions.R")















