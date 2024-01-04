# elo functions



## basic elo equations ----

get_ante <- function(elo) {

  10 ^ (elo / 400)

}

get_pot <- function(ante_1, ante_2) {

  ante_1 / (ante_1 + ante_2)

}

get_winnings <- function(elo_1, elo_2, k, outcome = c(1, 0, 0.5)) {

  pot <- get_pot(elo_1, elo_2)
  winnings <- round(k * (outcome - pot))

  return(winnings)

}

get_winnings(1000, 1500, 32, 1)


## functions for calculating results ----

get_net_driver_winnings <- function(race_resuts, starting_elos, driver_1, k) {

  driver_results <- race_resuts %>%
    filter(driver == driver_1) %>%
    select(driver_1 = driver, driver_1_pos = racePosition)

  other_results <- race_resuts %>%
    filter(driver != driver_1) %>%
    select(driver_2 = driver, driver_2_pos = racePosition)

  all_results <- other_results %>%
    merge(driver_results, all.x = TRUE, all.y = TRUE) %>%
    merge(
      starting_elos %>%
        select(driver_1 = driver, elo_1 = elo)
    ) %>%
    merge(
      starting_elos %>%
        select(driver_2 = driver, elo_2 = elo)
    ) %>%
    mutate(outcome = case_when(grepl("N", driver_1_pos) | grepl("N", driver_2_pos) ~ 0.5,
                               as.numeric(driver_1_pos) == as.numeric(driver_2_pos) ~ 0.5,
                               as.numeric(driver_1_pos) < as.numeric(driver_2_pos) ~ 1,
                               TRUE ~ 0)) %>%
    rowwise() %>%
    mutate(winnings = get_winnings(elo_1, elo_2, k, outcome)) %>%
    ungroup()

  return(all_results)

}

get_driver_team_winnings <- function(race_resuts, driver_starting_elos, team_starting_elos, driver_1, k,
                                     min_matches = 2, match_range = 200) {

  driver_results <- race_resuts %>%
    filter(driver == driver_1) %>%
    select(driver_1 = driver, team_1 = team, driver_1_pos = racePosition)

  other_results <- race_resuts %>%
    filter(driver != driver_1) %>%
    select(driver_2 = driver, team_2 = team, driver_2_pos = racePosition)

  all_results <- other_results %>%
    merge(driver_results, all.x = TRUE, all.y = TRUE) %>%
    merge(
      driver_starting_elos %>%
        select(driver_1 = driver, driver_elo_1 = elo)
    ) %>%
    merge(
      driver_starting_elos %>%
        select(driver_2 = driver, driver_elo_2 = elo)
    ) %>%
    merge(
      team_starting_elos %>%
        select(team_1 = team, team_elo_1 = elo)
    ) %>%
    merge(
      team_starting_elos %>%
        select(team_2 = team, team_elo_2 = elo)
    ) %>%
    mutate(elo_1 = (driver_elo_1 + team_elo_1) /2,
           elo_2 = (driver_elo_2 + team_elo_2) /2) %>%
    mutate(elo_diff = abs(elo_1 - elo_2)) %>%
    arrange(elo_diff)

  filtered_results <- all_results %>%
    head(min_matches) %>%
    bind_rows(
      all_results %>%
        filter(elo_diff < match_range) %>%
        filter(elo_diff > (-1 * match_range))
    ) %>%
    distinct(driver_2, .keep_all = TRUE) %>%
    mutate(outcome = case_when(grepl("N", driver_1_pos) | grepl("N", driver_2_pos) ~ 0.5,
                               as.numeric(driver_1_pos) == as.numeric(driver_2_pos) ~ 0.5,
                               as.numeric(driver_1_pos) < as.numeric(driver_2_pos) ~ 1,
                               TRUE ~ 0)) %>%
    rowwise() %>%
    mutate(winnings = get_winnings(elo_1, elo_2, k, outcome)) %>%
    ungroup()

  return(sum(filtered_results$winnings))

}

get_team_winnings <- function(race_resuts, driver_starting_elos, team_starting_elos, team_1, k,
                              min_matches = 2, match_range = 200) {

  team_best_result <- race_resuts %>%
    filter(team == team_1) %>%
    summarise(min(racePositionOrder)) %>%
    pull()

  team_best_result <- race_resuts %>%
    filter(team == team_1) %>%
    filter(racePositionOrder == team_best_result) %>%
    select(driver_1 = driver, team_1 = team, driver_1_pos = racePosition)

  other_results <- race_resuts %>%
    filter(team != team_1) %>%
    select(driver_2 = driver, team_2 = team, driver_2_pos = racePosition)

  all_results <- other_results %>%
    merge(team_best_result, all.x = TRUE, all.y = TRUE) %>%
    merge(
      driver_starting_elos %>%
        select(driver_1 = driver, driver_elo_1 = elo)
    ) %>%
    merge(
      driver_starting_elos %>%
        select(driver_2 = driver, driver_elo_2 = elo)
    ) %>%
    merge(
      team_starting_elos %>%
        select(team_1 = team, team_elo_1 = elo)
    ) %>%
    merge(
      team_starting_elos %>%
        select(team_2 = team, team_elo_2 = elo)
    ) %>%
    mutate(elo_1 = (driver_elo_1 + team_elo_1) / 2,
           elo_2 = (driver_elo_2 + team_elo_2) / 2) %>%
    mutate(elo_diff = abs(elo_1 - elo_2)) %>%
    arrange(elo_diff)

  filtered_results <- all_results %>%
    head(min_matches) %>%
    bind_rows(
      all_results %>%
        filter(elo_diff < match_range) %>%
        filter(elo_diff > (-1 * match_range))
    ) %>%
    distinct(driver_2, .keep_all = TRUE) %>%
    mutate(outcome = case_when(grepl("N", driver_1_pos) | grepl("N", driver_2_pos) ~ 0.5,
                               as.numeric(driver_1_pos) == as.numeric(driver_2_pos) ~ 0.5,
                               as.numeric(driver_1_pos) < as.numeric(driver_2_pos) ~ 1,
                               TRUE ~ 0)) %>%
    rowwise() %>%
    mutate(winnings = get_winnings(elo_1, elo_2, k, outcome)) %>%
    ungroup()

  return(sum(filtered_results$winnings))

}


# validation
test_results <- race_res_master %>%
  filter(race == "1950_01_uk")

test_elos <- race_res_master %>%
  distinct(driver) %>%
  mutate(elo = 1000)

test_team_elos <- race_res_master %>%
  distinct(team) %>%
  mutate(elo = 1000)

get_net_driver_winnings(test_results, test_elos, "fagioli", 32) %>% View()

get_driver_team_winnings(test_results, test_elos, test_team_elos, "cabantous", 32)

get_team_winnings(test_results, test_elos, test_team_elos, "lago", 32)





## get results for every driver and constructor in a race ----
get_elo_changes <- function(race_results, driver_starting_elos, team_starting_elos, k,
                            min_matches = 2, match_range = 200) {

  driver_elos <- race_results %>%
    distinct(driver) %>%
    merge(driver_starting_elos) %>%
    rowwise() %>%
    mutate(winnings = get_driver_team_winnings(
      race_results,
      driver_starting_elos,
      team_starting_elos,
      driver,
      k,
      min_matches,
      match_range)) %>%
    ungroup() %>%
    mutate(new_elo = elo + winnings)

  team_elos <- race_results %>%
    distinct(team) %>%
    merge(team_starting_elos) %>%
    rowwise() %>%
    mutate(winnings = get_team_winnings(
      race_results,
      driver_starting_elos,
      team_starting_elos,
      team,
      k / 2,
      min_matches,
      match_range)) %>%
    ungroup() %>%
    mutate(new_elo = elo + winnings)

  return(bind_rows(driver_elos, team_elos))

}

get_elo_changes(test_results, test_elos, test_team_elos, 16) %>% View()


get_race_results <- function(race_results, driver_starting_elos, team_starting_elos, k,
                             min_matches = 2, match_range = 200) {

  results_matrix <- race_results %>%
    select(driver_1 = driver,
           team_1 = team,
           position_1 = racePosition) %>%
    full_join(
      race_results %>%
        select(driver_2 = driver,
               team_2 = team,
               position_2 = racePosition),
      by = character()
    ) %>%
    filter(driver_1 != driver_2) %>%
    merge(
      driver_starting_elos %>%
        select(driver_1 = driver, driver_elo_1 = elo)
    ) %>%
    merge(
      driver_starting_elos %>%
        select(driver_2 = driver, driver_elo_2 = elo)
    ) %>%
    merge(
      team_starting_elos %>%
        select(team_1 = team, team_elo_1 = elo)
    ) %>%
    merge(
      team_starting_elos %>%
        select(team_2 = team, team_elo_2 = elo)
    ) %>%
    mutate(elo_comp_1 = (driver_elo_1 + team_elo_1) / 2,
           elo_comp_2 = (driver_elo_2 + team_elo_2) / 2,
           elo_diff = abs(elo_comp_1 - elo_comp_2)) %>%
    arrange(elo_diff)

  driver_results <- results_matrix %>%
    group_by(driver_1) %>%
    mutate(diff_rank = rank(elo_diff, ties.method = "first")) %>%
    ungroup() %>%
    filter(diff_rank <= min_matches | elo_diff <= match_range) %>%
    mutate(outcome = case_when(grepl("N", position_1) | grepl("N", position_2) ~ 0.5,
                               as.numeric(position_1) < as.numeric(position_2) ~ 1,
                               as.numeric(position_1) > as.numeric(position_2) ~ 0,
                               TRUE ~ 0.5)) %>%
    rowwise() %>%
    mutate(winnings = get_winnings(elo_comp_1, elo_comp_2, k, outcome)) %>%
    ungroup()

  team_results <- results_matrix %>%
    filter(team_1 != team_2) %>%
    group_by(team_1) %>%
    mutate(diff_rank = rank(elo_diff, ties.method = "first")) %>%
    ungroup() %>%
    filter(diff_rank <= min_matches | elo_diff <= match_range) %>%
    mutate(outcome = case_when(grepl("N", position_1) | grepl("N", position_2) ~ 0.5,
                               as.numeric(position_1) < as.numeric(position_2) ~ 1,
                               as.numeric(position_1) > as.numeric(position_2) ~ 0,
                               TRUE ~ 0.5)) %>%
    rowwise() %>%
    mutate(winnings = get_winnings(elo_comp_1, elo_comp_2, k, outcome)) %>%
    ungroup()

  winnings <- driver_results %>%
    group_by(driver_1) %>%
    summarise(elo = max(driver_elo_1),
              winnings = sum(winnings)) %>%
    ungroup() %>%
    mutate(new_elo = elo + winnings) %>%
    bind_rows(
      driver_results %>%
        group_by(team_1) %>%
        summarise(elo = max(team_elo_1),
                  winnings = sum(winnings)) %>%
        ungroup() %>%
        mutate(new_elo = elo + winnings)
    ) %>%
    rename(driver = driver_1, team = team_1)

  return(winnings)

}

get_race_results(test_results, test_elos, test_team_elos, 16) %>% View()
































