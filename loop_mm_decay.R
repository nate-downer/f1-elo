# analysis loop

#' this loop includes matchmaking and a global decay function
#' there is still work to do refining this process
#' in particular the global decay function applies to all drivers, 
#' not just active ones
#' 
#' a better version of this would add a stronger decay only for drivers that 
#' are not in a given race and then use a more gentle function to
#' control for elo creep that pushes the mean back to 1000
#' 
#' this algorithm also featurers dynamic k values that allow rattings
#' to change more rapidly in seasons with fewer races


## find results to filter out ----

race_res_master %>%
  filter(country == "USA") %>%
  distinct(race, circuitId) %>%
  View()

race_res_master %>%
  group_by(race) %>%
  summarise(drivers = n_distinct(driverId)) %>%
  View()



## dynamic limited run ----

run_elo_mm_decay <- function(start_year, decay_value, base_k_value) {

  start_race <- start_year * 100
  
  # set the parameters
  driver_starting_elos_limit <- race_res_master %>%
    filter(raceOrder > start_race) %>%
    distinct(driver) %>%
    mutate(elo = 1000)

  team_starting_elos_limit <- race_res_master %>%
    filter(raceOrder > start_race) %>%
    distinct(team) %>%
    mutate(elo = 1000)

  races_limit <- race_res_master %>%
    filter(circuitId != 19) %>% #removes indy 500 races
    distinct(raceOrder) %>%
    mutate(prior_race = lag(raceOrder, 1L)) %>%
    mutate(prior_race_text = as.character(prior_race)) %>%
    replace_na(list(prior_race_text = "0")) %>%
    filter(raceOrder > start_race)

  elo_results_limit <- driver_starting_elos_limit %>%
    mutate(elo_type = "driver") %>%
    bind_rows(
      team_starting_elos_limit %>%
        mutate(elo_type = "team")
    ) %>%
    mutate(`0` = 1000)

  k_by_year <- race_res_master %>%
    group_by(year) %>%
    summarise(n_races = n_distinct(raceId)) %>%
    ungroup() %>%
    mutate(mean_races = mean(n_races),
          k_total = mean_races * base_k_value,
          k_per_race = k_total / n_races)


  # run elo functions over all races
  for (this_race in races_limit$raceOrder) {

    this_year <- as.numeric(substr(as.character(this_race), 1, 4))

    k_per_race <- k_by_year %>%
      filter(year == this_year) %>%
      summarise(mean(k_per_race)) %>%
      pull()

    this_race_results <- race_res_master %>%
      filter(raceOrder == this_race)

    current_driver_elos <- elo_results_limit %>%
      filter(elo_type == "driver") %>%
      select(driver, elo = last_col())

    current_team_elos <- elo_results_limit %>%
      filter(elo_type == "team") %>%
      select(team, elo = last_col())

    elo_results_limit <- elo_results_limit %>%
      merge(
        get_race_results(this_race_results, current_driver_elos, current_team_elos, k_per_race) %>%
          select(driver, team, new_elo),
        all.x = TRUE
      ) %>%
      merge(
        elo_results_limit %>%
          select(driver, team, old_elo = last_col())
      ) %>%
      mutate(new_elo = coalesce(new_elo, old_elo)) %>%
      mutate(new_elo = ((new_elo - 1000) * (1 - decay_value)) + 1000) %>% # pushes all values back to 1000 gently
      mutate(raceOrder = as.character(this_race)) %>%
      pivot_wider(values_from = new_elo, names_from = raceOrder) %>%
      select(-old_elo)

    print(this_race)

  }

  return(elo_results_limit)

}

elo_results_mm_decay <- run_elo_mm_decay(1950, 0.01, 32)



## clean the results ----

long_elo_results_limit <- elo_results_mm_decay %>%
  pivot_longer(cols = -c(driver, team, elo, elo_type),
               names_to = "raceOrder",
               values_to = "finish_elo")

tagged_elo_results_limit <- long_elo_results_limit %>%
  merge(
    long_elo_results_limit %>%
      distinct(raceOrder) %>%
      mutate(contRaceOrder = rank(raceOrder))
  ) %>%
  merge(
    race_res_master %>%
      distinct(race, raceOrder)
  ) %>%
  merge(
    race_res_master %>%
      group_by(driver) %>%
      summarise(first_driver_race = min(raceOrder),
                last_driver_race = max(raceOrder)) %>%
      ungroup(),
    all.x = TRUE,
    all.y = TRUE
  ) %>%
  merge(
    race_res_master %>%
      group_by(team) %>%
      summarise(first_team_race = min(raceOrder),
                last_team_race = max(raceOrder)) %>%
      ungroup(),
    all.x = TRUE,
    all.y = TRUE
  ) %>%
  mutate(first_race = if_else(is.na(first_driver_race), first_team_race, first_driver_race),
         last_race = if_else(is.na(last_driver_race), last_team_race, last_driver_race)) %>%
  filter(raceOrder >= first_race) %>%
  filter(raceOrder <= last_race) %>%
  arrange(raceOrder, team, driver) %>%
  mutate(display_elo = round(finish_elo))



## plot best teams over time ----

team_list <- tagged_elo_results_limit %>%
  filter(!is.na(team)) %>%
  group_by(contRaceOrder) %>%
  arrange(desc(display_elo)) %>%
  slice_max(display_elo, n = 1) %>%
  ungroup() %>%
  distinct(team) %>%
  mutate(row_num = as.numeric(row_number()))

tagged_elo_results_limit %>%
  filter(team %in% team_list$team) %>%
  hchart(type = "line",
          hcaes(
            x = as.numeric(contRaceOrder),
            y = display_elo,
            group = team),
          tooltip = list(pointFormat = "<b>{point.team}</b> as of: {point.race}")) %>%
  hc_legend(enabled = FALSE) %>%
  hc_title(text = "Top Team Elo Over Time (Match Making + Decay + Dynamic K)", style = list(fontSize = "14px"))

tagged_elo_results_limit %>%
  filter(!is.na(team)) %>%
  group_by(contRaceOrder) %>%
  arrange(desc(display_elo)) %>%
  slice_max(display_elo, n = 1) %>%
  ungroup() %>%
  group_by(team) %>%
  summarise(peak_elo = max(display_elo),
            races_at_top = n_distinct(contRaceOrder)) %>%
  ungroup() %>%
  hchart(
    type = "point",
    hcaes(x = peak_elo,
          y = races_at_top,
          group = team),
    tooltip = list(pointForman = "")
  )



## plot best drivers over time ----

driver_list <- tagged_elo_results_limit %>%
  filter(!is.na(driver)) %>%
  group_by(contRaceOrder) %>%
  arrange(desc(display_elo)) %>%
  slice_max(display_elo, n = 1) %>%
  ungroup() %>%
  distinct(driver) %>%
  mutate(row_num = as.numeric(row_number()))

tagged_elo_results_limit %>%
  filter(driver %in% driver_list$driver) %>%
  hchart(type = "line",
          hcaes(
            x = as.numeric(contRaceOrder),
            y = display_elo,
            group = driver),
          tooltip = list(pointFormat = "<b>{point.driver}</b> as of: {point.race}")) %>%
  hc_legend(enabled = FALSE) %>%
  hc_title(text = "Top Driver Elo Over Time (Match Making + Decay + Dynamic K)", style = list(fontSize = "14px"))

tagged_elo_results_limit %>%
  filter(!is.na(driver)) %>%
  group_by(contRaceOrder) %>%
  arrange(desc(display_elo)) %>%
  slice_max(display_elo, n = 1) %>%
  ungroup() %>%
  group_by(driver) %>%
  summarise(peak_elo = max(display_elo),
            races_at_top = n_distinct(contRaceOrder)) %>%
  ungroup() %>%
  hchart(
    type = "point",
    hcaes(x = peak_elo,
          y = races_at_top,
          group = driver),
    tooltip = list(pointForman = "")
  )



## additional diagnostic plots ----

tagged_elo_results_limit %>%
  group_by(contRaceOrder) %>%
  summarise(elo_ave = mean(display_elo)) %>%
  ungroup() %>%
  hchart(
    type = "line",
    hcaes(x = contRaceOrder,
          y = elo_ave)
  )





