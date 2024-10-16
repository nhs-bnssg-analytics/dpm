
#' run_dpm main function for running the dpm model
#' @param initial_population initial population tibble
#' @param inner_trans_matrix_list list of inner transition matrices
#' @param total_time integer
#' @param births_net_migration_deaths_figures tibble
#' @param birth_migration_deaths_proportions tibble
#' @param deaths_method integer or string to decide how deaths are handled. Options are
#' 1 = "Keep CS split within deaths same as current year",
#' 2 = "Keep CS expected deaths same as current year but scale to ONS numbers",
#' 3 = "Keep CS expected deaths same as current year and don't align to ONS"
#' @param return_births_net_migration_deaths_in_output boolean if TRUE then list returned with named outputs
#' @export
#' @import dplyr
run_dpm <- function(initial_population,
                    inner_trans_matrix_list,
                    total_time,
                    births_net_migration_deaths_figures,
                    birth_migration_deaths_proportions,
                    deaths_method = 1,
                    return_births_net_migration_deaths_in_output = FALSE){

  # number of core segments
  num_cs <- length(unique(initial_population$state_name))

  # need to make sure its a list, because a matrix input is also fine
  # but needs editing
  inner_trans_matrix_list <- check_inner_trans(inner_trans_matrix_list,
                                               total_time)


  if(length(inner_trans_matrix_list) < total_time){
    stop("inner_trans_matrix_list doesn't cover whole time period")
  }

  # criteria for CS values matching inner trans levels
  inner_trans_unique_dims <- inner_trans_matrix_list  |>
    purrr::map(dim) |>
    unlist() |>
    unique()
  if(inner_trans_unique_dims != num_cs){
    stop("Inner Transition Matrix size doesn't align to the Initial Population states given")}


  # Methods for number dead
  deaths_method_options <- c(
    "1"  = "Keep CS split within deaths same as current year",
    "2"  = "Keep CS expected deaths same as current year but scale to ONS numbers",
    "3"  = "Keep CS expected deaths same as current year and don't align to ONS"
  )
  if(!(deaths_method %in% append(deaths_method_options,names(deaths_method_options)))){
    stop(paste0(
      "\n'deaths_method' input must be one of these options:\n  -",
      paste0(deaths_method_options,collapse="\n  -"),
      "\nor a number to indicate which one of the above to pick"))}
  if(is.numeric(deaths_method)){deaths_method = unname(deaths_method_options[deaths_method])}
  print(paste0("Running DPM with deaths_method: ",deaths_method))


  if(deaths_method == "Keep CS split within deaths same as current year"){
    # new joiners into each CS each year by the 3 events birth/migration/death
    births_net_migration_deaths_by_CS <-
      full_join(
        births_net_migration_deaths_figures,
        birth_migration_deaths_proportions,
        by="event", relationship="many-to-many") |>
      mutate(value = prop * value) |>
      select(year, state_name, event, value)
  } else {
    # only do first year or deaths by CS, as future years are dependent on
    # population split year before.

    total_deaths_first_year <-  births_net_migration_deaths_figures %>%
      filter(event=="deaths", year == min(year)) %>%
      pull(value)

    # first work out proportion of people in each CS who died last year
    expected_deaths_by_CS <- birth_migration_deaths_proportions %>%
      filter(event=="deaths") %>%
      left_join(initial_population,by="state_name") %>%
      mutate(num_expected_deaths = prop*total_deaths_first_year,
             year = 1) %>%
      mutate(prop_cs_that_die = num_expected_deaths/initial_pop,
             prev_pop = initial_pop,
             event = "expected_deaths") %>%
      select(year, state_name, event, value = num_expected_deaths, prop_cs_that_die, prev_pop)

    prop_cs_that_die <- expected_deaths_by_CS %>%
      select(state_name, prop_cs_that_die)

    # later on this won't be the case, but at initialisation it is that expected
    # deaths equals ONS deaths
    expected_deaths_by_CS <- bind_rows(
      expected_deaths_by_CS,
      expected_deaths_by_CS %>% mutate(event="ons_scaled_deaths"))

    births_net_migration_deaths_by_CS <- bind_rows(
      expected_deaths_by_CS,
      full_join(
        births_net_migration_deaths_figures %>% filter(event!="deaths"),
        birth_migration_deaths_proportions %>% filter(event!="deaths"),
        by="event", relationship="many-to-many") |>
        mutate(value = prop * value)) |>
      arrange(year, state_name, event, value) %>%
      select(year, state_name, event, value)
  }

  # Decide which values are being used for 'deaths'
  if(deaths_method == "Keep CS expected deaths same as current year but scale to ONS numbers"){
    births_net_migration_deaths_by_CS <- bind_rows(
      births_net_migration_deaths_by_CS,
      births_net_migration_deaths_by_CS %>% filter(event=="ons_scaled_deaths") %>% mutate(event="deaths"))
  }
  if(deaths_method == "Keep CS expected deaths same as current year and don't align to ONS"){
    warning("due to selected deaths_method, the deaths values in births_net_migration_deaths_figures is being ignored")
    births_net_migration_deaths_by_CS <- bind_rows(
      births_net_migration_deaths_by_CS,
      births_net_migration_deaths_by_CS %>% filter(event=="expected_deaths") %>% mutate(event="deaths"))
  }


  # create the population table - only first year filled in, by initial_population
  population_at_each_year <-initial_population |>
    mutate(year=0) |>
    rename(population=initial_pop)

  inner_trans_long_tbl <- from_list_to_long_tbl(inner_trans_matrix_list)
  zero_year_check <- 0
  for (i in 1:total_time) {
    inner_trans_long_tbl_i <- inner_trans_long_tbl |> filter(year==i)

    # take the prev population, minus the deaths that have occurred in the year to the new pop
    prev_pop_minus_deaths <- population_at_each_year |>
      filter(year==i-1) |>
      select(-year) |>
      left_join(
        births_net_migration_deaths_by_CS |> filter(event=="deaths",year==i),
        by=c("state_name")) |>
      mutate(pop_minus_deaths = population - value) |>
      select(state_name, pop_minus_deaths)

    # people that stayed in population year i-1 to i, what state_name do they move between
    prev_pop_into_new <- prev_pop_minus_deaths |>
      right_join(inner_trans_long_tbl_i, by = c("state_name"="from")) |>
      mutate(amount_into_to = pop_minus_deaths * transition_prop) |>
      group_by(to) |>
      summarise(amount_into = sum(amount_into_to)) |>
      rename(state_name = to)

    # combine calcs to get year i population
    new_population <-
      prev_pop_into_new |>
      left_join(
        births_net_migration_deaths_by_CS |>
          filter(event%in%c("births","net_migration"),
                 year==i) |>
          group_by(state_name) |>
          summarise(amount_from_births_net_migration = sum(value), .groups="drop"),
        by=c("state_name")) |>
      mutate(population = amount_into + amount_from_births_net_migration,
             year = i) |>
      select(year, state_name, population)

    # Work out how many are expected to die next year
    if(deaths_method %in% c("Keep CS expected deaths same as current year but scale to ONS numbers",
                            "Keep CS expected deaths same as current year and don't align to ONS")){
      # calculate the new expected deaths for year i+1
      new_expected_deaths_by_cs <- new_population %>%
        mutate(year = year+1) %>% rename(prev_pop = population) %>%
        left_join(prop_cs_that_die, by = "state_name") %>%
        mutate(expected_deaths = prop_cs_that_die * prev_pop) %>%
        select(year, state_name, expected_deaths, prop_cs_that_die, prev_pop)

      # work out deaths that year according to ONS
      total_deaths <- births_net_migration_deaths_figures %>% filter(event=="deaths",year==i) %>% pull(value)
      # scale the expected deaths to ONS numbers
      new_expected_deaths_by_cs <- new_expected_deaths_by_cs %>%
        mutate(prop_total_expected_deaths = expected_deaths / sum(expected_deaths)) %>%
        mutate(ons_scaled_deaths = prop_total_expected_deaths*total_deaths) %>%
        select(year, state_name, prop_cs_that_die, prev_pop, expected_deaths, ons_scaled_deaths) %>%
        tidyr::pivot_longer(cols = c("expected_deaths", "ons_scaled_deaths"), names_to = "event", values_to = "value")

      expected_deaths_by_CS <- bind_rows(expected_deaths_by_CS, new_expected_deaths_by_cs)

      births_net_migration_deaths_by_CS <- bind_rows(
        births_net_migration_deaths_by_CS %>% filter(!stringr::str_detect(event,"deaths")),
        expected_deaths_by_CS %>% select(-prop_cs_that_die,-prev_pop)) %>%
        arrange(year, state_name, event)

      # Decide which values are being used for 'deaths'
      if(deaths_method == "Keep CS expected deaths same as current year but scale to ONS numbers"){
        births_net_migration_deaths_by_CS <- bind_rows(
          births_net_migration_deaths_by_CS,
          births_net_migration_deaths_by_CS %>% filter(event=="ons_scaled_deaths") %>% mutate(event="deaths"))
      }
      if(deaths_method == "Keep CS expected deaths same as current year and don't align to ONS"){
        births_net_migration_deaths_by_CS <- bind_rows(
          births_net_migration_deaths_by_CS,
          births_net_migration_deaths_by_CS %>% filter(event=="expected_deaths") %>% mutate(event="deaths"))
      }
    }

    if(min(new_population$population) < 0){
      zero_year_check <- zero_year_check + 1
      # tare it to be 0 as that's the min
      new_population <- new_population |>
        mutate(population = ifelse(population<0,0,population))
    }
    population_at_each_year <-
      bind_rows(population_at_each_year,
                new_population) |>
      select(year, state_name, population) |>
      arrange(year, state_name)
  }


  if(zero_year_check){
    warning(paste0("there were ", zero_year_check, " years a Core Segment has ",
                   "gone to population 0. Model invalid."))
  }

  if(return_births_net_migration_deaths_in_output){
    return(list("population_at_each_year" = population_at_each_year,
                "births_net_migration_deaths_by_CS" = births_net_migration_deaths_by_CS))
  } else {return(population_at_each_year)}
}
