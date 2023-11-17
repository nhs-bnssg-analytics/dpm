
#' run_dpm main function for running the dpm model
#' @param initial_population initial population tibble
#' @param inner_trans_matrix_list list of inner transition matrices
#' @param total_time integer
#' @param births_net_migration_deaths_figures tibble
#' @param birth_migration_deaths_proportions tibble
#' @export
#' @import dplyr
run_dpm <- function(initial_population,
                    inner_trans_matrix_list,
                    total_time,
                    births_net_migration_deaths_figures,
                    birth_migration_deaths_proportions){

  # number of core segments
  num_cs <- length(unique(initial_population$state_name))

  # need to make sure its a list, because a matrix input is also fine
  # but needs editing
  inner_trans_matrix_list <- check_inner_trans(inner_trans_matrix_list,
                                               total_time)

  # is total_time -1 because the inner_trans_matrix is indexed by what happens
  # NEXT ie at time total_time there is no further transition to be done as it
  # is the end of our window
  if(length(inner_trans_matrix_list) < total_time - 1){
    stop("inner_trans_matrix_list doesn't cover whole time period")
  }

  # criteria for CS values matching inner trans levels
  inner_trans_unique_dims <- inner_trans_matrix_list  %>%
    purrr::map(dim) %>%
    unlist() %>%
    unique()
  if(inner_trans_unique_dims != num_cs){
    stop("Inner Transition Matrix size doesn't align to the Initial Population states given")}

  # new joiners into each CS each year by the 3 events birth/migration/death
  births_net_migration_deaths_by_CS <-
    full_join(
      births_net_migration_deaths_figures,
      birth_migration_deaths_proportions,
      by="event", relationship="many-to-many") |>
    mutate(value = prop * value) |>
    select(year, state_name, event, value)

  # create the population table - only first year filled in, by initial_population
  population_at_each_year <-initial_population |>
    mutate(year=1) |>
    rename(population=initial_pop)

  inner_trans_long_tbl <- from_list_to_long_tbl(inner_trans_matrix_list)
  zero_year_check <- 0
  for (i in 2:total_time) {
    inner_trans_long_tbl_i <- inner_trans_long_tbl %>% filter(year==i-1)

    # take the prev population, minus the deaths
    prev_pop_minus_deaths <- population_at_each_year |>
      filter(year==i-1) |>
      left_join(
        births_net_migration_deaths_by_CS |> filter(event=="deaths",year==i-1),
        by=c("year","state_name")) |>
      mutate(pop_minus_deaths = population - value) |>
      select(state_name, pop_minus_deaths)

    prev_pop_into_new <- prev_pop_minus_deaths |>
      right_join(inner_trans_long_tbl_i, by = c("state_name"="from")) %>%
      mutate(amount_into_to = pop_minus_deaths * transition_prop) %>%
      group_by(to) %>%
      summarise(amount_into = sum(amount_into_to)) %>%
      rename(state_name = to)

    new_population <-
      prev_pop_into_new %>%
      left_join(
        births_net_migration_deaths_by_CS |>
          filter(event%in%c("births","net_migration"),
                        year==i-1) |>
          group_by(state_name) |>
          summarise(amount_from_births_net_migration = sum(value), .groups="drop"),
        by=c("state_name")) |>
      mutate(population = amount_into + amount_from_births_net_migration,
             year = i) %>%
      select(year, state_name, population)

    if(min(new_population$population) < 0){
      zero_year_check <- zero_year_check + 1
      # tare it to be 0 as that's the min
      new_population <- new_population %>%
        mutate(population = ifelse(population<0,0,population))
    }
    population_at_each_year <-
      bind_rows(population_at_each_year,
                new_population) %>%
      select(year, state_name, population) %>%
      arrange(year, state_name)
  }


  if(zero_year_check){
    warning(paste0("there were ", zero_year_check, " years a Core Segment has ",
                   "gone to population 0. Model invalid."))
  }

  return(population_at_each_year)
}
