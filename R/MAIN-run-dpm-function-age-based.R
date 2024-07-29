

#' function for reading in all .RDS inputs of age-based DPM into
#' @param folder file path for folder inputs from DPMmicrosim age-based model
#' @export
get_age_based_dpm_inputs_from_folder <- function(folder){

  # identify all the .rds files in the folder
  files <- list.files(folder, pattern = ".rds", full.names = TRUE)

  # read the files into a list, named with the file name
  inputs <- purrr::map(files,readRDS) |>
    purrr::set_names(basename(files))

  # remove .rds from the list names in inputs
  names(inputs) <- gsub(".rds","",names(inputs))

  # check for config file one level up from folder
  config_file <- list.files(dirname(folder),
                            pattern = ".yml|.yaml", full.names = TRUE)
  if(length(config_file==1)){
    inputs$config <- yaml::read_yaml(config_file)
  } else {
    stop("No unique config file found in parent directory of inputs folder")
  }

  return(inputs)
}


#' run_dpm main function for running the dpm model
#' @param folder folder to find all information from
#' @param return_births_net_migration_deaths_in_output boolean if TRUE then list returned with named outputs
#' @export
#' @import dplyr
run_dpm_age_based <- function(folder){
  # read in the inputs
  inputs <- get_age_based_dpm_inputs_from_folder(folder)

  deaths_method = "Keep CS expected deaths same as current year and don't align to ONS"

  year_range <- (inputs$config$baseline_year+1):inputs$config$final_year

  # get things into same format and naming convention as in in run_dpm function
  initial_population <- inputs$individuals_initial_population_tbl |>
    count(state_name, age, age_group) |>
    rename(initial_pop = n) |>
    complete(state_name, age, age_group, fill = list(initial_pop = 0)) |>
    # remove completed rows where first character of age is not first character of age_group
    filter(stringr::str_sub(age,1,1) == stringr::str_sub(age_group,1,1) |
             # annoying "17-29" category
             (stringr::str_sub(age,1,1)=="1" & stringr::str_sub(age_group,1,1)=="2") |
             (stringr::str_sub(age,1,1)=="2" & stringr::str_sub(age_group,1,1)=="1")) |>
    # stupid situation of those in their hundreds given 17-29 cat
    filter(!(nchar(age)==3 & age_group=="17-29")) |>
    arrange(age, state_name)

  # work out the proportion of event for each state name and age
  birth_im_em_death_probs <- inputs$birth_im_em_death_probs |>
    group_by(event) |>
    mutate(prop_of_total_event = num_people_had_event / sum(num_people_had_event)) |>
    ungroup()
  # misnoma in that we are NOT combining to net migration - we're leaving as immigration and emigration
  births_net_migration_deaths_by_CS <- inputs$ons_birth_im_em_death_nums |>
    select(year, event, ons_num_people=value) |>
    left_join(birth_im_em_death_probs |> select(event, state_name, age, age_group, prop_of_total_event),
              by = c("event"),
              relationship="many-to-many") |>
    mutate(value = prop_of_total_event * ons_num_people) |>
    select(year, event, state_name, age, age_group, value)


  # create the population table - only first year filled in, by initial_population
  population_at_each_year <-initial_population |>
    mutate(year=inputs$config$baseline_year) |>
    rename(population=initial_pop)

  inner_trans_long_tbl <- inputs$trans_probs_tbl |>
    select(from=core_seg_prev_name,
           to=core_seg_orig_name,
           age_group,
           transition_prop = prop) |>
    expand_grid(year=year_range)

  zero_year_check <- 0
  for (i in year_range) {
    print(paste0("starting year",i))

    inner_trans_long_tbl_i <- inner_trans_long_tbl |> filter(year==i)

    # get the population at beginning of the year
    prev_pop <- population_at_each_year |>  filter(year==i-1) |>  select(-year)

    #####
    # remove the emigrations
    #####

    prev_pop_minus_emigration <-
      left_join(
        prev_pop,
        births_net_migration_deaths_by_CS |>
          filter(event%in%c("emigrations"),
                 year==i) |>
          select(-year),
        by=c("state_name","age","age_group")) |>
      mutate(population = population-value,
             year = i) |>
      select(year, state_name, age, age_group, population)

    # check we haven't oversteppd
    if(prev_pop_minus_emigration |> filter(population<0) |>  nrow()){
      num_neg <- prev_pop_minus_emigration |>
        filter(population<0) |>
        summarise(a=sum(population)) |>  pull(a)
      warning(paste0("emigrations led to negative numbers (",abs(round(num_neg)), " in total) - have reset to 0"))
      prev_pop_minus_emigration <- prev_pop_minus_emigration
    }
    # count the differences
    pop_prev <- prev_pop |>  summarise(a=sum(population)) |>  pull(a)
    pop_prev_m_e_actual <- prev_pop_minus_emigration |>
      summarise(a=sum(population)) |> pull(a)
    emigrations_actual <- pop_prev - pop_prev_m_e_actual
    emigrations_expected <- births_net_migration_deaths_by_CS |>
      filter(event=="emigrations",year==i) |> summarise(a=sum(value)) |> pull(a)

    print(paste0(round(emigrations_expected)," expected"))
    print(paste0(round(emigrations_actual)," actual"))

    #####
    # sample health state change (including deaths)
    #####

    prev_pop_minus_emigration_with_transitions <- prev_pop_minus_emigration |>
      right_join(inner_trans_long_tbl_i, by = c("state_name"="from",
                                                "age_group"),
                 relationship = "many-to-many") |>
      mutate(amount_into_to = population * transition_prop) |>
      group_by(to, age, age_group) |>
      summarise(population = sum(amount_into_to), .groups="drop") |>
      rename(state_name = to) |>
      # make them a year older :)
      mutate(age = age+1) |>
      select(-age_group) |>
      dpm:::add_age_group_column() |>
      dpm:::convert_to_decade("age_group")

    # remove the deaths
    died <- prev_pop_minus_emigration_with_transitions |> filter(state_name=="Died")
    prev_pop_minus_emigration_with_transitions <-
      prev_pop_minus_emigration_with_transitions |>
      filter(state_name != "Died")

    print(paste0("Deaths: ",died |> summarise(a=sum(population)) |> pull(a) |>round()))

    #####
    # here come some births
    #####

    prev_pop_minus_emigration_with_transitions_and_births <-
      prev_pop_minus_emigration_with_transitions |>
      full_join(
        births_net_migration_deaths_by_CS |>
          filter(event%in%c("births"),year==i, value!=0) |>
          select(state_name, age, age_group, value),
        by=c("state_name","age","age_group")) |>
      mutate(value = replace_na(value,0),
             population = replace_na(population,0)) |>
      mutate(population = population + value) |>
      select(state_name, age, age_group, population)

    births_expected <-
      births_net_migration_deaths_by_CS |>
      filter(event%in%c("births"),year==i, value!=0) |>
      summarise(a=sum(value)) |> pull(a)
    pop_prev_m_e_w_t <- prev_pop_minus_emigration_with_transitions |>
      summarise(a=sum(population)) |> pull(a)
    pop_prev_m_e_w_t_a_b <- prev_pop_minus_emigration_with_transitions_and_births |>
      summarise(a=sum(population)) |> pull(a)
    births_actual <- pop_prev_m_e_w_t_a_b - pop_prev_m_e_w_t

    print(paste0(round(births_expected)," births expected"))
    print(paste0(round(births_actual)," births actual"))


    #####
    # here comes some immigration
    #####

    prev_pop_minus_emigration_with_transitions_and_births_and_immigration <-
      prev_pop_minus_emigration_with_transitions_and_births |>
      full_join(
        births_net_migration_deaths_by_CS |>
          filter(event%in%c("immigrations"),year==i, value!=0) |>
          select(state_name, age, age_group, value),
        by=c("state_name","age","age_group")) |>
      mutate(value = replace_na(value,0),
             population = replace_na(population, 0)) |>
      mutate(population = population + value) |>
      select(state_name, age, age_group, population)

    immigrations_expected <-
      births_net_migration_deaths_by_CS |>
      filter(event%in%c("immigrations"),year==i, value!=0) |>
      summarise(a=sum(value)) |> pull(a)
    pop_prev_m_e_w_t_a_b_a_e <- prev_pop_minus_emigration_with_transitions_and_births_and_immigration |>
      summarise(a=sum(population)) |> pull(a)
    immigrations_actual <- pop_prev_m_e_w_t_a_b_a_e - pop_prev_m_e_w_t_a_b

    print(paste0(round(immigrations_expected)," immigrations expected"))
    print(paste0(round(immigrations_actual)," immigrations actual"))

    if(min(prev_pop_minus_emigration_with_transitions_and_births_and_immigration$population) < 0){
      zero_year_check <- zero_year_check + 1
      # tare it to be 0 as that's the min
      prev_pop_minus_emigration_with_transitions_and_births_and_immigration <-
        prev_pop_minus_emigration_with_transitions_and_births_and_immigration |>
        mutate(population = ifelse(population<0,0,population))
    }
    population_at_each_year <-
      bind_rows(population_at_each_year,
                prev_pop_minus_emigration_with_transitions_and_births_and_immigration |>  mutate(year=i)) |>
      select(year, age, age_group, state_name, population) |>
      arrange(year, age, age_group, state_name)
  }

  return(population_at_each_year)
}
