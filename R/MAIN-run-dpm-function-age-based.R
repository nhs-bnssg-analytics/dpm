

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

log_and_print_intermediate_results <- function(log_list,
                                               message,
                                               print_intermediate_results){
  # add the message to the loglist
  log_list <- append(log_list, message)
  # print into console if desired
  if(print_intermediate_results){print(message)}
  return(log_list)
}


#' run_dpm main function for running the dpm model
#'
#' takes folder input as used in DPMmicrosim terminology for outputs, so is
#' compatible with that ecosystem
#' @param folder folder to find all information from
#' @param print_intermediate_results boolean if TRUE then outputs as it goes
#' returned with named outputs
#' @export
#' @import dplyr
run_dpm_age_based <- function(folder, print_intermediate_results=T){
  # start by initialising the log list
  log_list <- list()

  # read in the inputs
  inputs <- get_age_based_dpm_inputs_from_folder(folder)

  deaths_method = paste0(
    "Keep CS expected deaths same as current year and don't align to ONS")

  if(is.null(inputs$config$weight_external_moves_based_on_current_pop)){
    warning("as weight_external_moves_based_on_current_pop is NULL in config will assume the default which is FALSE")
    inputs$config$weight_external_moves_based_on_current_pop <- FALSE
  }

  year_range <- (inputs$config$baseline_year+1):inputs$config$final_year

  # get things into same format and naming convention as in in run_dpm function
  initial_population <- inputs$individuals_initial_population_tbl |>
    count(state_name, age, age_group) |>
    rename(initial_pop = n) |>
    complete(state_name, age, age_group, fill = list(initial_pop = 0)) |>
    # remove completed rows where first character of age is not first character
    # of age_group
    filter(stringr::str_sub(age,1,1) == stringr::str_sub(age_group,1,1) |
             # annoying "17-29" category
             (stringr::str_sub(age,1,1)=="1" &
                stringr::str_sub(age_group,1,1)=="2") |
             (stringr::str_sub(age,1,1)=="2" &
                stringr::str_sub(age_group,1,1)=="1")) |>
    # stupid situation of those in their hundreds given 17-29 cat
    filter(!(nchar(age)==3 & age_group=="17-29")) |>
    arrange(age, state_name)

  # work out the proportion of event for each state name and age
  birth_im_em_death_probs <- inputs$birth_im_em_death_probs |>
    group_by(event) |>
    mutate(prop_of_total_event =
             num_people_had_event / sum(num_people_had_event)) |>
    ungroup()
  # misnoma in that we are NOT combining to net migration - we're leaving as
  # immigration and emigration
  births_net_migration_deaths_by_CS <- inputs$ons_birth_im_em_death_nums |>
    select(year, event, ons_num_people=value) |>
    left_join(birth_im_em_death_probs |> select(event, state_name, age,
                                                age_group, prop_of_total_event),
              by = c("event"),
              relationship="many-to-many") |>
    mutate(value = prop_of_total_event * ons_num_people) |>
    select(year, event, state_name, age, age_group, value)


  # create the population table - only first year filled in, by
  # initial_population
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
    # log the start of the year
    log_list <- log_and_print_intermediate_results(
      log_list,
      paste0("*** STARTING YEAR ",i),
      print_intermediate_results)

    inner_trans_long_tbl_i <- inner_trans_long_tbl |> filter(year==i)

    # get the population at beginning of the year
    prev_pop <- population_at_each_year |>  filter(year==i-1) |>  select(-year)

    #####
    # remove the emigrations
    #####

    this_years_emigrations <- births_net_migration_deaths_by_CS |>
      filter(event%in%c("emigrations"),
             year==i) |>
      select(-year)

    if(inputs$config$weight_external_moves_based_on_current_pop){
      this_years_emigrations <- calculate_weighted_birth_im_em_death_probs(
        event_probs = birth_im_em_death_probs |> filter(event=="emigrations"),
        current_pop = prev_pop,
        number_of_events = this_years_emigrations |> summarise(a=sum(value)) |> pull(a),
        chosen_event = "emigrations")
    }

    prev_pop_minus_emigration <-
      left_join(
        prev_pop,
        this_years_emigrations,
        by=c("state_name","age","age_group")) |>
      mutate(population = population-value,
             year = i) |>
      mutate(population = replace_na(population,0)) |>
      select(year, state_name, age, age_group, population)

    # check we haven't overstepped
    if(prev_pop_minus_emigration |> filter(population<0) |>  nrow()){
      num_neg <- prev_pop_minus_emigration |>
        filter(population<0) |>
        summarise(a=sum(population)) |>  pull(a)
      warning(paste0("emigrations led to negative numbers (",abs(round(num_neg)),
                     " in total) - have reset to 0"))
      prev_pop_minus_emigration <- prev_pop_minus_emigration |>
        mutate(population = ifelse(population<0,0,population))
    }
    # count the differences
    pop_prev <- prev_pop |>  summarise(a=sum(population)) |>  pull(a)
    pop_prev_m_e_actual <- prev_pop_minus_emigration |>
      summarise(a=sum(population)) |> pull(a)
    emigrations_actual <- pop_prev - pop_prev_m_e_actual
    emigrations_expected <- births_net_migration_deaths_by_CS |>
      filter(event=="emigrations",year==i) |> summarise(a=sum(value)) |> pull(a)

    # log list
    log_list <- log_and_print_intermediate_results(
      log_list,
      paste0("Emigrations: ",
             round(emigrations_expected)," expected, ",
             round(emigrations_actual)," actual"),
      print_intermediate_results)



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
    died <- prev_pop_minus_emigration_with_transitions |>
      filter(state_name=="Died")
    prev_pop_minus_emigration_with_transitions <-
      prev_pop_minus_emigration_with_transitions |>
      filter(state_name != "Died") |>
      select(state_name, age, age_group, population)

    # log list
    log_list <- log_and_print_intermediate_results(
      log_list,
      paste0("Deaths: ",
             died |> summarise(a=sum(population)) |> pull(a) |>round()),
      print_intermediate_results)


    #####
    # here come some births
    #####


    this_years_births <- births_net_migration_deaths_by_CS |>
      filter(event%in%c("births"),year==i, value!=0) |>
      select(state_name, age, age_group, value)

    if(inputs$config$weight_external_moves_based_on_current_pop){
      this_years_births <- calculate_weighted_birth_im_em_death_probs(
        event_probs = birth_im_em_death_probs |> filter(event=="births"),
        current_pop = prev_pop,
        number_of_events = this_years_births |> summarise(a=sum(value)) |> pull(a),
        chosen_event = "births")
    }

    prev_pop_minus_emigration_with_transitions_and_births <-
      prev_pop_minus_emigration_with_transitions |>
      full_join(this_years_births,
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
    pop_prev_m_e_w_t_a_b <-
      prev_pop_minus_emigration_with_transitions_and_births |>
      summarise(a=sum(population)) |> pull(a)
    births_actual <- pop_prev_m_e_w_t_a_b - pop_prev_m_e_w_t


    # log list
    log_list <- log_and_print_intermediate_results(
      log_list,
      paste0("Births: ",
             round(births_expected)," expected, ",
      round(births_actual)," actual"),
      print_intermediate_results)



    #####
    # here comes some immigration
    #####

    this_years_immigrations <- births_net_migration_deaths_by_CS |>
      filter(event%in%c("immigrations"),year==i, value!=0) |>
      select(state_name, age, age_group, value)

    if(inputs$config$weight_external_moves_based_on_current_pop){
      this_years_immigrations <- calculate_weighted_birth_im_em_death_probs(
        event_probs = birth_im_em_death_probs |> filter(event=="immigrations"),
        current_pop = prev_pop,
        number_of_events = this_years_immigrations |> summarise(a=sum(value)) |> pull(a),
        chosen_event = "immigrations")
    }

    prev_pop_minus_emigration_with_transitions_and_births_and_immigration <-
      prev_pop_minus_emigration_with_transitions_and_births |>
      full_join(this_years_immigrations,
        by=c("state_name","age","age_group")) |>
      mutate(value = replace_na(value,0),
             population = replace_na(population, 0)) |>
      mutate(population = population + value) |>
      select(state_name, age, age_group, population)

    immigrations_expected <-
      births_net_migration_deaths_by_CS |>
      filter(event%in%c("immigrations"),year==i, value!=0) |>
      summarise(a=sum(value)) |> pull(a)
    pop_prev_m_e_w_t_a_b_a_e <-
      prev_pop_minus_emigration_with_transitions_and_births_and_immigration |>
      summarise(a=sum(population)) |> pull(a)
    immigrations_actual <- pop_prev_m_e_w_t_a_b_a_e - pop_prev_m_e_w_t_a_b

    # log list
    log_list <- log_and_print_intermediate_results(
      log_list,
      paste0("Immigrations: ",
             round(immigrations_expected)," expected, ",
             round(immigrations_actual)," actual"),
      print_intermediate_results)

    if(prev_pop_minus_emigration_with_transitions_and_births_and_immigration |>
       pull(population) |>
       min() < 0){
      zero_year_check <- zero_year_check + 1
      # tare it to be 0 as that's the min
      prev_pop_minus_emigration_with_transitions_and_births_and_immigration <-
        prev_pop_minus_emigration_with_transitions_and_births_and_immigration |>
        mutate(population = ifelse(population<0,0,population))
    }
    population_at_each_year <-
      bind_rows(
        population_at_each_year,
        prev_pop_minus_emigration_with_transitions_and_births_and_immigration |>
          mutate(year=i)) |>
      select(year, age, age_group, state_name, population) |>
      arrange(year, age, age_group, state_name)
  }

  return(population_at_each_year)
}

#' function to calculate an adjusted probability matrix for one of immigration/emigration/births
#' given the current population distribution
#' @param event_probs the original event probabilities expected per person
#' @param current_pop the current population distribution
#' @param number_of_events the total number of events we need to decide
#' @param chosen_event one of 'births','immigrations','emigrations'
#' @param simple_output boolean, if TRUE returns minimal cols, with column value as key metric
calculate_weighted_birth_im_em_death_probs <- function(
    event_probs,
    current_pop,
    number_of_events,
    chosen_event,
    simple_output = T){

  # checks and balances
  if(!(chosen_event %in% c("births","immigrations","emigrations"))){
    stop("event must be one of 'births','immigrations','emigrations'")}

  if(!(chosen_event %in% unique(event_probs$event)) | length(unique(event_probs$event))>1){
    stop("event must be in event_probs and only one event in event_probs")
  }

  if(nrow(event_probs) != nrow(current_pop)){
    warning("event_probs not same number of rows as current_pop")
  }

  # now start the process

  # first - calculate how many events we'd expect given the current population
  workings_num_events_per_cs_age <- event_probs |>
    left_join(current_pop, by=c("age","age_group","state_name")) |>
    # the unscaled is the number of events we'd expect given the probs and current
    # population without knowing the total number_of_events
    mutate(num_events_unweighted = prob_of_event_to_individual * population) |>
    mutate(num_events_unweighted=replace_na(num_events_unweighted,0))

  unweighted_total_events <- sum(workings_num_events_per_cs_age$num_events_unweighted)

  # now adjust to get the weighter number of events, so that the proportions
  # in-group stay the same whilst also honouring the current population
  workings_num_events_per_cs_age <- workings_num_events_per_cs_age |>
    mutate(num_events_weighted = num_events_unweighted / unweighted_total_events * number_of_events)

  # make the final cleaned output tbl
  weighted_num_events_per_cs_age  <- workings_num_events_per_cs_age |>
    mutate(event = chosen_event,
           prob_of_event_to_individual = ifelse(population==0,0,num_events_weighted / population),
           method = "weighted event_probs using calculate_weighted_birth_im_em_death_probs") |>
    select(event,
           state_name,
           age,
           age_group,
           method,
           prob_of_event_to_individual,
           num_people_having_event = num_events_weighted,
           num_people_at_time = population)


  # check outputs look ok
  if(abs(sum(weighted_num_events_per_cs_age$num_people_having_event)-number_of_events)>1){
    stop("number of events doesn't match")
  }

  if(simple_output){
    weighted_num_events_per_cs_age <- weighted_num_events_per_cs_age |>
      select(event, state_name, age, age_group, value=num_people_having_event)
  }

  return(weighted_num_events_per_cs_age)
}
