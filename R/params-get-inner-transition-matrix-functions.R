


#' applies create_monthly_transition_rate over multiple months
#' @param sql_con connection to Microsoft SQL server object
#' @param months_to_go_through vector of months to go through
#' @param compare_against the number of months back to look
#' @param min_age minimum age of patient
#' @param method an integer, see get_transition_numbers
#' @param age_group boolean, include the 5 year age bands or not
#' @import purrr
create_many_monthly_transitions <- function(sql_con,
                                            months_to_go_through,
                                            compare_against=12,
                                            min_age=17,
                                            method = 1,
                                            age_groups=F){
  monthly_transition_rates <- purrr::map(
    months_to_go_through,
    function(x) get_transition_numbers(sql_con = sql_con,
                                       method = method,
                                       orig_month_start_date = x,
                                       compare_against = compare_against,
                                       age_groups = age_groups)) |>
    bind_rows()
}

#' Calculate all the transitions between two months
#' @param sql_con connection to SQL database
#' @param orig_month_start_date month interested in date format, day of month is not considered
#' @param method options for which method to use
#' @param compare_against the number of months back to look
#' @param min_age the minimum age to consider. Default 17
#' @export
get_transition_numbers <- function(sql_con,
                                   orig_month_start_date,
                                   method = 1,
                                   compare_against = 12,
                                   min_age = 17,
                                   age_groups=F){

  # error messages for rogue compare_against entries
  if(compare_against < 1 | !(compare_against==round(compare_against))){
    stop("compare_against is number of months into past to compare against, must be >0 integer")
  }
  orig_month_start_date_char <- format(orig_month_start_date,"%Y-%m-%d")
  prev_month_start_date <- orig_month_start_date - months(compare_against)
  prev_month_start_date_char <- format(prev_month_start_date,"%Y-%m-%d")
  if(prev_month_start_date < as_date("2020-01-01")){
    stop("data doesn't go that far back to compare")
  }

  # method
  # Methods
  method_options <- c(
    "1"  = "Using only patients with full records of data",
    "2"  = "Using all possible CMS data points",
    "3"  = "Using only patients with full data between prev_month_start_date and orig_month_start_date_char"
  )
  if(!(method %in% append(method_options,names(method_options)))){
    stop(paste0(
      "\n'method' input must be one of these options:\n  -",
      paste0(method_options,collapse="\n  -"),
      "\nor a number to indicate which one of the above to pick"))}
  if(is.numeric(method)){method = unname(method_options[method])}
  print(paste0("Getting initial population using method: ",method))


  if(method %in% c("Using only patients with full data between prev_month_start_date and orig_month_start_date_char")){
    stop("Not implemented for that method yet")}

  if(method == "Using all possible CMS data points"){
    source_new_cambridge_score <- get_sql_table_source_new_cambridge_score(sql_con)
    data_to_use <- source_new_cambridge_score
  }

  if(method == "Using only patients with full records of data"){
    # get the two tables we use
    source_clean_nhs_numbers <- get_sql_table_source_clean_nhs_numbers(sql_con)
    source_new_cambridge_score <- get_sql_table_source_new_cambridge_score(sql_con)

    data_to_use <- source_clean_nhs_numbers |> left_join(source_new_cambridge_score, by = "nhs_number")
  }
  source_deaths <- get_sql_table_source_deaths(sql_con)

  # get the data frame we are interested in and the one from the past
  orig_month_df <- data_to_use |>
    filter(attribute_period == orig_month_start_date_char,
           age >= min_age)
  prev_month_df <- data_to_use |>
    filter(attribute_period == prev_month_start_date_char,
           age >= min_age)

  if(age_groups){
    comparison_df <- full_join(orig_month_df |> select(nhs_number,attribute_period, segment, age),
                               prev_month_df |> select(nhs_number,attribute_period, segment, age),
                               by = "nhs_number", suffix = c("_orig","_prev"))
    comparison_df <- collect(comparison_df)

    comparison_df <- comparison_df %>%
      add_age_group_column(age_column_name = "age_orig") %>%
      add_age_group_column(age_column_name = "age_prev") %>%
      # important to have it grouped for the next bit (summarising)
      group_by(segment_orig, segment_prev, age_orig_group, age_prev_group)
  } else {
    # only take 3 columns we need
    comparison_df <- full_join(orig_month_df |> select(nhs_number,attribute_period, segment),
                               prev_month_df |> select(nhs_number,attribute_period, segment),
                               by = "nhs_number", suffix = c("_orig","_prev"))
    comparison_df <- collect(comparison_df) %>%
      group_by(segment_orig, segment_prev)
  }

  core_seg_change <- comparison_df |>
    # number of people moving from segment_prev to segment_orig
    summarise(num_people = n(), .groups="drop") |>
    mutate(state_name_prev = paste0("CS",segment_prev),
           state_name_orig = paste0("CS",segment_orig)) |>
    # sort out the NAs
    mutate(state_name_prev = ifelse(state_name_prev=="CSNA",
                                    "assumed_birth_or_migrate",
                                    state_name_prev),
           state_name_orig = ifelse(state_name_orig=="CSNA",
                                    "assumed_death_or_migrate",
                                    state_name_orig)) |>
    # we need to keep track of from/to for months if the data set is to be joined onto others
    mutate(prev_month_start_date=prev_month_start_date,
           orig_month_start_date=orig_month_start_date)
  if(age_groups){
    core_seg_change <- core_seg_change %>%
      mutate(transition_name = paste0("Transition",segment_prev,"to",segment_orig,
                                      "_and_",age_prev_group,"to",age_orig_group)) |>
      select(
        prev_month_start_date,
        orig_month_start_date,
        segment_prev,
        state_name_prev,
        segment_orig,
        state_name_orig,
        transition_name,
        age_prev_group,
        age_orig_group,
        num_people
      )

  } else {
    core_seg_change <- core_seg_change %>%
      mutate(transition_name = paste0("Transition",segment_prev,"to",segment_orig)) %>%
      select(
        prev_month_start_date,
        orig_month_start_date,
        segment_prev,
        state_name_prev,
        segment_orig,
        state_name_orig,
        transition_name,
        num_people
      )
  }

  # add in births and deaths
  born_core_seg_change <- create_births_by_cs(data_to_use,
                                              orig_month_start_date,
                                              prev_month_start_date,
                                              min_age,
                                              age_groups=age_groups)
  died_core_seg_change <- create_deaths_by_cs(data_to_use,
                                              source_deaths,
                                              orig_month_start_date,
                                              prev_month_start_date,
                                              min_age,
                                              age_groups=age_groups)

  final_out <- core_seg_change |>
    bind_rows(born_core_seg_change) |>
    bind_rows(died_core_seg_change) |>
    mutate(method = method) |>
    arrange(segment_prev, segment_orig)

  if(age_groups){
    #decsiion to use decades as Age Groups, not the ONS 5 year bands
    final_out <- final_out |>
      convert_to_decade("age_prev_group") |>
      convert_to_decade("age_orig_group")

    # summarise across new age groups
    final_out <- final_out %>%
      mutate(transition_name = paste0("Transition",segment_prev,"to",segment_orig,
                                      "_and_",age_prev_group,"to",age_orig_group)) |>
      group_by(
        prev_month_start_date,
        orig_month_start_date,
        segment_prev,
        state_name_prev,
        segment_orig,
        state_name_orig,
        transition_name,
        age_prev_group,
        age_orig_group) %>%
      summarise(num_people = sum(num_people),.groups="drop")
  }

  return(final_out)
}

# subfunction of get_transition_numbers
create_births_by_cs <- function(data_to_use,
                                orig_month_start_date,
                                prev_month_start_date,
                                min_age,
                                age_groups=F){

  # previous one is anyone UNDER the age ie not born
  prev_month_df <- data_to_use |>
    filter(attribute_period == as.character(prev_month_start_date),
           age < min_age)
  # original one is anyone OVER the age ie born
  orig_month_df <- data_to_use |>
    filter(attribute_period == as.character(orig_month_start_date),
           age >= min_age)

  born <- inner_join(orig_month_df |> select(nhs_number,attribute_period, segment, age),
                     prev_month_df |> select(nhs_number,attribute_period),
                     by = "nhs_number", suffix = c("_orig","_prev"))

  if(age_groups){
    born_core_seg_change <- born |>
      count(attribute_period_prev, attribute_period_orig, segment, age) |>
      collect()

    born_core_seg_change <- born_core_seg_change %>%
      add_age_group_column(age_column_name = "age") %>%
      group_by(attribute_period_prev, attribute_period_orig, segment, age_group) %>%
      summarise(n=sum(n),.groups="drop") %>%
      mutate(state_name_prev = "Born",
             segment_prev = as.numeric(NA),
             age_prev_group = as.factor(NA),
             age_orig_group = age_group,
             state_name_orig = paste0("CS",segment)) |>
      mutate(prev_month_start_date=as_date(attribute_period_prev),
             orig_month_start_date=as_date(attribute_period_orig),
             segment_orig = segment,
             num_people = n) |>
      mutate(transition_name = paste0("Transition",segment_prev,"to",segment_orig,
                                      "_and_",age_prev_group,"to",age_orig_group)) |>
      select(prev_month_start_date,
             orig_month_start_date,
             segment_prev,
             state_name_prev,
             segment_orig,
             state_name_orig,
             transition_name,
             age_prev_group,
             age_orig_group,
             num_people)


  } else {
    born_core_seg_change <- born |>
      count(attribute_period_prev, attribute_period_orig, segment) |>
      collect() |>
      mutate(state_name_prev = "Born",
             segment_prev = as.numeric(NA),
             state_name_orig = paste0("CS",segment)) |>
      mutate(prev_month_start_date=as_date(attribute_period_prev),
             orig_month_start_date=as_date(attribute_period_orig),
             segment_orig = segment,
             num_people = n) |>
      mutate(transition_name = paste0("Transition",segment_prev,"to",segment_orig)) |>
      select(prev_month_start_date,
             orig_month_start_date,
             segment_prev,
             state_name_prev,
             segment_orig,
             state_name_orig,
             transition_name,
             num_people)
  }

  return(born_core_seg_change)
}

# subfunction of get_transition_numbers
create_deaths_by_cs <- function(data_to_use,
                                source_deaths,
                                orig_month_start_date,
                                prev_month_start_date,
                                min_age,
                                age_groups=F){
  # previous one is anyone who was born and alive at the time
  prev_month_df <- data_to_use |>
    filter(attribute_period == as.character(prev_month_start_date),
           age >= min_age)
  # find out who has died in the period
  # important for filtering later
  last_day_max_month <- format(
    as_date(orig_month_start_date) + months(1) - days(1),"%Y-%m-%d")
  # who has died in the period
  died_in_period <- source_deaths |>
    filter(REG_DATE_OF_DEATH >= prev_month_start_date,
           REG_DATE_OF_DEATH <= last_day_max_month) |>
    mutate(nhs_number = as.character(Derived_Pseudo_NHS)) |>
    select(nhs_number, death_date = REG_DATE_OF_DEATH)


  if(age_groups){
    # who was alive who is now dead
    died_core_seg_change <-
      # who was alive at beginning of period
      prev_month_df |>
      select(nhs_number, segment, age) |>
      # who then died
      inner_join(died_in_period, by="nhs_number") |>
      # count what segment they were in
      count(segment,age) |>
      # tidy up
      collect()

    died_core_seg_change <- died_core_seg_change %>%
      add_age_group_column(age_column_name = "age") %>%
      group_by(segment, age_group) %>%
      summarise(n=sum(n),.groups="drop") %>%
      mutate(prev_month_start_date=prev_month_start_date,
             orig_month_start_date=orig_month_start_date,
             segment_prev = segment,
             segment_orig = as.numeric(NA),
             state_name_prev = paste0("CS",segment_prev),
             state_name_orig = "Died",
             age_prev_group = age_group,
             age_orig_group = as.character(NA),
             num_people = n) |>
      mutate(transition_name = paste0("Transition",segment_prev,"to",segment_orig,
                                      "_and_",age_prev_group,"to",age_orig_group)) |>
      select(prev_month_start_date,
             orig_month_start_date,
             segment_prev,
             state_name_prev,
             segment_orig,
             state_name_orig,
             age_prev_group,
             age_orig_group,
             transition_name,
             num_people)

  } else {
    # who was alive who is now dead
    died_core_seg_change <-
      # who was alive at beginning of period
      prev_month_df |>
      select(nhs_number, segment) |>
      # who then died
      inner_join(died_in_period, by="nhs_number") |>
      # count what segment they were in
      count(segment) |>
      # tidy up
      collect() |>
      mutate(prev_month_start_date=prev_month_start_date,
             orig_month_start_date=orig_month_start_date,
             segment_prev = segment,
             segment_orig = as.numeric(NA),
             state_name_prev = paste0("CS",segment_prev),
             state_name_orig = "Died",
             num_people = n) |>
      mutate(transition_name = paste0("Transition",segment_prev,"to",segment_orig)) |>
      select(prev_month_start_date,
             orig_month_start_date,
             segment_prev,
             state_name_prev,
             segment_orig,
             state_name_orig,
             transition_name,
             num_people)
  }

  return(died_core_seg_change)
}

#' takes output from get_transition_numbers and creates the inner transitions matrix
#' out_type can either be 'matrix' or 'tbl"
#' @param transitions tibble output from get_transition_numbers
#' @param out_type either 'matrix' or 'tbl' depending on your flAvah
#' @export
get_inner_trans_rate_from_transitions_tbl <- function(transitions,
                                                      out_type = "matrix"){
  if(!(out_type %in% c("matrix","tbl"))){stop("out_type must be matrix or tbl")}

  inner_transitions_tbl <- transitions |>
    # remove NAs as we're plotting the INNER transitions
    filter(!is.na(segment_orig),!is.na(segment_prev)) |>
    # calculate the proportion of that transition rate that moved - this is the transition rate
    group_by(prev_month_start_date, state_name_prev) |>
    mutate(prop = num_people / sum(num_people)) |>
    ungroup()

  if(out_type == "tbl"){return(inner_transitions_tbl)}
  if(out_type == "matrix"){
    inner_trans_matrix <- inner_transitions_tbl |>
      select(state_name_prev, state_name_orig, prop) |>
      arrange(state_name_prev, state_name_orig) |>
      tidyr::pivot_wider(names_from = state_name_orig, values_from = prop) |>
      select(-state_name_prev) |>
      as.matrix()
    return(inner_trans_matrix)}
}

