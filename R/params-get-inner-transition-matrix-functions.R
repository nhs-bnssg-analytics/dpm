


#' applies create_monthly_transition_rate over multiple months
#' @param source_new_cambridge_score connection to Microsoft SQL server
#' @param months_to_go_through vector of months to go through
#' @param compare_against the number of months back to look
create_many_monthly_transitions <- function(source_new_cambridge_score,
                                            source_deaths,
                                            months_to_go_through,
                                            compare_against=12,
                                            min_age=17,
                                            method = 1){
  monthly_transition_rates <- map(
    months_to_go_through,
    function(x) create_monthly_transitions(source_new_cambridge_score = source_new_cambridge_score,
                                           source_deaths = source_deaths,
                                           orig_month_start_date = x,
                                           compare_against = compare_against)) %>%
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
                                       min_age = 17){

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

  if(method %in% c("Using all possible CMS data points",
                   "Using only patients with full data between prev_month_start_date and orig_month_start_date_char")){
    stop("Not implemented for that method yet")}

  # get the two tables we use
  if(method == "Using only patients with full records of data"){
    source_clean_nhs_numbers <- get_sql_table_source_clean_nhs_numbers(sql_con)
    source_new_cambridge_score <- get_sql_table_source_new_cambridge_score(sql_con)

    data_to_use <- source_clean_nhs_numbers %>% left_join(source_new_cambridge_score, by = "nhs_number")
    }
  source_deaths <- get_sql_table_source_deaths(sql_con)

  # get the data frame we are interested in and the one from the past
  orig_month_df <- data_to_use %>%
    filter(attribute_period == orig_month_start_date_char,
           age >= min_age)
  prev_month_df <- data_to_use %>%
    filter(attribute_period == prev_month_start_date_char,
           age >= min_age)


  comparison_df <- full_join(orig_month_df %>% select(nhs_number,attribute_period, segment),
                             prev_month_df %>% select(nhs_number,attribute_period, segment),
                             by = "nhs_number", suffix = c("_orig","_prev")) %>%
    rename(segment_orig = segment_orig, segment_prev = segment_prev)

  comparison_df <- collect(comparison_df)

  # num_groups usually includes a NA value, for when join not possible
  num_groups <- comparison_df %>% pull(segment_orig) %>% unique() %>% length()

  core_seg_change <- comparison_df %>%
    group_by(segment_orig, segment_prev) %>%
    # number of people moving from segment_prev to segment_orig
    summarise(num_people = n(), .groups="drop") %>%
    mutate(state_name_prev = paste0("CS",segment_prev),
           state_name_orig = paste0("CS",segment_orig)) %>%
    # sort out the NAs
    mutate(state_name_prev = ifelse(state_name_prev=="CSNA",
                                       "assumed_birth_or_migrate",
                                       state_name_prev),
           state_name_orig = ifelse(state_name_orig=="CSNA",
                                       "assumed_death_or_migrate",
                                       state_name_orig)) %>%
    # we need to keep track of from/to for months if the data set is to be joined onto others
    mutate(prev_month_start_date=prev_month_start_date,
           orig_month_start_date=orig_month_start_date) %>%
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

  # add in births and deaths
  born_core_seg_change <- create_births_by_cs(data_to_use,
                                              orig_month_start_date,
                                              prev_month_start_date,
                                              min_age)
  died_core_seg_change <- create_deaths_by_cs(data_to_use,
                                              source_deaths,
                                              orig_month_start_date,
                                              prev_month_start_date,
                                              min_age)

  final_out <- core_seg_change %>%
    bind_rows(born_core_seg_change) %>%
    bind_rows(died_core_seg_change) %>%
    mutate(method = method) %>%
    arrange(segment_prev, segment_orig)

  return(final_out)
}

# subfunction of create_monthly_transitions
create_births_by_cs <- function(data_to_use,
                                orig_month_start_date,
                                prev_month_start_date,
                                min_age){

  # previous one is anyone UNDER the age ie not born
  prev_month_df <- data_to_use %>%
    filter(attribute_period == as.character(prev_month_start_date),
           age < min_age)
  # original one is anyone OVER the age ie born
  orig_month_df <- data_to_use %>%
    filter(attribute_period == as.character(orig_month_start_date),
           age >= min_age)

  born <- inner_join(orig_month_df %>% select(nhs_number,attribute_period, segment),
                     prev_month_df %>% select(nhs_number,attribute_period),
                     by = "nhs_number", suffix = c("_orig","_prev"))

  born_core_seg_change <- born %>%
    count(attribute_period_prev, attribute_period_orig, segment) %>%
    collect() %>%
    mutate(state_name_prev = "Born",
           segment_prev = as.numeric(NA),
           state_name_orig = paste0("CS",segment)) %>%
    mutate(prev_month_start_date=as_date(attribute_period_prev),
           orig_month_start_date=as_date(attribute_period_orig),
           segment_orig = segment,
           num_people = n) %>%
    mutate(transition_name = paste0("Transition",segment_prev,"to",segment_orig)) %>%
    select(prev_month_start_date,
           orig_month_start_date,
           segment_prev,
           state_name_prev,
           segment_orig,
           state_name_orig,
           transition_name,
           num_people)

  return(born_core_seg_change)
}

# subfunction of create_monthly_transitions
create_deaths_by_cs <- function(data_to_use,
                                source_deaths,
                                orig_month_start_date,
                                prev_month_start_date,
                                min_age){
  # previous one is anyone who was born and alive at the time
  prev_month_df <- data_to_use %>%
    filter(attribute_period == as.character(prev_month_start_date),
           age >= min_age)
  # find out who has died in the period
  # important for filtering later
  last_day_max_month <- format(
    as_date(orig_month_start_date) + months(1) - days(1),"%Y-%m-%d")
  # who has died in the period
  died_in_period <- source_deaths %>%
    filter(REG_DATE_OF_DEATH >= prev_month_start_date,
           REG_DATE_OF_DEATH <= last_day_max_month) %>%
    mutate(nhs_number = as.character(Derived_Pseudo_NHS)) %>%
    select(nhs_number, death_date = REG_DATE_OF_DEATH)
  # who was alive who is now dead
  died_core_seg_change <-
    # who was alive at beginning of period
    prev_month_df %>%
    select(nhs_number, segment) %>%
    # who then died
    inner_join(died_in_period, by="nhs_number") %>%
    # count what segment they were in
    count(segment) %>%
    # tidy up
    collect() %>%
    mutate(prev_month_start_date=prev_month_start_date,
           orig_month_start_date=orig_month_start_date,
           segment_prev = segment,
           segment_orig = as.numeric(NA),
           state_name_prev = paste0("CS",segment_prev),
           state_name_orig = "Died",
           num_people = n) %>%
    mutate(transition_name = paste0("Transition",segment_prev,"to",segment_orig)) %>%
    select(prev_month_start_date,
           orig_month_start_date,
           segment_prev,
           state_name_prev,
           segment_orig,
           state_name_orig,
           transition_name,
           num_people)


  return(died_core_seg_change)
}

#' takes output from get_transition_numbers and creates the inner transitions matrix
#' out_type can either be 'matrix' or 'tbl"
#' @export
get_inner_trans_rate_from_transitions_tbl <- function(transitions,
                                                      out_type = "matrix"){
  if(!(out_type %in% c("matrix","tbl"))){stop("out_type must be matrix or tbl")}

  inner_transitions_tbl <- transitions %>%
    # remove NAs as we're plotting the INNER transitions
    filter(!is.na(segment_orig),!is.na(segment_prev)) %>%
    # calculate the proportion of that transition rate that moved - this is the transition rate
    group_by(prev_month_start_date, state_name_prev) %>%
    mutate(prop = num_people / sum(num_people)) %>%
    ungroup()

  if(out_type == "tbl"){return(inner_transitions_tbl)}
  if(out_type == "matrix"){
    inner_trans_matrix <- inner_transitions_tbl %>%
      select(state_name_prev, state_name_orig, prop) %>%
      arrange(state_name_prev, state_name_orig) %>%
      tidyr::pivot_wider(names_from = state_name_orig, values_from = prop) %>%
      select(-state_name_prev) %>%
      as.matrix()
    return(inner_trans_matrix)}
}

