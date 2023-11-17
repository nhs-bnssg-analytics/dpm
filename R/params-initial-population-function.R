
#' Given a month, create the initial_population tibble input, by Core Segment
#' @param start_month the start month as a character of form YYYY-MM
#' @param source_or_preload either 'source' or 'preloaded' depending on whether to
#' calculate source_or_preload the source SQL tables or use a pre-created file
#' @param method either a number (1 default) or string specifying the method. Valid
#' methods are
#' 1  or "CS props: Cleaned CMS CS. Total pop: GP Estimates scaled down 90% to match ONS"
#' 2  or "CS props: Cleaned CMS Values. Total pop: Cleaned CMS Values"
#' @param sql_con connection to SQL database
#' @param min_age the minimum age to be included in the model
#' @import dplyr
#' @export
get_initial_population <- function(start_month,
                                   source_or_preload = "source",
                                   method=1,
                                   sql_con=NA,
                                   min_age=17){

  start_month_date_char <- paste0(start_month, "-01")
  # Warnings and Errors around inputs
  if(!(source_or_preload %in% c("source","preload"))){
    stop("source_or_preload needs to be either 'source' or 'preload'")}
  if(nchar(start_month)!=7 | !is.character(start_month)){
    stop("start_month needs to be character of form YYYY-MM")}
  if(!(source_or_preload %in% c("source","preloaded"))){
    stop("source_or_preload needs to be either 'source' or 'preloaded'")
  }

  # Methods
  method_options <- c(
    "1" ="CS props: Cleaned CMS CS. Total pop: GP Estimates scaled down 90% to match ONS",
    "2" ="CS props: Cleaned CMS Values. Total pop: Cleaned CMS Values"
  )
  if(!(method %in% append(method_options,names(method_options)))){
    stop(paste0(
      "\n'method' input must be one of these options:\n  -",
      paste0(method_options,collapse="\n  -"),
      "\nor a number to indicate which one of the above to pick"))}
  if(is.numeric(method)){method = unname(method_options[method])}
  # print statement
  print(paste0("Getting initial population using method: ",
               stringr::str_replace(method,"\\. ","\n")))


  if(source_or_preload=="preloaded"){
    warning("not implemented - returning blank")
    return()
  }

  if(source_or_preload=="source"){
    if(class(sql_con)[1]!="Microsoft SQL Server"){stop("sql_con variable needs to be provided as class Microsoft SQL Server")}

    # two table connections
    source_clean_nhs_numbers <- get_sql_table_source_clean_nhs_numbers(sql_con)
    source_new_cambridge_score <- get_sql_table_source_new_cambridge_score(sql_con)

    # proportions of initial population per Core Segment
    initial_population_props <- source_clean_nhs_numbers %>%
      left_join(source_new_cambridge_score %>%
                  filter(attribute_period==start_month_date_char),
                by="nhs_number") %>%
      filter(!is.na(segment), age>=min_age) %>%
      count(segment) %>%
      collect() %>%
      mutate(state_name = paste0("CS",segment)) %>%
      mutate(prop = n/sum(n)) %>%
      select(state_name, prop)

    if(method == "CS props: Cleaned CMS CS. Total pop: GP Estimates scaled down 90% to match ONS"){
      total_pop <- get_pop_from_gp_data(start_month_date_char, sql_con, min_age)
      }
    if(method == "CS props: Cleaned CMS Values. Total pop: Cleaned CMS Values"){
      total_pop <- source_clean_nhs_numbers %>%
        left_join(source_new_cambridge_score %>%
                    filter(attribute_period==start_month_date_char),
                  by="nhs_number") %>%
        filter(!is.na(segment), age>=min_age) %>%
        count() %>% collect() %>% pull(n)}
  }

  # scale the proportions to get initial population values
  initial_population <- initial_population_props %>%
    mutate(initial_pop = round(total_pop * prop)) %>%
    select(state_name, initial_pop)

  return(initial_population)
}


#' subfunction of get_initial_population when method is
#' 1 or "CS props: Cleaned CMS CS. Total pop: GP Estimates scaled down 90% to match ONS"
#' @import janitor
#' @noRd
get_pop_from_gp_data <- function(start_month_date_char, sql_con, min_age=17){
  # connect to the data
  source_population <- get_sql_table_source_population(sql_con)


  # get age at ICB level for the GP Population data set (updated monthly)
  age_by_sub_icb <- source_population %>%
    select(EXTRACT_DATE, ORG_CODE, SEX, SUB_ICB_LOCATION_CODE, AGE, NUMBER_OF_PATIENTS) %>%
    # filter to just BNSSG ICB
    filter(glue::glue_sql("SUB_ICB_LOCATION_CODE LIKE '15C'") |
             glue::glue_sql("CCG_CODE LIKE '15C'")) %>%
    # just the month we're interested in
    filter(EXTRACT_DATE == paste0(start_month_date_char," 00:00:00")) %>%
    # bring into R environment
    collect() %>%
    janitor::clean_names()

  if(nrow(age_by_sub_icb)==0){stop("couldn't find data for month ",start_month_date_char,
                                   " in GP population table")}

  unscaled_gp_pop_estimate <- age_by_sub_icb %>%
    # take out the ALL otherwise we'll get twice as many patients
    filter(age!="ALL") %>%
    # make it into a numeric field - for this purpose "95+" is 1 billion just in case
    # accidently summing using the numeric field - should give garbage answer
    mutate(age = ifelse(age=="95+",1e9,age)) %>%
    mutate(age = as.integer(age)) %>%
    filter(age >= min_age) %>%
    summarise(number_of_patients = sum(number_of_patients),.groups="drop") %>%
    pull(number_of_patients)

  # known feature that this list over-estimates compared to
  scaled_gp_pop_estimate <- unscaled_gp_pop_estimate * 0.9

  return(scaled_gp_pop_estimate)
}

