
#' Given a start_month, compare the population then VS compare_against months prior,
#' to output the proportion of births, migrations, and deaths that come from
#' each Core Segment
#' @param start_month the start month as a character of form YYYY-MM
#' @param source_or_preload either 'source' or 'preloaded' depending on whether to
#' calculate source_or_preload the source SQL tables or use a pre-created file
#' @param method either a number (1 default) or string specifying the method. Valid
#' methods are
#' 1  or "Matching at patient-level migrations in/out"
#' 2  or "Taking average movements from data"
#' @param sql_con connection to SQL database
#' @param min_age the minimum age to be included in the model
#' @param compare_against how many months back in time to compare against, default is 12
#' @param combine_immigration_emigration boolean whether to combine into migration or not
#' @param output_proportions_or_numbers should the output tibble be raw numbers, or
#' proportions
#' @import dplyr
#' @import lubridate
#' @export
get_births_migrations_deaths_proportions <- function(
    start_month,
    source_or_preload = "source",
    method = 1,
    sql_con=NA,
    min_age=17,
    compare_against=12,
    combine_immigration_emigration = TRUE,
    output_proportions_or_numbers = "proportions",
    age_groups = F){

  start_month_date_char = paste0(start_month,"-01")
  compare_against_month_date_char <- format(as_date(start_month_date_char)-months(compare_against),format="%Y-%m-%d")
  # Warnings and Errors around inputs
  if(nchar(start_month)!=7 | !is.character(start_month)){
    stop("start_month needs to be character of form YYYY-MM")}
  if(!(source_or_preload %in% c("source","preload"))){
    stop("source_or_preload needs to be either 'source' or 'preload'")}
  if(!(output_proportions_or_numbers %in% c("proportions","numbers"))){
    stop("output_proportions_or_numbers needs to be either 'proportions' or 'numbers'")}
  if(compare_against!=12){
    warning(paste0(
      "The DPM is built on yearly time increments, so 12 is the most ",
      "sensible value for compare_against as it is in months"))
  }

  # Methods
  method_options <- c(
    "1"  = "Matching at patient-level migrations in/out",
    "2"  = "Taking average movements from data"
  )
  if(!(method %in% append(method_options,names(method_options)))){
    stop(paste0(
      "\n'method' input must be one of these options:\n  -",
      paste0(method_options,collapse="\n  -"),
      "\nor a number to indicate which one of the above to pick"))}
  if(is.numeric(method)){method = unname(method_options[method])}
  print(paste0("Getting initial population using method: ",method))


  if(source_or_preload=="preloaded"){
    warning("not implemented - returning blank")
    return()
  }

  if(source_or_preload=="source"){
    if(class(sql_con)[1]!="Microsoft SQL Server"){stop("sql_con variable needs to be provided as class Microsoft SQL Server")}

    if(method == "Matching at patient-level migrations in/out"){
      births_net_migration_deaths_figures <- get_bmd_vals_matching_method(
        sql_con = sql_con,
        start_month_date_char = start_month_date_char,
        compare_against_month_date_char = compare_against_month_date_char,
        combine_immigration_emigration = combine_immigration_emigration,
        min_age = min_age,
        age_groups = age_groups)
    } else {
      stop("Haven't implemented other than first method, sorry!")
    }

    if(age_groups){
      births_net_migration_deaths_figures <-
        add_age_cs_state_col(births_net_migration_deaths_figures)}

    if(output_proportions_or_numbers=="numbers"){
      return(births_net_migration_deaths_figures)}
    if(output_proportions_or_numbers=="proportions"){
      birth_migration_deaths_proportions <-
        births_net_migration_deaths_figures |>
        group_by(event) |>
        mutate(prop = num_people/sum(num_people)) |>
        select(-num_people) |>
        arrange(state_name, event) |>
        ungroup()
      return(birth_migration_deaths_proportions)
    }
  }
}

#' subfunction of get_births_migrations_deaths_proportions when method is
#' 1 or "Matching at patient-level migrations in/out
#' @param sql_con connection to SQL database
#' @param start_month_date_char character string format YYYY-MM-DD
#' @param compare_against_month_date_char character string format YYYY-MM-DD
#' @param combine_immigration_emigration boolean whether to combine into migration or not
#' @param min_age default 17
get_bmd_vals_matching_method <- function(sql_con,
                                         start_month_date_char,
                                         compare_against_month_date_char,
                                         combine_immigration_emigration = TRUE,
                                         min_age = 17,
                                         age_groups=F){
  # table connections
  source_new_cambridge_score <- get_sql_table_source_new_cambridge_score(sql_con)
  source_deaths <- get_sql_table_source_deaths(sql_con)

  # create the two data sets
  orig_cms_tbl <- source_new_cambridge_score |>
    filter(attribute_period==start_month_date_char,
           age >= min_age)
  prev_cms_tbl <- source_new_cambridge_score |>
    filter(attribute_period==compare_against_month_date_char,
           age >= min_age)

  # Sense check with warning if we're too close to current time
  most_recent_death_reg <- source_deaths |>
    filter(REG_DATE==max(REG_DATE,na.rm=T)) |>
    head(1) |>
    pull(REG_DATE)
  days_between <- as.numeric(as_date(most_recent_death_reg) -
                               as_date(start_month_date_char), units="days")
  if(days_between < 90){
    warning(paste0("Only ",days_between, " days between start_month and the ",
                   "most recent death log, could cause under counting deaths"))}

  # who was in previous but isn't in the original
  death_or_emigrate_tbl <- anti_join(
    prev_cms_tbl, orig_cms_tbl |> select(nhs_number), by = "nhs_number")
  # find out if the missing patient is registered died
  death_or_emigrate_tbl <- death_or_emigrate_tbl |>
    left_join(source_deaths |> select(Derived_Pseudo_NHS,
                                      Dec_Age_At_Death,
                                      REG_DATE_OF_DEATH),
              by=c("nhs_number"="Derived_Pseudo_NHS")) |>
    # have they been registered died in the period
    mutate(registered_dead =
             REG_DATE_OF_DEATH<=start_month_date_char &
             REG_DATE_OF_DEATH>=compare_against_month_date_char)

  # who has been born or migrated in
  born_or_immigrate_tbl <- anti_join(
    orig_cms_tbl, prev_cms_tbl |> select(nhs_number), by = "nhs_number")

  # combine it together - by NHS number all those entering/leaving the
  # environment
  born_migrant_death_by_nhs_number_tbl <- bind_rows(
    # first - born or immigrant by NHS number
    born_or_immigrate_tbl |>
      select(nhs_number, age, segment) |>
      collect() |>
      # work out if they're born or an immigrant
      mutate(status = ifelse(age==min_age,"born","immigrant")),
    # second - death or emmigrant by NHS number
    death_or_emigrate_tbl |>
      select(nhs_number, registered_dead, segment,age) |>
      collect() |>
      # work out if dead or emigrant
      mutate(status = ifelse(registered_dead,"died","emigrant"))
  ) |>
    # tidy up
    mutate(state_name = paste0("CS",segment)) |>
    select(nhs_number, state_name, status, age)

  # create a totals tibble
  if(age_groups){
    born_emigrant_immigrant_death_tbl <-
      born_migrant_death_by_nhs_number_tbl |>
      add_age_group_column() |>
      convert_to_decade("age_group") |>
      # count the people
      summarise(num_people = n(),
                .by=c(state_name, status, age_group)) |>
      # complete the data to include zeros where missing - eg often
      # no one born into CS5 as unlikely for 17 year olds to be
      # that sick
      tidyr::complete(state_name,
                      status,
                      age_group,
                      fill= list(num_people = 0))
  } else {
    born_emigrant_immigrant_death_tbl <-
      born_migrant_death_by_nhs_number_tbl |>
      # count the people
      summarise(num_people = n(),
                .by=c(state_name, status)) |>
      # complete the data to include zeros where missing - eg often
      # no one born into CS5 as unlikely for 17 year olds to be
      # that sick
      tidyr::complete(state_name,
                      status,
                      fill= list(num_people = 0))
  }

  # difference here is migrants are combined
  if(combine_immigration_emigration){
    born_emigrant_immigrant_death_tbl <- born_emigrant_immigrant_death_tbl %>%
      mutate(status = case_when(
        status %in% c("emigrant","immigrant") ~ "net_migration",
        TRUE ~ status))
  }

  # final grouping and counting
  births_net_migration_deaths_figures <- born_emigrant_immigrant_death_tbl |>
    mutate(event = case_when(
      status == "born" ~ "births",
      status == "emigrant" ~ "emigrations",
      status == "immigrant" ~ "immigrations",
      status == "died" ~ "deaths",
      TRUE ~ status
    ))

  if(age_groups){
    births_net_migration_deaths_figures <- births_net_migration_deaths_figures |>
      summarise(num_people=sum(num_people),.by=c(event,state_name, age_group))
  } else {
    births_net_migration_deaths_figures <- births_net_migration_deaths_figures |>
      summarise(num_people=sum(num_people),.by=c(event,state_name))
  }

  return(births_net_migration_deaths_figures)
}
