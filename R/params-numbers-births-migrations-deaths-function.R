
#' only way we have to get predictions is from ONS figures
#' @param method which method of getting the data do you want to use. Options are:
#' - 'Use Original Aug 2023 DPM Calc' the original values as calculated by Zehra in Aug 2023
#' - 'use ONS closest year' the ONS data set at [url](http://tinyurl.com/ons-bmd-2020)
#' with the year as the closest year to @date_of_year_zero
#' @param date_of_year_zero the date to take as the start point of the data
#' @import dplyr
#' @export
get_numbers_births_migrations_deaths <- function(method = "Use Original Aug 2023 DPM Calc",
                                                 date_of_year_zero = Sys.Date()){

  valid_methods <- c("Use Original Aug 2023 DPM Calc","use ONS closest year")
  if(!(method %in% valid_methods)){
    stop("method field must be in:\n",paste0("'",valid_methods,"'",
                                             collapse="\n"))}

  input_folder_loc <- fs::path_package("extdata", package = "dpm")
  # first method reads the input file direct from Zehra file. Doesn't
  # use the date input
  if(method == "Use Original Aug 2023 DPM Calc"){
    s2_file <- paste0(input_folder_loc,"/DPM-v1-Model-Inputs-S2.xlsx")
    births_net_migration_deaths_figures <- s2_file %>%
      readxl::read_excel(sheet="Birth_Migration_Death")  %>%
      tidyr::pivot_longer(cols=c("Births","Net_Migration","Deaths")) %>%
      mutate(name = case_when(
        name=="Births"~"births",
        name=="Net_Migration"~"net_migration",
        name=="Deaths"~"deaths"
      )) %>%
      rename(year=Year, event=name)
  }

  if(method == "use ONS closest year"){
    ons_file <- paste0(input_folder_loc,"/table53.xls")
    ons_projs <- ons_file %>%
      readxl::read_excel(sheet="Persons", skip=6)  %>%
      janitor::clean_names() %>%
      dplyr::filter(area%in% c("Bristol, City of",
                        "North Somerset",
                        "South Gloucestershire"))
    # check we have all areas
    if(ons_projs %>% count(area) %>% nrow() != 3){
      stop("Something wrong with the AREA field in the data")}

    ons_projs <- ons_projs %>%
      tidyr::pivot_longer(cols = where(is.numeric),
                          names_to = "year") %>%
      mutate(year = as.numeric(stringr::str_remove(year,"x"))) %>%
      group_by(component, year) %>%
      summarise(value = sum(value), .groups="drop")


    births <- dpm:::get_numbers_births(input_folder_loc)


    # get to Births / Deaths / Net Migration
    births_net_migration_deaths_figures <- ons_projs %>%
      filter(component %in% c("All Migration Net","Deaths")) %>%
      mutate(event = case_when(
        component=="All Migration Net"~"net_migration",
        component=="Deaths"~"deaths"
      )) %>%
      select(year, event, value) %>%
      bind_rows(births) %>%
      arrange(year, event)

    # what proportion of deaths shall we say are in the 17+ population?
    deaths_props <- calc_death_prop_age(age_lower_bound = 17,
                                        sql_con = dpm:::get_sql_con(),
                                        start_date = date_of_year_zero - lubridate::years(1),
                                        end_date = date_of_year_zero)
    # apply the scalar
    births_net_migration_deaths_figures <- births_net_migration_deaths_figures %>%
      mutate(value = ifelse(event=="deaths", value*deaths_props, value))


    # sort the start date and what year 1 is - define year 0 as the closest June 30th
    # value you can find, as that's the point the forecasts are for
    year_0 <- date_of_year_zero %>% lubridate::year()
    births_net_migration_deaths_figures <- births_net_migration_deaths_figures %>%
      mutate(year = year - year_0) %>%
      filter(year > 0)
  }

  return(births_net_migration_deaths_figures)
}

#' get the numbers of births from ONS projections
get_numbers_births <- function(input_folder_loc){
  warning("births defined as turning 17")
  # ONS five year age projections for BNSSG NHS area
  ons_age_proj_file <- paste0(input_folder_loc,"/table3.xls")
  one_age_proj <- ons_age_proj_file %>%
    readxl::read_excel(sheet="Persons", skip=6)  %>%
    janitor::clean_names() %>%
    dplyr::filter(area%in% c("NHS Bristol, North Somerset and South G"))
  # clean and reshape
  one_age_proj <- one_age_proj %>%
    tidyr::pivot_longer(cols = where(is.numeric),
                        names_to = "year") %>%
    mutate(year = as.numeric(stringr::str_remove(year,"x"))) %>%
    group_by(area, year, age_group) %>%
    summarise(value = sum(value), .groups="drop")
  # BIRTHS are 1/5 of the 15-19 age group, as a guess for how many
  # turned 17 that year
  births <- one_age_proj %>%
    filter(age_group=="15-19") %>%
    mutate(value = value/5) %>%
    select(year, value) %>%
    mutate(event = "births") %>%
    select(year, event, value)
  return(births)
}

#' Calculate the Percentage of Deaths which are from the population aged between two ages for a given period
#' @param age_lower_bound numeric the minimum age for death, 0 if left blank
#' @param age_lower_bound numeric the maximum age for death, inf if left blank
#' @param sql_con connection to SQL database
#' @param start_date character format YYYY-MM-DD
#' @param end_date character format YYYY-MM-DD
#' @import dplyr
calc_death_prop_age <-function(age_lower_bound = 0,
                               age_upper_bound = Inf,
                               sql_con=NA,
                               start_date = "2020-01-01",
                               end_date = "2020-12-31"){

  if(class(sql_con)[1]!="Microsoft SQL Server"){stop("sql_con variable needs to be provided as class Microsoft SQL Server")}

  if(age_upper_bound == Inf){age_upper_bound <- 999}

  # connect to the SQL data
  source_deaths <- get_sql_table_source_deaths(sql_con) %>%
    filter(between(REG_DATE_OF_DEATH, start_date, end_date))

  # number of deaths where person was within age range given
  deaths_within_age_range <- source_deaths %>%
    filter(between(Dec_Age_At_Death, age_lower_bound, age_upper_bound)) %>%
    summarise(value = n()) %>%
    pull(value)

  # get the population
  total_deaths <- source_deaths %>%
    summarise(value = n()) %>%
    pull(value)

  return(deaths_within_age_range/total_deaths)
}

