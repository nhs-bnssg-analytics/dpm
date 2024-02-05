
#' Getting ONS Population Forecasts
#' only way we have to get predictions is from ONS figures. They publish different estimates, and
#' this function helps you select the
#' @param method which method of getting the data do you want to use. Options are:
#' - 'Use Original Aug 2023 DPM Calc' the original values as calculated by Zehra in Aug 2023
#' - 'use ONS closest year' the ONS data set at [url](http://tinyurl.com/ons-bmd-2020)
#' with the year as the closest year to @date_of_year_zero
#' @param forecast_variant Which ONS forecast to use. To get the list of valid options, run dpm::get_ons_forecast_variant_options()
#' @param date_of_year_zero the date to take as the start point of the data. If you are using month_of_interest elsewhere - put date_of_year_zero = as_date(paste0(month_of_interest,"-01"))
#' @import dplyr
#' @export
get_numbers_births_migrations_deaths <- function(method = "Use Original Aug 2023 DPM Calc",
                                                 forecast_variant = "normal",
                                                 date_of_year_zero = Sys.Date()){

  if(forecast_variant=="normal"){
    print("normal forecast_variant is 2018based ONS projections")
    forecast_variant <- "2018based"}

  if(is.character(date_of_year_zero)){
    warning("date_of_year_zero should be date input")
    if(nchar(date_of_year_zero == 10)){
      date_of_year_zero = as.Date(date_of_year_zero)
      } else {stop("can not convert date_of_year_zero to date")}
  }

  valid_methods <- c("Use Original Aug 2023 DPM Calc","use ONS closest year")
  if(!(method %in% valid_methods)){
    stop("method field must be in:\n",paste0("'",valid_methods,"'",
                                             collapse="\n"))}

  input_folder_loc <- fs::path_package("extdata", package = "dpm")

  # first method reads the input file direct from Zehra file. Doesn't
  # use the date input
  if(method == "Use Original Aug 2023 DPM Calc"){
    if(forecast_variant!="2018based"){stop("can only do forecast_variant 'normal' for this method")}
    s2_file <- paste0(input_folder_loc,"/DPM-v1-Model-Inputs-S2.xlsx")
    births_net_migration_deaths_figures <- s2_file |>
      readxl::read_excel(sheet="Birth_Migration_Death")  |>
      tidyr::pivot_longer(cols=c("Births","Net_Migration","Deaths")) |>
      mutate(name = case_when(
        name=="Births"~"births",
        name=="Net_Migration"~"net_migration",
        name=="Deaths"~"deaths"
      )) |>
      rename(year=Year, event=name)
  }

  # Second method reads from t'internet
  if(method == "use ONS closest year"){
    ons_projs <- read_ons_projections_online(forecast_variant = forecast_variant,
                                             table = "births_deaths_migration")

    ons_projs <- ons_projs |>
      dplyr::filter(area%in% c("Bristol, City of",
                        "North Somerset",
                        "South Gloucestershire"))
    # check we have all areas
    if(ons_projs |> count(area) |> nrow() != 3){
      stop("Something wrong with the AREA field in the data")}

    ons_projs <- ons_projs |>
      group_by(component, year) |>
      summarise(value = sum(value), .groups="drop")


    births <- dpm:::get_numbers_births(forecast_variant=forecast_variant)


    # get to Births / Deaths / Net Migration
    births_net_migration_deaths_figures <- ons_projs |>
      filter(component %in% c("All Migration Net","Deaths")) |>
      mutate(event = case_when(
        component=="All Migration Net"~"net_migration",
        component=="Deaths"~"deaths"
      )) |>
      select(year, event, value) |>
      bind_rows(births) |>
      arrange(year, event)

    # what proportion of deaths shall we say are in the 17+ population?
    deaths_props <- calc_death_prop_age(age_lower_bound = 17,
                                        sql_con = dpm:::get_sql_con(),
                                        start_date = date_of_year_zero - lubridate::years(1),
                                        end_date = date_of_year_zero)
    # apply the scalar
    births_net_migration_deaths_figures <- births_net_migration_deaths_figures |>
      mutate(value = ifelse(event=="deaths", value*deaths_props, value))


    # sort the start date and what year 1 is - define year 0 as the closest June 30th
    # value you can find, as that's the point the forecasts are for
    year_0 <- date_of_year_zero |> lubridate::year()
    births_net_migration_deaths_figures <- births_net_migration_deaths_figures |>
      mutate(year = year - year_0) |>
      filter(year > 0)
  }

  return(births_net_migration_deaths_figures)
}

#' get the numbers of births from ONS projections. Subfunction of
#' dpm::get_numbers_births_migrations_deaths
#' @param forecast_variant character goes directly into dpm:::read_ons_projections_online
get_numbers_births <- function(forecast_variant){
  warning("births defined as turning 17")
  # ONS five year age projections for BNSSG NHS area
  ons_age_proj <- read_ons_projections_online(forecast_variant = forecast_variant,
                                           table = "population_by_age")

  ons_age_proj <- ons_age_proj |>
    dplyr::filter(area%in% c("Bristol, City of",
                             "North Somerset",
                             "South Gloucestershire"))
  # clean and reshape
  ons_age_proj <- ons_age_proj |>
    group_by(year, age_group) |>
    summarise(value = sum(value), .groups="drop")
  # BIRTHS are 1/5 of the 15-19 age group, as a guess for how many
  # turned 17 that year
  births <- ons_age_proj |>
    filter(age_group=="15-19") |>
    mutate(value = value/5) |>
    select(year, value) |>
    mutate(event = "births") |>
    select(year, event, value)
  return(births)
}

#' Calculate the Percentage of Deaths which are from the population aged between two ages for a given period
#' @param age_lower_bound numeric the minimum age for death, 0 if left blank
#' @param age_upper_bound numeric the maximum age for death, inf if left blank
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
  source_deaths <- get_sql_table_source_deaths(sql_con) |>
    filter(between(REG_DATE_OF_DEATH, start_date, end_date))

  # number of deaths where person was within age range given
  deaths_within_age_range <- source_deaths |>
    filter(between(Dec_Age_At_Death, age_lower_bound, age_upper_bound)) |>
    summarise(value = n()) |>
    pull(value)

  # get the population
  total_deaths <- source_deaths |>
    summarise(value = n()) |>
    pull(value)

  return(deaths_within_age_range/total_deaths)
}

#' Show what options exist for ONS forecasts
#' @param url the URL, can be left blank
#' @param message_with_options logical whether to print the options to the console
#' @export
get_ons_forecast_variant_options <- function(url = "I don't know the url",
                                             message_with_options = TRUE){
  if(url == "I don't know the url"){
    warning("using known ONS url")
    url <- paste0(
      "https://www.ons.gov.uk/peoplepopulationandcommunity",
      "/populationandmigration/populationprojections/datasets",
      "/localauthoritiesinenglandtable2")
    print("using this url:")
    print(url)
    }

  if(httr::http_error(url)){stop("ONS projections url not found")}

  # Make an HTTP request to the webpage
  response <- GET(url)
  # Check if the request was successful
  if (http_status(response)$category == "Success") {
    # Parse the HTML content of the webpage
    webpage <- read_html(content(response, "text"))
  } else {
    cat("Failed to retrieve the webpage. HTTP status:", http_status(response)$reason, "\n")
  }

  # Extract all links with .xls extension
  xls_links <- webpage |>
    html_nodes("a[href$='.xls']") |>
    html_attr("href")
  # get the penultimate part of the url, the bit between the last two foreward slashes
  valid_forecast_variants <- xls_links |>
    stringr::str_split("/") |>
    purrr::map(tail, 2) |>
    purrr::map(head,1) |>
    unlist()

  # print the options into the console
  if(message_with_options){
  message("forecast_variant options are:\n",
        paste0("'",valid_forecast_variants,"'",
                                          collapse="\n"),"\n")
  } else {
    # only return output if message_with_options is false
    out_df <- tibble::tibble(
      xls_links = xls_links,
      valid_forecast_variants = valid_forecast_variants)

    return(out_df)
    }
}

#' Given a forecast variant from ONS, read the appropriate Excel file and clean
#' it, into a tidy format for use elsewhere
#' @param forecast_variant character the variant of the forecast you want to use
#' @param table character the table you want to use, one of:
#' - 'births_deaths_migration' the table of births, deaths and migration.
#' - 'population_by_age' the table of population by 5 year age bands. Useful for
#' births' of 17+
#' @param sleep the number of seconds to pause for, to avoid errors for requestion too much from the web
#' @import httr
#' @import rvest
#' @import purrr
read_ons_projections_online <- function(forecast_variant,
                                        table,
                                        sleep = 1){

  if(!table %in% c("births_deaths_migration","population_by_age",
                   "table2","table5","table53")){
    stop("table must be one of:\n",
         paste0("'",c("births_deaths_migration","population_by_age"),
                "'",collapse="\n"),"\ndepending on what you want!")}

  if(table %in% c("table2","population_by_age")){
    url <- paste0(
      "https://www.ons.gov.uk/peoplepopulationandcommunity",
      "/populationandmigration/populationprojections/datasets",
      "/localauthoritiesinenglandtable2")
  }
  if(table %in% c("table5","births_deaths_migration","table53")){
    url <- paste0(
      "https://www.ons.gov.uk/peoplepopulationandcommunity",
      "/populationandmigration/populationprojections/datasets",
      "/componentsofchangebirthsdeathsandmigrationforregionsand",
      "localauthoritiesinenglandtable5")
  }

  # what are the valid options for that URL
  valid_forecast_variants_tbl <- dpm::get_ons_forecast_variant_options(
    url = url,message_with_options = FALSE)

  # make sure we're not going crazy on the requests.
  # Sometimes causes errors if we move to fast.
  Sys.sleep(sleep)

  # get the full URL links
  xls_links <- valid_forecast_variants_tbl |> pull(xls_links)
  valid_forecast_variants <- valid_forecast_variants_tbl |> pull(valid_forecast_variants)
  # check validity, if so select the URL
  if(forecast_variant %in% valid_forecast_variants){
    xls_url <- paste0("https://www.ons.gov.uk",
                      xls_links[valid_forecast_variants==forecast_variant])
  } else {
    stop("forecast_variant must be in:\n",paste0("'",valid_forecast_variants,"'",
                                                collapse="\n"))}


  # download the file to location tf
  # the use of invisible is to stop any output into the console
  invisible(httr::GET(xls_url, httr::write_disk(temp_file <- tempfile(fileext = ".xls"))))
  # read into R as a tibble
  ons_projs <- temp_file |>
    readxl::read_excel(sheet="Persons", skip=6)  |>
    janitor::clean_names() |>
    tidyr::pivot_longer(cols = where(is.numeric),
                        names_to = "year",values_to="value") |>
    mutate(year = as.numeric(stringr::str_remove(year,"x")))

  # annoyance in data downloads where sometimes in thousands
  # check by taking the largest value for England - must be over 1 million
  england_val <- ons_projs |>
    filter(area %in% c("England","ENGLAND")) |>
    filter(value==max(value,na.rm=T)) |>
    pull(value)
  if(england_val < 1e6){
    warning("think values are in thousands - scaling up")
    ons_projs <- ons_projs |>
      mutate(value = value*1000)}

  # remove the downloaded file
  invisible(file.remove(temp_file))

  return(ons_projs)
}

