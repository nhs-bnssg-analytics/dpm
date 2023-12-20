
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


    # migration figs
    net_migration <- ons_projs %>%
      filter(stringr::str_detect(component,"Migration")) %>%
      tidyr::pivot_wider(names_from="component",
                         values_from = "value") %>%
      mutate(Net_Migration =
               `Cross-border Migration In` -
               `Cross-border Migration Out` +
               `Internal Migration In` -
               `Internal Migration Out`) %>%
      select(year, Net_Migration) %>%
      mutate(component = "Net_Migration") %>%
      select(year, component, value=Net_Migration)
    # get to Births / Deaths / Net Migration
    births_net_migration_deaths_figures <- ons_projs %>%
      filter(component %in% c("Births","Deaths")) %>%
      bind_rows(net_migration) %>%
      mutate(name = case_when(
        component=="Births"~"births",
        component=="Net_Migration"~"net_migration",
        component=="Deaths"~"deaths"
      )) %>%
      rename(event=name) %>%
      select(year, event, value) %>%
      arrange(year, event)

    # sort the start date and what year 1 is - define year 0 as the closest June 30th
    # value you can find, as that's the point the forecasts are for
    year_0 <- date_of_year_zero %>% lubridate::year()
    births_net_migration_deaths_figures <- births_net_migration_deaths_figures %>%
      mutate(year = year - year_0) %>%
      filter(year > 0)
  }

  return(births_net_migration_deaths_figures)
}
