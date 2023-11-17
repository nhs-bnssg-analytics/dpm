
#' only way we have to get predictions is from ONS figures
#' @export
get_numbers_births_migrations_deaths <- function(){
  input_folder_loc <- fs::path_package("extdata", package = "dpm")
  s2_file <- paste0(input_folder_loc,"/DPM v1 Model Inputs S2.xlsx")
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
