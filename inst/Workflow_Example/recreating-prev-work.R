################################################################################
# Recreating the work under previous models S1 and S2
################################################################################

library(dpm)
library(magrittr)

input_folder_loc <- fs::path_package("extdata", package = "dpm")
s2_file <- paste0(input_folder_loc,"/DPM v1 Model Inputs S2.xlsx")

##########
initial_population <- readxl::read_excel(s2_file,sheet="Initial State") %>%
  filter(State!="Death") %>%
  rename(state_name = State, initial_pop = Initial_pop)

inner_trans_matrix <- readxl::read_excel(s2_file,sheet="Inner Transition Matrix") %>%
  select(-State) %>%
  as.matrix()

total_time <- readxl::read_excel(s2_file,sheet="Horizon") %>%
  pull(`Time Horizon (years)`)

births_net_migration_deaths_figures <- s2_file %>%
  readxl::read_excel(sheet="Birth_Migration_Death")  %>%
  tidyr::pivot_longer(cols=c("Births","Net_Migration","Deaths")) %>%
  mutate(name = case_when(
    name=="Births"~"births",
    name=="Net_Migration"~"net_migration",
    name=="Deaths"~"deaths"
  )) %>%
  rename(year=Year, event=name)

birth_migration_deaths_proportions <- s2_file %>%
  readxl::read_excel(sheet="Proportion")  %>%
  tidyr::pivot_longer(cols=contains("Proportion")) %>%
  mutate(name = case_when(
    name=="Proportion_birth"~"births",
    name=="Proportion_migration"~"net_migration",
    name=="Proportion_death"~"deaths"
  )) %>%
  rename(state_name=State,event=name,prop=value) %>%
  filter(state_name!="Death")



population_at_each_year <- dpm::run_dpm(
  initial_population = initial_population,
  inner_trans_matrix_list = inner_trans_matrix,
  total_time = total_time,
  births_net_migration_deaths_figures = births_net_migration_deaths_figures,
  birth_migration_deaths_proportions = birth_migration_deaths_proportions)

create_sankey(population_at_each_year,
              inner_trans_matrix)
