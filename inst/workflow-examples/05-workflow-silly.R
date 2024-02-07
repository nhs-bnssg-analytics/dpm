################################################################################
# Workflow example using simple dummy data - with different parameters having
# key fluctuations within years causing obvious visual differences
################################################################################

# The PopwerPoint slide in inst/extdata/DPM-indexing.pptx is helpful with
# visualising this.

library(dpm)
library(magrittr)
library(ggplot2)
# parameter define --------------------------------------------------------


initial_population <- tibble::tribble(
  ~state_name, ~initial_pop,
  "CS1",2000,
  "CS2",1000,
  "CS3",500,
  "CS4",200,
  "CS5",100
)
# define the initial matrix
inner_trans_matrix_default <- matrix(
  data = c(0.8,0.2,0,0,0,
           0.2,0.8,0,0,0,
           0.0,0,1,0,0,
           0.0,0,0,1,0,
           0.0,0,0,0,1),
  nrow = 5,
  ncol = 5)

total_time <- 20

births_net_migration_deaths_figures <- tibble::tibble(
  year = rep(1:total_time,each=3),
  event = rep(c("births","net_migration","deaths"),times=total_time),
  value = rep(c(100,10,50),times=total_time)
) %>%
  # silly momeny in year 10 where inputs are 10 times bigger
  dplyr::mutate(value = ifelse(year==10,value*10,value))

birth_migration_deaths_proportions <- tibble::tribble(
  ~state_name, ~event, ~prop,
  "CS1","births",0.9,
  "CS2","births",0.07,
  "CS3","births",0.03,
  "CS4","births",0,
  "CS5","births",0,
  "CS1","net_migration",0.5,
  "CS2","net_migration",0.3,
  "CS3","net_migration",0.2,
  "CS4","net_migration",0.09,
  "CS5","net_migration",0.01,
  "CS1","deaths",0.05,
  "CS2","deaths",0.05,
  "CS3","deaths",0.15,
  "CS4","deaths",0.25,
  "CS5","deaths",0.5)


# run scenarios -----------------------------------------------------------


# BASE SCENARIO
population_at_each_year_a <-
dpm::run_dpm(
  initial_population = initial_population,
  inner_trans_matrix_list = inner_trans_matrix_default,
  total_time = total_time,
  births_net_migration_deaths_figures = births_net_migration_deaths_figures,
  birth_migration_deaths_proportions = birth_migration_deaths_proportions)
# see the change in population - plot 1
dpm::plot_dpm_with_growth(population_at_each_year_a) +
  labs(title = "In year 10 there is 10 times usual injection of births/migrations/deaths",
       subtitle = "baseline model")
# see the sankey - plot 2
dpm::create_sankey(
  population_at_each_year_a,
  inner_trans_matrix_list=inner_trans_matrix_default) +
  labs(title = "BASE SCENARIO - no change to inner trans \n \n")

# ONE CHANGE TO TRANS MATRIX
inner_trans_matrix_1change <- inner_trans_matrix_default |>
  # make a change
  changing_inner_trans_matrix(from_cs = 1,
                              to_cs = 2,
                              scalar_change = 0,
                              over_n_iterations = 3)
population_at_each_year_b <-
  dpm::run_dpm(
    initial_population = initial_population,
    inner_trans_matrix_list = inner_trans_matrix_1change,
    total_time = total_time,
    births_net_migration_deaths_figures = births_net_migration_deaths_figures,
    birth_migration_deaths_proportions = birth_migration_deaths_proportions)
# see the change in population - plot 1
dpm::plot_dpm_with_growth(population_at_each_year_b) +
  labs(title = "In year 10 there is 10 times usual injection of births/migrations/deaths",
       subtitle = "changed transitions model")
# see the sankey - plot 2
dpm::create_sankey(
  population_at_each_year_b,
  inner_trans_matrix_list=inner_trans_matrix_1change) +
  labs(title = paste0("BASE SCENARIO - no change to inner trans \n ",
                      " + in 3 iterations trans CS1-->CS2 is down to 0",
                      "\n"))
