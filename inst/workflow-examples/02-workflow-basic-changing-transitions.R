################################################################################
# Workflow example using simple dummy data - and then showcasing how
# the changing_inner_trans_matrix() function works in practice
################################################################################

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
)

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
dpm::run_dpm(
  initial_population = initial_population,
  inner_trans_matrix_list = inner_trans_matrix_default,
  total_time = total_time,
  births_net_migration_deaths_figures = births_net_migration_deaths_figures,
  birth_migration_deaths_proportions = birth_migration_deaths_proportions) %>%
  # plot the Sankey
  dpm::create_sankey(inner_trans_matrix_list=inner_trans_matrix_default) +
  labs(title = "BASE SCENARIO - no change to inner trans \n \n")

# ONE CHANGE
inner_trans_matrix_1change <- inner_trans_matrix_default %>%
  # make a change
  changing_inner_trans_matrix(from_cs = 1,
                              to_cs = 2,
                              scalar_change = 0.9,
                              over_n_iterations = 5)
dpm::run_dpm(
  initial_population = initial_population,
  inner_trans_matrix_list = inner_trans_matrix_1change,
  total_time = total_time,
  births_net_migration_deaths_figures = births_net_migration_deaths_figures,
  birth_migration_deaths_proportions = birth_migration_deaths_proportions) %>%
  # plot the Sankey
  create_sankey(inner_trans_matrix_list=inner_trans_matrix_1change) +
  labs(title = "BASE + \nCS1-->CS2 10% transition reduction in 5 years \n")

# TWO CHANGES
inner_trans_matrix_2changes <- inner_trans_matrix_1change %>%
  # make another change
  changing_inner_trans_matrix(
    from_cs = 2,
    to_cs = 1,
    scalar_change = 0,
    over_n_iterations = 10)
dpm::run_dpm(
  initial_population = initial_population,
  inner_trans_matrix_list = inner_trans_matrix_2changes,
  total_time = total_time,
  births_net_migration_deaths_figures = births_net_migration_deaths_figures,
  birth_migration_deaths_proportions = birth_migration_deaths_proportions) %>%
  # plot the Sankey
  create_sankey(inner_trans_matrix_list=inner_trans_matrix_2changes) +
  labs(title = paste0("BASE + ",
                      "\nCS1-->CS2 10% transition reduction in 5 years + ",
                      "\nCS2-->CS1 100% transition reduction in 10 years"))




