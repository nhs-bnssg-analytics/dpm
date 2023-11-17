##########
# Workflow example using simple dummy data
##########


library(dpm)
library(magrittr)


##########
initial_population <- tibble::tribble(
  ~state_name, ~initial_pop,
  "CS1",2000,
  "CS2",1000,
  "CS3",500,
  "CS4",200,
  "CS5",100
)

inner_trans_matrix <- matrix(
  data = c(0.80, 0.20, 0.00, 0.00, 0.00,
           0.20, 0.80, 0.00, 0.00, 0.00,
           0.00, 0.10, 0.80, 0.10, 0.00,
           0.00, 0.01, 0.04, 0.90, 0.05,
           0.01, 0.01, 0.01, 0.02, 0.95),
  nrow = 5,
  ncol = 5) %>% t()

### lets say we can improve our inner transition rates a bit
# CS1 --> CS2 we'll get 10% better over 5 years
inner_trans_matrix <- changing_inner_trans_matrix(inner_trans_matrix,
                                                  from_cs = 1,
                                                  to_cs = 2,
                                                  scalar_change = 0.9,
                                                  over_n_iterations = 5)
# CS2 --> CS1 unfortunately no one will move into the healthier category after 10 years
inner_trans_matrix <- changing_inner_trans_matrix(inner_trans_matrix,
                            from_cs = 2,
                            to_cs = 1,
                            scalar_change = 0,
                            over_n_iterations = 10)

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

population_at_each_year <- dpm::run_dpm(
  initial_population = initial_population,
  inner_trans_matrix_list = inner_trans_matrix,
  total_time = total_time,
  births_migrations_deaths_figures = births_net_migration_deaths_figures,
  birth_migration_deaths_proportions = birth_migration_deaths_proportions)

dpm::create_sankey(population_at_each_year,
                   inner_trans_matrix)




