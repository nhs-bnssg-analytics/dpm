################################################################################
# Workflow example using SQL connections
################################################################################

library(dpm)
library(magrittr)
library(lubridate)

# PARAMETER CHOICES
month_of_interest <- "2023-09"
initial_population_method <- "CS props: Cleaned CMS CS. Total pop: GP Estimates scaled down 90% to match ONS"
transition_method <- "Using only patients with full records of data"
bmd_vals_method <- "use ONS closest year"
bmd_props_method <- "Matching at patient-level migrations in/out"

# requires Server environment variable to exist
sql_con <- dpm::get_sql_con()
# initial population
initial_population <- dpm::get_initial_population(
  start_month = month_of_interest,
  source_or_preload = "source",
  method = 1,
  sql_con = sql_con,
  min_age = 17)
# inner transition matrix - using default methods
inner_trans_matrix <- dpm::get_transition_numbers(
  sql_con = sql_con,
  method = transition_method,
  orig_month_start_date = as_date(paste0(month_of_interest,"-01"))) |>
  dpm::get_inner_trans_rate_from_transitions_tbl()
# total time taken
total_time <- 20
# Births/Migrations/Deaths projections from ONS
births_net_migration_deaths_figures <- dpm::get_numbers_births_migrations_deaths(
  method = bmd_vals_method,
  date_of_year_zero = as_date(paste0(month_of_interest,"-01")))
# proportional births/migrations/deaths
birth_migration_deaths_proportions <- dpm::get_births_migrations_deaths_proportions(
  start_month = month_of_interest,
  sql_con = sql_con,
  method = bmd_props_method,
  output_proportions_or_numbers = "proportions"
)

# Finally - we can run the dpm!
population_at_each_year <- dpm::run_dpm(
  initial_population = initial_population,
  inner_trans_matrix_list = inner_trans_matrix,
  total_time = total_time,
  births_net_migration_deaths_figures = births_net_migration_deaths_figures,
  birth_migration_deaths_proportions = birth_migration_deaths_proportions)
# visualise population output
dpm::create_sankey(population_at_each_year,
                   inner_trans_matrix)
# save the results
dpm::save_dpm_inputs_outputs()
