################################################################################
# Workflow example using simple dummy data - with different parameters having
# key fluctuations within years causing obvious visual differences
################################################################################

# The below is the same as running this one command
meta_run_dpm(
  start_month = "2023-03",
  continuous_dpm = T,
  source_or_preload = "source",
  get_initial_population_method =
    "CS props: Raw CMS values. Total pop: GP Estimates scaled down 90% to match ONS",
  get_transition_numbers_method =
    "Using only patients with full records of data",
  get_numbers_births_migrations_deaths_method =
    "use ONS closest year",
  get_births_migrations_deaths_proportions_method =
    "Matching at patient-level migrations in/out",
  combine_immigration_emigration = F,
  min_age = 17,
  age_groups = T,
  ons_forecast_variant = "normal",
  my_seed = 2,
  save_folder = NA
)

# The PopwerPoint slide in inst/extdata/DPM-indexing.pptx is helpful with
# visualising this.

library(dpm)
library(magrittr)
library(ggplot2)

start_month <- "2023-03"
my_seed <- 2
save_folder  <- here::here("data",paste0("dpm_ct_",start_month, "seed",my_seed))

initial_population <- dpm::get_initial_population(
  start_month = start_month,
  source_or_preload = "source",
  method = 3,
  sql_con = dpm::get_sql_con(),
  min_age = 17,
  age_groups = T)

# define the initial matrix
inner_trans_matrix <- dpm::get_transition_numbers(
  sql_con = dpm::get_sql_con(),
  orig_month_start_date = as_date(paste0(start_month,"-01")),
  method=1,
  compare_against=1,
  age_groups = T)


births_immigration_emigration_figures <- dpm::get_numbers_births_migrations_deaths(
  method = "use ONS closest year",
  forecast_variant = "normal",
  combine_immigration_emigration = FALSE,
  date_of_year_zero = as_date(paste0(start_month,"-01"))
) |>
  convert_yearly_to_monthly() |>
  filter(event!="deaths")

birth_migration_proportions <- dpm::get_births_migrations_deaths_proportions(
  start_month = start_month,
  source_or_preload = "source",
  method = 1,
  sql_con = dpm::get_sql_con(),
  min_age = 17,
  compare_against=12,
  combine_immigration_emigration = FALSE,
  output_proportions_or_numbers = "proportions",
  age_groups = T) |>
  filter(event!="deaths") |>
  select(age_cs_state, age_group,state_name, event, prop)

monthly_entrants_exits <- births_immigration_emigration_figures |>
  full_join(birth_migration_proportions, by=c("event"),
            relationship="many-to-many") |>
  mutate(value = round(prop*value)) |>
  select(month, age_cs_state, event, value)

# run scenarios -----------------------------------------------------------
run_dpm_ct(
  initial_population,
  inner_trans_matrix=1,
  monthly_entrants_exits,
  start_time = 0,
  seed = my_seed,
  start_month = start_month,
  save_folder = save_folder
)
