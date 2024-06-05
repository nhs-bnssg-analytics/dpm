
#' meta_run_dpm runs the entire DPM, using subfunctions from within this package
#' in such a way that the inputs to this function fully determine the outputs
#' @export
meta_run_dpm <- function(
    start_month,
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
    my_seed = 1,
    save_folder = NA
){

  if(!continuous_dpm){
    stop("not implemented within meta_run_dpm yet - please use ")
  }

  if(is.na(save_folder)){
    save_folder <- here::here("data",paste0("dpm_ct_",start_month, "seed",my_seed))
  }
  if (!file.exists(save_folder)) {
    dir.create(save_folder, recursive = TRUE)
    print("Saving outputs at:", save_folder)
  }


  # First get the initial population
  initial_population <- dpm::get_initial_population(
    start_month = start_month,
    source_or_preload = source_or_preload,
    method = get_initial_population_method,
    sql_con = dpm::get_sql_con(),
    min_age = min_age,
    age_groups = age_groups)

  # Second define the inner transition matrix
  inner_trans_matrix <- dpm::get_transition_numbers(
    sql_con = dpm::get_sql_con(),
    orig_month_start_date = as_date(paste0(start_month,"-01")),
    method=get_transition_numbers_method,
    compare_against=1,
    age_groups = age_groups)

  if(1==1){
    # This should be brought outside of this function and standardised somehow.
    # It gets you to inner_trans_rate_matrix
    warning("using predefined transition rate matrix!")
    get_transition_numbers_method <- "Using predefined transition rate matrix 2020-12-01 to 2024-02-01"
    inner_trans_rate_tbl <-
      readr::read_csv(paste0(
        "C:/GitHub/Core-Segments-Over-Time/outputs/",
        "2024-06-04_transition_rate_matrix_using2020-12-01to2024-02-01",
        "observed_older_rates.csv"),
        show_col_types=F) |>
      mutate(age_cs_state_prev = ordered(
        age_seg_state_prev,
        levels=levels(initial_population$age_cs_state)),
        age_cs_state_orig = ordered(
          age_seg_state_orig,
          levels=levels(initial_population$age_cs_state))) |>
      select(age_cs_state_prev,
             age_cs_state_orig,
             transition_rate_estimate) |>
      arrange(age_cs_state_orig, age_cs_state_prev)

    inner_trans_rate_matrix <- inner_trans_rate_tbl |>
      pivot_wider(names_from=age_cs_state_orig,
                  values_from=transition_rate_estimate) |>
      select(-age_cs_state_prev) |>
      as.matrix()
    rownames(inner_trans_rate_matrix) <- inner_trans_rate_tbl$age_cs_state_prev |>
      unique()
  }

  # Third work out the numbers of comers and goers
  births_immigration_emigration_figures <- dpm::get_numbers_births_migrations_deaths(
    method = get_numbers_births_migrations_deaths_method,
    forecast_variant = ons_forecast_variant,
    combine_immigration_emigration = combine_immigration_emigration,
    date_of_year_zero = as_date(paste0(start_month,"-01"))
  ) |>
    convert_yearly_to_monthly() |>
    filter(event!="deaths")

  # Fourth work out the proportions of comers and goers by state
  birth_migration_proportions <- dpm::get_births_migrations_deaths_proportions(
    start_month = start_month,
    source_or_preload = "source",
    method = get_births_migrations_deaths_proportions_method,
    sql_con = dpm::get_sql_con(),
    min_age = 17,
    compare_against=12,
    combine_immigration_emigration = FALSE,
    output_proportions_or_numbers = "proportions",
    age_groups = T) |>
    filter(event!="deaths") |>
    select(age_cs_state, age_group,state_name, event, prop)

  # Fifth, penultimately, combine this to work out exactly how many from each
  # state, and at what time, are coming and going
  monthly_entrants_exits <- births_immigration_emigration_figures |>
    full_join(birth_migration_proportions, by=c("event"),
              relationship="many-to-many") |>
    mutate(value = round(prop*value)) |>
    select(month, age_cs_state, event, value)

  # Lastly - run the DPM!
  run_dpm_ct(
    initial_population = initial_population,
    inner_trans_rate_matrix = inner_trans_rate_matrix,
    monthly_entrants_exits = monthly_entrants_exits,
    start_time = 0,
    seed = my_seed,
    start_month = start_month)

  # Extra extra lastly - save the parameters
  # all the inputs in one .xlsx file
  wb <- openxlsx::createWorkbook()
  addWorksheet(wb, "about")
  about_tbl <-  tribble(~"Dynamic Population Model",~"DPM",
                        "Package Version",as.character(packageVersion("dpm")),
                        "Created Date",as.character(Sys.Date()),
                        "Start Month",start_month,
                        "End of Time Window",as_date(paste0(start_month,"-01")) %m+%
                          months(max(monthly_entrants_exits$month)) %>%
                          format("%Y-%m"),
                        "Method for Initial Population",
                        get_initial_population_method,
                        "Method for Inner Transitions",
                        get_transition_numbers_method,
                        "Mwthod for Birth/Migration/Death values",
                        get_numbers_births_migrations_deaths_method,
                        "Method for Birth/Migration/Death proportions",
                        get_births_migrations_deaths_proportions_method,
                        "combine_immigration_emigration",
                        as.character(combine_immigration_emigration),
                        "min_age",
                        as.character(min_age),
                        "age_groups",
                        as.character(age_groups),
                        "ons_forecast_variant",
                        ons_forecast_variant,
                        "seed",
                        as.character(my_seed),
                        "save location",
                        save_folder
  )
  writeData(wb, "about",about_tbl)

  addWorksheet(wb, "initial_population")
  writeData(wb, "initial_population",initial_population)
  addWorksheet(wb, "inner_trans_rate_matrix")
  writeData(wb, "inner_trans_rate_matrix",inner_trans_rate_matrix)
  addWorksheet(wb, "bm_vals")
  writeData(wb, "bm_vals",births_immigration_emigration_figures)
  addWorksheet(wb, "bm_props")
  writeData(wb, "bm_props",birth_migration_proportions)
  addWorksheet(wb, "outputs_location")
  writeData(wb, "outputs_location",paste0("outputs are saved as .rds files in ",save_folder))

  file_name <- paste0(save_folder,"/",Sys.Date(),"-",
                      "DPM-params-from",start_month,"seed",my_seed,".xlsx")

  openxlsx::saveWorkbook(wb,
                         file_name,
                         overwrite = T)

  print(paste0("parameters file saved at ",file_name))
}

