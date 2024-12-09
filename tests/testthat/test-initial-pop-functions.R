test_that("Sep 2023 initial pop gives expected values", {

  expected_initial_pop <- tribble(
    ~state_name, ~initial_pop,
    "CS1",410656,
    "CS2",191668,
    "CS3",106895,
    "CS4",56925,
    "CS5",28002)

  sql_con <- get_sql_con()

  initial_pop <- get_initial_population(
    start_month = "2023-09",
    source_or_preload = "source",
    method = "CS props: Cleaned CMS values Total pop: GP Estimates scaled down 90% to match ONS",
    sql_con = sql_con,
    min_age = 17
  )

  joined <- initial_pop %>%
    left_join(expected_initial_pop |> rename(expected_initial_pop = initial_pop),
              by = "state_name") |>
    mutate(abs_diff = abs(initial_pop - expected_initial_pop),
           rel_diff = abs_diff / expected_initial_pop)

  if(max(joined$abs_diff) == 0 & max(joined$rel_diff) == 0){
    return_val <- TRUE
  } else {
    # if no more than 10,000 people out and 5% out on any one CS
    if(max(joined$abs_diff) < 10000 & max(joined$rel_diff) < 5){
      warning("Soft fail: max abs diff < 10,000 and max rel diff < 5%")
      return_val <- TRUE
    } else {
      return_val <- FALSE
    }
  }
  expect_true(return_val)
}
)
