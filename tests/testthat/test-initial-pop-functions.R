test_that("Sep 2023 initial pop gives expected values"){

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
    method = "CS props: Cleaned CMS CS. Total pop: GP Estimates scaled down 90% to match ONS",
    sql_con = sql_con,
    min_age = 17
  )

  expect_equal(initial_pop,
               expected_initial_pop)
}
