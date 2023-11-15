test_that("Sep 2023 birth/migration/death expected values"){

  expected_bmd_prop <- tibble(
    state_name = c("CS1", "CS1", "CS1", "CS2", "CS2", "CS2", "CS3", "CS3", "CS3",
                   "CS4", "CS4", "CS4", "CS5", "CS5", "CS5"),
    event = c("births", "deaths", "net_migration", "births", "deaths", "net_migration",
              "births", "deaths", "net_migration", "births", "deaths", "net_migration",
              "births", "deaths", "net_migration"),
    prop = c(0.881804202972834, 0.046831955922865, 0.615951698352512, 0.0999487442337263,
             0.0729371638462548, 0.211478247083537, 0.0159917990773962, 0.143906598452053,
             0.102337133873248, 0.00215274218349564, 0.249901613537977, 0.0475485856971106,
             0.000102511532547412, 0.48642266824085, 0.0226843349935924)
  )

  births_net_migration_deaths_figures <- get_births_migrations_deaths_proportions(
    start_month = "2023-09",
    method = 1,
    sql_con = get_sql_con(),
    min_age=17,
    compare_against=12,
    output_proportions_or_numbers = "proportions")

  expect_equal(births_net_migration_deaths_figures,
               expected_bmd_prop)
}
