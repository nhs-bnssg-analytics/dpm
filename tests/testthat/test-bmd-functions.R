test_that("Sep 2023 birth/migration/death expected values", {

  expected_bmd_prop <- tibble(
    state_name = c("CS1", "CS1", "CS1", "CS2", "CS2", "CS2", "CS3", "CS3", "CS3",
                   "CS4", "CS4", "CS4", "CS5", "CS5", "CS5"),
    event = c("births", "deaths", "net_migration", "births", "deaths", "net_migration",
              "births", "deaths", "net_migration", "births", "deaths", "net_migration",
              "births", "deaths", "net_migration"),
    prop = c(0.881264973646382, 0.0535987748851455,0.625502675898769,
             0.100431241015812, 0.0765696784073507, 0.210239470262752,
             0.0164829899377096, 0.139217597104274, 0.0961736159404953,
             0.00182079540009583, 0.246972017262982, 0.0448553200495873,
             0, 0.483641932340248,0.023228917848396)
  )

  births_net_migration_deaths_figures <- get_births_migrations_deaths_proportions(
    start_month = "2023-01",
    method = 1,
    sql_con = get_sql_con(),
    min_age=17,
    compare_against=12,
    output_proportions_or_numbers = "proportions")

  expect_equal(
    births_net_migration_deaths_figures |> mutate(prop=round(prop,2)) |> select(event,state_name,prop),
               expected_bmd_prop |> mutate(prop=round(prop,2)) |> select(event,state_name,prop))
}
)
