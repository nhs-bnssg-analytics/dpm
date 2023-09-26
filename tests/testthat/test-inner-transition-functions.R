
test_that("decrease CS1 to CS2 by 10% - proportional"){
  inner_trans_matrix <- matrix(
    data = c(0.8 ,0,0,0,0,
             0.15,1,0,0,0,
             0.03,0,1,0,0,
             0.01,0,0,1,0,
             0.01,0,0,0,1),
    nrow = 5,
    ncol = 5)

  desired_output_matrix_proportional_change <- matrix(
    data = c(0.8  * 1.017647,0,0,0,0,
             0.15 * 9/10,1,0,0,0,
             0.03 * 1.017647,0,1,0,0,
             0.01 * 1.017647,0,0,1,0,
             0.01 * 1.017647,0,0,0,1),
    nrow = 5,
    ncol = 5)

  output_matrix_proportional_change <- scalar_from_to(
    inner_trans_matrix,
    from_cs = 1,
    to_cs = 2,
    scalar_change = 0.9,
    method = "take proportionally from other changes"
  )

  expect_equal(round(desired_output_matrix_proportional_change,5),
               round(output_matrix_proportional_change,5))
}


test_that("decrease CS1 to CS2 by 10% - from no change"){
  inner_trans_matrix <- matrix(
    data = c(0.8 ,0,0,0,0,
             0.15,1,0,0,0,
             0.03,0,1,0,0,
             0.01,0,0,1,0,
             0.01,0,0,0,1),
    nrow = 5,
    ncol = 5)

  desired_output_matrix_from_no_change <- matrix(
    data = c(0.815     ,0,0,0,0,
             0.15 * 0.9,1,0,0,0,
             0.03 * 1  ,0,1,0,0,
             0.01 * 1  ,0,0,1,0,
             0.01 * 1  ,0,0,0,1),
    nrow = 5,
    ncol = 5)

  output_matrix_from_no_change <- scalar_from_to(
    inner_trans_matrix,
    from_cs = 1,
    to_cs = 2,
    scalar_change = 0.9,
    method = "take from no change"
  )

  expect_equal(round(desired_output_matrix_from_no_change,5),
               round(output_matrix_from_no_change,5))


}
