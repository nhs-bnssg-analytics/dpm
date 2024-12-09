test_that("01 North Somerset works as expected", {
  folder <- fs::path_package("extdata","tests_data","01_NSom",
                             package="dpm")

  observed_out <- dpm::run_dpm_age_based(paste0(folder,"/inputs"),
                                         print_intermediate_results = F,
                                         return_yearly_deaths = F,
                                         return_yearly_changes=F)

  expected_out <- readRDS(paste0(folder,"/expected_out.rds"))

  expect_equal(observed_out, expected_out)
})

test_that("04_full_without_transitions_weighted",{

  # test copied from DPMmicrosim package

  # Test that the model produces expected output for the input parameters
  # where we have
  # 5,000 people equally spread between 17-99 and each CS
  # all kinds of immigrations, births, and emigrations

  # with parameter weight_external_moves_based_on_current_pop = TRUE

  folder <- fs::path_package(
    "extdata","tests_data","04_full_without_transitions_weighted",package="dpm")

  observed_out <- dpm::run_dpm_age_based(paste0(folder,"/inputs"),
                                        print_intermediate_results = F,
                                        return_yearly_deaths = F,
                                        return_yearly_changes=F)

  expected_out <- readRDS(paste0(folder,"/expected_out.rds"))

  expect_equal(observed_out, expected_out)
})
