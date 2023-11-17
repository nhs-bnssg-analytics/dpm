
#' save all the inputs and outputs - pulls from environment variables
#' @import openxlsx
#' @export
save_dpm_inputs_outputs <- function(save_loc = here::here()){
  # all the inputs in one .xlsx file
  wb <- openxlsx::createWorkbook()
  addWorksheet(wb, "about")
  about_tbl <-  tribble(~"Dynamic Population Model",~"DPM",
                        "Created Date",as.character(Sys.Date()),
                        "Most Recent Data Month",month_of_interest,
                        "Time Window (years)",as.character(total_time),
                        "Method for Initial Population",initial_population_method,
                        "Method for Inner Transitions",transition_method,
                        "Method for Birth/Migration/Death proportions",bmd_method)
  writeData(wb, "about",about_tbl)

  # SANKEY
  # first get a temporary filename (we'll save then delete the image at the end)
  temp_filename <- paste0(here::here(),"/",
                          paste0(1e4*round(runif(10),3),collapse="-")
                          ,".png")
  create_sankey(population_at_each_year,inner_trans_matrix,save=T,
                save_filename = temp_filename)
  openxlsx::insertImage(wb,sheet="about",file=temp_filename,
                        startRow = nrow(about_tbl)+3,startCol=1)

  addWorksheet(wb, "initial_population")
  writeData(wb, "initial_population",initial_population)
  addWorksheet(wb, "inner_trans_matrix")
  writeData(wb, "inner_trans_matrix",inner_trans_matrix)
  addWorksheet(wb, "bmd_vals")
  writeData(wb, "bmd_vals",births_net_migration_deaths_figures)
  addWorksheet(wb, "bmd_props")
  writeData(wb, "bmd_props",birth_migration_deaths_proportions)
  addWorksheet(wb, "forecast_by_state_name")
  writeData(wb, "forecast_by_state_name",population_at_each_year)

  file_name <- paste0(save_loc,"/",Sys.Date(),"-",
                      "DPM-output",".xlsx")

  openxlsx::saveWorkbook(wb,
                         file_name,
                         overwrite = T)
  # clean up - delete Sankey image
  file.remove(temp_filename)

  print(paste0("file saved at ",file_name))
}
