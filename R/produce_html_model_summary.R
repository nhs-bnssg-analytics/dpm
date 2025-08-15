
#' Given a simulation model output folder from DPMmicrosim, create a summary
#' document that details all the
#' @param model_folder folder string of outputs
#' @param output_folder_loc folder string to save outputs, if blank same as model_folder
produce_html_model_summary <- function(
    model_folder_loc,
    output_folder_loc = NULL,
    only_show_bnssg = FALSE,
    model_nickname = NA){

  if(is.null(output_folder_loc)){
    output_folder_loc <- model_folder_loc[1]}
  if(length(model_folder_loc)>1){
    warning("default save output is first folder provided")
  }
  # qmd_path <- fs::path_package("inst","reports", "model_summary.qmd", package = "dpm")
  qmd_path <- fs::path_package("reports", "model_summary.qmd", package = "dpm")
  output_file <- "model_summary.html"

  browser()

  quarto::quarto_render(
    input = qmd_path,
    output_file = output_file,
    execute_params = list(
      model_folder_loc =  model_folder_loc,
      only_show_bnssg =  only_show_bnssg,
      model_nickname = model_nickname
    )
  )
  # copy the generated file from .qmd location to output_folder_loc
  orig_html_loc <- paste0(
    here::here(),"/",
    # stringr::str_remove(qmd_path,"model_summary.qmd"),
    output_file)
  if(!file.exists(orig_html_loc)){stop("don't know where the quarto output has gone :(")}
  file.copy(orig_html_loc,
            paste0(output_folder_loc,"/",output_file),
            overwrite = TRUE)
  file.remove(orig_html_loc)  # Delete original after copying

  return(paste0(output_folder_loc,"/",output_file))

}

if(0==1){
  dpm:::produce_html_model_summary(
    model_folder_loc = paste0("C:/GitHub/DPMmicrosim/outputs/",
                              c("areaSGlos-sims1-N5640-seed1531-2025-03-27",
                                "areaNSom-sims1-N3875-seed1531-2025-03-27",
                                "areaBris-sims1-N10485-seed1531-2025-03-27")))
}
