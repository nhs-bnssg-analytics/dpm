#' Create data forecasting given type_name with the DPM output
#'
#' Given the dpm_output from run_dpm, and the breakdown
#' of core_segment_by_type showing the amount each type_name used in a year
#' by the average person in that Core Segment, create the forecast.
#'
#' Output can be plotted with plot_dpm_with_growth function
#'
#' @param dpm_output dataframe output from run_dpm function
#' @param core_segment_by_type dataframe with columns state_name and yearly_amount
#' @param type_name char character name for what you want the value of the type
#' column to be called in the output ie what is the measure in core_segments_by_type
#' @import dplyr
#' @import tidyr
#' @export
create_type_forecasts_from_population <- function(dpm_output,
                                                  core_segment_by_type,
                                                  type_name){

  if(!is.character(type_name)){
    stop("type_name should be a character string eg 'activity'")}
  if(!(all(names(core_segment_by_type) %in% c("state_name","yearly_amount")))){
    stop("core_segment_by_type should only have columns 'state_name' and 'yearly_amount'")}

  # reshape dpm_output to be ready for function run
  if(all(names(dpm_output) %in% c("year","state_name","population"))){
    dpm_output <- dpm_output %>%
      tidyr::pivot_longer(cols=population, names_to = "type")
  }

  # check the input is correct - easy mistake to make
  if(sum(core_segment_by_type$yearly_amount)==1){
    warning("core_segment_by_type yearly_amounts sum to 1 - check you haven't put in proportions by accident?")
  }

  # check we have all the right state_names
  state_names_1 <- unique(population_at_each_year$state_name)
  state_names_2 <- unique(core_segment_by_type$state_name)
  if(!(all(state_names_1 %in% state_names_2) && all(state_names_2 %in% state_names_1))){
    stop("missing some state_names values somewhere")}

  # let's go - actually quite a simple calc
  output <- dpm_output %>%
    left_join(core_segment_by_type, by = "state_name") %>%
    mutate(value = value*yearly_amount,
           type = type_name) %>%
    select(-yearly_amount)

  output <- bind_rows(dpm_output,output) %>%
    arrange(year, type_name)

  return(output)
}
