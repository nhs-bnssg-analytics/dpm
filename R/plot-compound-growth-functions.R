


#' plot the DPM outputs with growth factors
#' @param dpm_output an output from dpm::run_dpm, or anything with columns year,
#' state_name, type, value
#' @param growth_factors what growth lines to plot - note a compound growth factor
#' of 1\% would be growth_factors = 1.01
#' @param add_compound_growth_label boolean - do you want the label of compound
#' growth in the plot or not
#' import @dplyr
#' import @tidyr
#' import @ggplot2
#' @export
plot_dpm_with_growth <- function(dpm_output,
                                 growth_factors = c(1.01,1.02,1.03),
                                 add_compound_growth_label = F){

  if(!(all(c("year","state_name") %in% names(dpm_output)))){stop("need columns year and state_name")}
  if(ncol(dpm_output)<3){stop("dpm_output not enough cols")}

  # pivot the data if it's not in a type/value split ie tidy
  if(identical(sort(names(dpm_output)),
               c("population","state_name","year"))){
    dpm_output <- dpm_output %>%
      pivot_longer(cols="population",names_to = "type")
  }

  # get the baseline for each
  if(!("baseline_value" %in% names(dpm_output))){
    dpm_output <- dpm_output %>%
      left_join(dpm_output %>% filter(year==min(year)) %>%
                  rename(baseline_value=value) %>% select(-year),
                by = names(dpm_output %>% select(-value,-year)))
  }
  # create a POD splitting if none exists
  if(!("dpm_pod_splitting" %in% names(dpm_output))){
    dpm_output <- dpm_output %>%
      mutate(dpm_pod_splitting = "All PODs")
  }

  dpm_output <- dpm_output %>%
    mutate(value = ifelse(type=="cost",value/1e4,value),
           baseline_value = ifelse(type=="cost",baseline_value/1e4,baseline_value)) %>%
    mutate(type = case_when(
      type == "activity" ~ "Activity",
      type == "cost" ~ "Cost (Million Pounds)",
      TRUE ~ type
    ))



  #get the growths
  if(!is.na(growth_factors[1])){
    growths_tbl <- create_growth_tbl(length(unique(dpm_output$year)),
                                     growth_factors,
                                     1) %>%
      mutate(growth_label = paste0("Compound ",round(100*growth_factor-100,5),"% growth"))
    growths_tbl <- cross_join(
      growths_tbl %>% mutate(year = year + min(dpm_output$year)-1),
      dpm_output %>% filter(year==min(year)) %>%
        select(state_name, dpm_pod_splitting, baseline_value, type) %>%
        group_by(dpm_pod_splitting, type) %>%
        summarise(baseline_value=sum(baseline_value),.groups="drop")) %>%
      mutate(value = baseline_value * real_terms_growth)
  }


  # get the colours from the package
  core_seg_cols_greenred <- c(CS1 = "#77A033",
                              CS2 = "#C4D22A",
                              CS3 = "#FFE34D",
                              CS4 = "#FFA833",
                              CS5 = "#FF6C53")
  # the plot
  plot_out <-
    ggplot(dpm_output, aes(x=year,y=value)) +
    geom_bar(aes(fill=state_name),stat="identity",position="stack",col="darkgrey") +
    scale_fill_manual(values = core_seg_cols_greenred) +
    facet_grid(type~dpm_pod_splitting,scale="free_y") +
    labs(x="Year Into the Future",
         y="",
         fill="Core Segment",
         linetype= "Compound Growth",
         subtitle = "Cost Projections For DPM")
  if(!is.na(growth_factors[1])){
    plot_out <- plot_out +
      geom_line(
        data = growths_tbl,
        mapping = aes(linetype=growth_label))
  }

  if(add_compound_growth_label){
    # calculate the real terms compound growth over 20 years
    compound_growth_at_end <- dpm_output %>%
      filter(year==max(year)) %>%
      group_by(year, type, dpm_pod_splitting) %>%
      summarise(value=sum(value),baseline_value=sum(baseline_value),.groups="drop") %>%
      mutate(growth_change = value / baseline_value) %>%
      mutate(growth_factor = growth_change^(1/(year-1))) %>%
      select(type, dpm_pod_splitting, growth_factor)
    # create a label parameter to plot for the compound growth effect
    dpm_output <- dpm_output %>%
      left_join(compound_growth_at_end, by = c("type","dpm_pod_splitting")) %>%
      mutate(compound_growth_label = paste0(round(100*(growth_factor-1),3),
                                            "% compound growth \nover full time period"))
    label_position_tbl <- dpm_output %>%
      group_by(year, dpm_pod_splitting, type) %>%
      mutate(total_no_cs = sum(value)) %>%
      group_by(dpm_pod_splitting, type) %>%
      mutate(xpos = min(year)-1,
             ypos = max(total_no_cs))

    plot_out <- plot_out +
      # put compound growth in top left hand corner of chart
      geom_label(data = label_position_tbl,
                 mapping = aes(x=xpos,
                               y=ypos,
                               label=compound_growth_label),
                 hjust=0, vjust=1)
  }

  return(plot_out)
}

#' A subfunction of plot_dpm_with_growth.
#' @param num_years number of years in data
#' @param growth_factor 1 is no growth, 1.1 is yearly 10\% compound growth
#' @param start_val the starting value to grow from
create_growth_tbl <- function(num_years, growth_factor, start_val){
  tibble(year = 1:num_years) %>%
    cross_join(tibble(growth_factor = growth_factor)) %>%
    rowwise() %>%
    mutate(real_terms_growth = growth_factor^(year-1),
           growth = start_val*real_terms_growth)
}

