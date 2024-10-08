
#' create Sankey diagram from a DPM run
#' @param population_at_each_year output from dpm::run_dpm
#' @param inner_trans_matrix_list inner transition matrix
#' @param save logical whether to save output as png
#' @param save_filename default NA, what filepath should output be. Only works if save=T
#' @param index boolean, whether to show outputs as indexed with initial population as 100
#' @import ggplot2
#' @export
create_sankey <- function(population_at_each_year,
                          inner_trans_matrix_list,
                          save=FALSE,
                          save_filename = NA,
                          index = FALSE){

  if(index==T){
    p <- population_at_each_year |>
      filter(year==min(year)) |>
      summarise(pop = sum(population)) |>
      pull(pop)
    population_at_each_year <- population_at_each_year |>
      mutate(population = round(100*population/p))
  }

  links_df <- make_links_df(population_at_each_year,
                            inner_trans_matrix_list)

  year_options <- population_at_each_year |> pull(year) |> unique()
  # plot results ------------------------------------------------------------

  biggest_total_pop <- population_at_each_year |>
    group_by(year) |>
    summarise(population=sum(population)) |>
    pull(population) |>
    max()

  space_between_sankey_blobs <- biggest_total_pop / 20

  # text labels at beginning and end of run
  text_labels_df <- tibble(
    year = rep(c(min(year_options),max(year_options)),each=5),
    cs = paste0("CS",rep(1:5,times=2))
  ) |>
    left_join(
      population_at_each_year,
      by = c("year",
             "cs"="state_name")) |>
    mutate(cs_numeric = as.numeric(stringr::str_remove(cs,"CS"))) |>
    group_by(year) |>
    #have to do a cumulitive sum for Y axis location as is stacked
    mutate(pop_pos = cumsum(population) - population/2 +
             space_between_sankey_blobs * (cs_numeric-1)) |>
    ungroup() |>
    mutate(orig_pop_label = scales::comma(round(population,-(floor(log10(biggest_total_pop)) - 2))))

  my_sankey_plot <- ggplot2::ggplot(links_df) +
    ggsankey::geom_sankey(
      mapping = aes(x = year,
                    next_x = year+1,
                    value = transition_amount,
                    node = source_node,
                    next_node = target_node,
                    space = space_between_sankey_blobs,
                    fill = factor(source_cs_name),
                    label = year),
      flow.alpha = 0.6,
      node.color = "gray30",
      type = "alluvial") +
    # add first year
    geom_text(data = text_labels_df |> filter(year==min(year)),
              mapping = aes(x=year,y=pop_pos,label=orig_pop_label),
              hjust=1.1) +
    # add last year label
    geom_text(data = text_labels_df |> filter(year==max(year)),
              mapping = aes(x=year,y=pop_pos,label=orig_pop_label),
              hjust=-0.1) +
    bnssgtheme() +
    scale_fill_manual(values = core_seg_cols_greenred) +
    # want to show each year
    scale_x_continuous(breaks = year_options,
                       limits = c(min(year_options)-2,
                                  max(year_options)+2)) +
    # don't show y limits as gaps between Core Segments make scale off
    scale_y_continuous(breaks = NULL) +
    labs(
      title = "Inner Transitions between Core Segment (CS) groups",
      subtitle = "NOT SHOWN - births / net_migration / deaths",
      x="Year",
      y="Population",
      fill = "Core Segment (bigger number = higher CMS)",
      caption = paste0(Sys.Date(),"\n",
                      "dpm R package fn create_sankey")
    )

  my_sankey_plot

  if(save){
    if(is.na(save_filename)){
      save_filename <- paste0( here::here("figs"),"/",Sys.Date(),"-sankey.png")}
    ggsave(
      filename = save_filename,
      my_sankey_plot,
      width = 10,
      height = 5)
  }

  return(my_sankey_plot)
}

