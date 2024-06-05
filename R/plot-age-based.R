

#' given an input_tbl with columns containing age_cs_state_col - plot them as blobs
#' using the value_col column for the blob sizing
#' @import stringr
#' @export
plot_age_based_blobs <- function(input_tbl,
                                 age_cs_state_col = "age_cs_state",
                                 value_col = "value",
                                 blob_labels = T,
                                 label_type = "all"){

  if(!(all(c(age_cs_state_col, value_col) %in% colnames(input_tbl)))){
    stop(paste0(age_cs_state_col," and ",value_col," must be columns in input_tbl"))
  }

  if(!(all(c("age_group", "state_name") %in% colnames(input_tbl)))){
    input_tbl <- input_tbl |>
      mutate(state_name = str_remove(!!sym(age_cs_state_col), ".*__"),
             age_group = str_remove(!!sym(age_cs_state_col), "__.*"))
  }

  plot_out <- input_tbl |>
    dpm:::add_age_cs_state_col() |>
    mutate(perc_text = paste0(round(100*!!sym(value_col) / sum(!!sym(value_col)),1),"%"),
           value_text = scales::comma(!!sym(value_col)),
           all_text = paste0(value_text,"\n",perc_text)) |>
    bind_rows(tibble(state_name = "CS0",age_group=factor("17-29"))) |>
    ggplot(aes(x=age_group, y=state_name)) +
    scale_size(range = c(1, 24), name = "Population (M)") +
    geom_point(aes(size=!!sym(value_col), fill = state_name), shape=21) +
    scale_y_discrete(breaks = c(paste0("CS",1:5))) +
    scale_fill_manual(values=dpm::core_seg_cols_greenred) +
    theme(legend.position="none") +
    labs(x="Age Group",
         y="Core Segment",
         title = "Forty States : 5 Core Segments and 8 Age Groups",
         subtitle = "Size determined by...")

  if(blob_labels){
    if(label_type == "percentages"){
      plot_out <- plot_out + geom_text(aes(label=perc_text),vjust=2)}
    if(label_type == "values"){
      plot_out <- plot_out + geom_text(aes(label=value_text),vjust=2)}
    if(label_type == "all"){
      plot_out <- plot_out + geom_text(aes(label=all_text),vjust=2)}
  }

  return(plot_out)
}

#' @export
plot_either_core_seg_or_age_stacked <- function(input_tbl,
                                                fill_col = "age_group",
                                                age_cs_state_col = "age_cs_state",
                                                time_col = "month",
                                                value_col = "value",
                                                bar_labels = T,
                                                label_type = "all",
                                                facet=F){

  if(sum(c("age_group","state_name") %in% fill_col)!=1){
    stop("fill_col must be age_group or state_name")
  }

  if(facet){
    input_tbl <- input_tbl |>
      mutate(facet_col = !!sym(fill_col))
  }

  if(!(all(c(age_cs_state_col, value_col) %in% colnames(input_tbl)))){
    stop(paste0(age_cs_state_col," and ",value_col," must be columns in input_tbl"))
  }

  if(!(all(c("age_group", "state_name") %in% colnames(input_tbl)))){
    input_tbl <- input_tbl |>
      mutate(state_name = str_remove(!!sym(age_cs_state_col), ".*__"),
             age_group = str_remove(!!sym(age_cs_state_col), "__.*"))
  }

  plot_out <- input_tbl |>
    group_by(!!sym(time_col)) %>%
    mutate(perc_text = paste0(round(100*!!sym(value_col) / sum(!!sym(value_col)),1),"%"),
           value_text = scales::comma(!!sym(value_col)),
           all_text = paste0(value_text,"\n",perc_text)) |>
    ungroup() |>
    mutate(!!sym(fill_col) := forcats::fct_rev(!!sym(fill_col))) |>
    ggplot(aes(x=!!sym(time_col), y=!!sym(value_col), fill = !!sym(fill_col))) +
    geom_bar(position="stack",stat="identity") +
    theme(legend.position="top") +
    labs(x=paste0("Time (",time_col,")"),
         title = "Forty States : 5 Core Segments and 8 Age Groups",
         subtitle = "Size determined by...")

  if(fill_col == "state_name"){
    plot_out <- plot_out +
      scale_fill_manual(values=dpm::core_seg_cols_greenred)
  }

  if(facet){
    plot_out <- plot_out +
      facet_wrap(~facet_col,scale="free_y",nrow=1)

  }

  if(bar_labels){
    if(label_type == "percentages"){
      plot_out <- plot_out + geom_text(aes(label=perc_text),vjust=2)}
    if(label_type == "values"){
      plot_out <- plot_out + geom_text(aes(label=value_text),vjust=2)}
    if(label_type == "all"){
      plot_out <- plot_out + geom_text(aes(label=all_text),vjust=2)}
  }

  return(plot_out)
}

