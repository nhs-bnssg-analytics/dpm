
#' create Inner Transition Matrix as a plot
#' @param inner_trans_matrix_list inner transition matrix
#' @param save logical whether to save output as png
#' @param transition_to_plot integer, which transition year to plot for
#' @param save_filename default NA, what filepath should output be. Only works if save=T
#' @import ggplot2
#' @import forcats
#' @export
plot_inner_transition_matrix <- function(inner_trans_matrix_list,
                                         transition_to_plot = 1,
                                         save=FALSE,
                                         save_filename = NA){

  inner_trans_matrix <- inner_trans_matrix_list[[transition_to_plot]]

  my_inner_trans_plot <- inner_trans_matrix %>%
    dpm::from_matrix_to_long_tbl() %>%
    mutate(label_text = paste0(round(100*transition_prop,2),"%")) %>%
    # make a factor and reverse order so plots matching intuition, with
    # CS1 at the top row
    mutate(from = factor(from)) %>%
    mutate(from = fct_rev(from)) %>%
    ggplot(aes(x=to,
               y=from,
               fill=transition_prop)) +
    geom_tile() +
    scale_fill_gradient(low="gray", high="red", limits=c(0,1)) +
    geom_label(aes(label=label_text)) +
    labs(x="Core Segment transitioned to",
         y="Core Segment transitioned from",
         fill = "Proportion change",
         title = paste0("Inner Transition Matrix in year ",transition_to_plot))



  if(save){
    if(is.na(save_filename)){
      save_filename <- paste0( here::here("figs"),"/",Sys.Date(),"-inner-trans.png")}
    ggsave(
      filename = save_filename,
      my_inner_trans_plot,
      width = 10,
      height = 5)
  }

  return(my_inner_trans_plot)
}

