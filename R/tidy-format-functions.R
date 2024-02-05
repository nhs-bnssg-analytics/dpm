
#' takes inner_trans_matrix and makes it a long tibble object
#' @param inner_trans_matrix inner transition matrix
#' @export
from_matrix_to_long_tbl <- function(inner_trans_matrix){

  if(!is.matrix(inner_trans_matrix)){stop("needs matrix input")}

  if(is.null(dimnames(inner_trans_matrix))){
    # if no names - make them and assume ordering is by CS name
    cs_names <- paste0("CS",1:nrow(inner_trans_matrix))
    dimnames(inner_trans_matrix)<-list(from = cs_names,to = cs_names)
  }

  # transform it
  inner_trans_long_tbl <- inner_trans_matrix |>
    tibble::as_tibble() |>
    dplyr::mutate(from = paste0("CS",
                                dplyr::row_number())) |>
    tidyr::pivot_longer(cols = dplyr::contains("CS"),names_to="to",
                        values_to = "transition_prop")
  return(inner_trans_long_tbl)
}

#' same as from_matrix_to_long_tbl but for list input
#' @param inner_trans_matrix_list list of inner transition matrices
#' @export
from_list_to_long_tbl <- function(inner_trans_matrix_list){
  #
  stopifnot("needs list input" = is.list(inner_trans_matrix_list))

  num_cs <- nrow(inner_trans_matrix_list[[1]])

  inner_trans_long_tbl <- inner_trans_matrix_list |>
    lapply(from_matrix_to_long_tbl) |>
    purrr::list_rbind()

  total_time <- nrow(inner_trans_long_tbl) / num_cs^2

  inner_trans_long_tbl <- inner_trans_long_tbl |>
    mutate(year = rep(1:total_time,each=num_cs^2))

}

#' create a links_df object for use in plotting a sankey
#' @param population_at_each_year output of dpm::run_dpm()
#' @param inner_trans_matrix_list inner transition matrix
#' @export
make_links_df <- function(population_at_each_year,
                          inner_trans_matrix_list){
  year_options <- unique(population_at_each_year$year) %>% sort()
  # each year except for last as no transition once at time n - remove
  year_options_end_cut <- year_options[-length(year_options)]


  # inner transition fun
  inner_trans_matrix_list <- check_inner_trans(inner_trans_matrix_list,
                                               length(year_options_end_cut))
  # add a nonsensical last transition outside of the time window, purely for the node
  # to exist so can be plotted later by Sankey.
  inner_trans_matrix_list[[length(year_options)]] <- inner_trans_matrix_list[[1]]
  inner_trans_long_tbl <- from_list_to_long_tbl(inner_trans_matrix_list)

  #cs_lookup <- c("CS1"=0,"CS2"=1,"CS3"=2,"CS4"=3,"CS5"=4)
  num_cs <- length(unique(population_at_each_year$state_name))

  # the shell - frame ready to be filled in
  links_df_shell <- tibble(
    year = rep(year_options,each=num_cs^2),
    source_cs = rep(rep(1:num_cs,each=num_cs),length(year_options)),
    target_cs = rep(1:num_cs,times=num_cs*length(year_options))
  )

  # used to be called source_target_options_df but realised it
  # was the same as nodes_df --> what is the name and identifier for
  # each node (or blob) in the Sankey
  nodes_df <- tibble(
    year = rep(year_options,each=num_cs),
    cs = rep(1:num_cs,times=length(year_options))) |>
    mutate(string_name = paste("year",stringr::str_pad(year,2,pad=0),"cs",cs)) |>
    mutate(node = row_number())

  # this still uses the terminology from networkD3 (having links and nodes data frames),
  # but final plotting is using ggsankey
  links_df <- links_df_shell |>
    # get the multiplier from one section to the next
    left_join(inner_trans_long_tbl |> mutate(
      source_cs_name =from,
      from= as.numeric(stringr::str_remove(from,"CS")),
      to=as.numeric(stringr::str_remove(to,"CS")),
      # joining on year but inner trans indexed for  year 1 is
      # from year 0 to year 1 hence -1 in formula below
      year = year - 1),
      by=c("source_cs"="from","target_cs"="to", "year"="year")) |>
    # join onto the outputs so we know the population at time year
    left_join(population_at_each_year,
              by=c("source_cs_name"="state_name","year")) |>
    rowwise() |>
    # the amount that goes from source_cs into target_cs by the next gap
    mutate(transition_amount = population * transition_prop) |>
    ungroup() |>
    # factors is a way of getting to numeric iteration for the CS blob at time T
    # in an increasing numerical ordering
    mutate(source_string = paste("year",stringr::str_pad(year  ,2,pad=0),"cs",source_cs),
           target_string = paste("year",stringr::str_pad(year+1,2,pad=0),"cs",target_cs)
    ) |>
    left_join(nodes_df |> select(string_name, source_node=node),
              by=c("source_string"="string_name")) |>
    left_join(nodes_df |> select(string_name, target_node=node),
              by=c("target_string"="string_name")) |>
    select(year,
           source_cs,
           source_cs_name,
           source_node,
           target_cs,
           target_node,
           source_population = population,
           transition_prop,
           transition_amount
    )

  return(links_df)
}
