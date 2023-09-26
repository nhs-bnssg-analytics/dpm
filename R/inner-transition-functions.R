
#' takes a starter inner_transition_matrix and creates a list of future matrices based on their scalar changee
#' @param inner_trans_matrix the original inner transition matrix
#' @param from_cs the core segment starting from
#' @param to_cs the core segment transitioning to
#' @param change_prop how much to change the value - between (-1,1). A value
#' of -1 is totally go to zero, a value of 1 is totally go to 1
#' @param over_n_iterations over how many discrete time steps for the change to happen over. Defaults to 1 ie immediately.
#' If scalar_change = 0.5 then
#' @param total_time discrete time steps - ie how many transition matrices to produce + 1
#' @method either "take from no change" or "take proportionally from other changes" - how to get the
#' proportions to add back to 1
#' @return the set of inner transitions
changing_inner_trans_matrix <- function(inner_trans_matrix,
                                        from_cs,
                                        to_cs,
                                        scalar_change,
                                        over_n_iterations = 1,
                                        total_time = 20,
                                        method = "take from no change"){

  # set up our shell - list output
  inner_trans_matrix_list <- list()
  # default fill the list with the inner transition matrix
  for(i in 1:total_time){inner_trans_matrix_list[[i]] <- inner_trans_matrix}

  if(over_n_iterations==1){
    # this function is just a wrapper for scalar_from_to with varying over_n_iterations
    # If over_n_iterations = 1 then it's a straight call of the data
    inner_trans_matrix_list[[2]] <- scalar_from_to(inner_trans_matrix,
                                                   from_cs,
                                                   to_cs,
                                                   scalar_change,
                                                   method)
  } else {
    # more complicated - have to change the amount scalar_change is each time to make
    # the overall scalar_change correct after over_n_iterations. Took some simple, but
    # annoying, linear algebra.

    # each time will change by how much?
    linear_change <- (inner_trans_matrix[from_cs,to_cs] * (1-scalar_change))/over_n_iterations
    # iterate through
    for(i in 2:(over_n_iterations+1)){
      scalar_change_i <- 1 - linear_change / inner_trans_matrix_list[[i-1]][from_cs, to_cs]
      inner_trans_matrix_list[[i]] <- scalar_from_to(inner_trans_matrix_list[[i-1]],
                                                     from_cs,
                                                     to_cs,
                                                     scalar_change_i,
                                                     method)
    }
  }
  # make the end transition matrix permeate to the end of the time period
  for(i in (over_n_iterations+1):total_time){inner_trans_matrix_list[[i]] <- inner_trans_matrix_list[[over_n_iterations+1]]}

  return(inner_trans_matrix_list)
}


from_matrix_to_long_tbl <- function(inner_trans_matrix){
  inner_trans_matrix |>
    tibble::as_tbl() |>
    mutate(from = paste0("CS",row_number())) |>
    pivot_longer(cols = contains("CS"),names_to="to")
}


#' takes an Inner Transition Matrix and which inner transition to rescale by scalar_change, for example
#' a 10% reduction would be scalar_change = 0.9
#' @param inner_trans_matrix matrix input
#' @param from_cs integer. The Core Segment to change from. Equivalent to *row* of inner_trans_matrix.
#' @param to_cs integer. The Core Segment to change to. Equivalent to *column* of inner_trans_matrix.
#' @param scalar_change numeric, the amount to scale the transition from from_cs to to_cs
#' @method either "take from no change" or "take proportionally from other changes" - how to get the
#' proportions to add back to 1
scalar_from_to <- function(inner_trans_matrix,
                           from_cs,
                           to_cs,
                           scalar_change,
                           method = "take from no change"){

  if(!(method %in% c("take from no change","take proportionally from other changes"))){
    stop("haven't implemented that method")
  }

  if(method == "take proportionally from other changes"){
    # this is from linear algebra - what should the other proportion values scale by to
    # make it still add to 1
    scalar = (1-(scalar_change)*inner_trans_matrix[from_cs, to_cs]) / (1-inner_trans_matrix[from_cs, to_cs])
    # the row to multiply by
    change_row <- matrix(data = rep(scalar,5),nrow=1,ncol=5)
    # don't forget the change we've been asked for
    change_row[to_cs] = scalar_change
  }

  if(method == "take from no change"){
    change_row <- matrix(data = rep(1,5),nrow=1,ncol=5)
    amount_changed <- (1-scalar_change) * inner_trans_matrix[from_cs, to_cs]
    # rescale the desired change
    change_row[to_cs] = scalar_change
    # put the amount changed into the from_cs location
    change_row[from_cs] = amount_changed/inner_trans_matrix[from_cs, from_cs] + 1
  }

  # apply the change to get new transition matrix
  new_inner_trans_matrix <- inner_trans_matrix
  new_inner_trans_matrix[from_cs,] <- new_inner_trans_matrix[from_cs,] * change_row

  # sanity check - scalar_change can sometimes go too far
  if(length(unique(round(rowSums(new_inner_trans_matrix),5)))!=1){
    stop(paste0("one of the rowSums is wrong",
                paste0(rowSums(new_inner_trans_matrix),collapse=", ")))
  }

  return(new_inner_trans_matrix)
}
