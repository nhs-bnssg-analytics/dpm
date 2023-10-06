
#' takes a starter inner_transition_matrix and creates a list of future matrices based on their scalar changee
#' @param inner_trans_matrix the original inner transition matrix
#' @param from_cs the core segment starting from
#' @param to_cs the core segment transitioning to
#' @param change_prop how much to change the value - between (-1,1). A value
#' of -1 is totally go to zero, a value of 1 is totally go to 1
#' @param over_n_iterations over how many discrete time steps for the change to happen over. Defaults to 1 ie immediately.
#' If scalar_change = 0.5 then
#' @param total_time discrete time steps - ie how many transition matrices to produce + 1
#' @param method either "take from no change" or "take proportionally from other changes" - how to get the
#' proportions to add back to 1
#' @return the set of inner transitions
#' @export
changing_inner_trans_matrix <- function(inner_trans_matrix_list,
                                        from_cs,
                                        to_cs,
                                        scalar_change,
                                        over_n_iterations = 1,
                                        total_time = 20,
                                        method = "take from no change"){

  inner_trans_matrix_list <- check_inner_trans(inner_trans_matrix_list,
                                               total_time)

  if(over_n_iterations==1){
    # this function is just a wrapper for scalar_from_to with varying over_n_iterations
    # If over_n_iterations = 1 then it's a straight call of the data
    inner_trans_matrix_list[[1]] <- scalar_from_to(inner_trans_matrix,
                                                   from_cs,
                                                   to_cs,
                                                   scalar_change,
                                                   method)
  } else {
    # more complicated - have to change the amount scalar_change is each time to make
    # the overall scalar_change correct after over_n_iterations. Took some simple, but
    # annoying, linear algebra.

    # each time will change by how much?
    linear_change <- (inner_trans_matrix_list[[1]][from_cs,to_cs] * (1-scalar_change))/over_n_iterations
    # iterate through
    for(i in 2:(over_n_iterations+1)){
      scalar_change_i <- 1 - linear_change / inner_trans_matrix_list[[i-1]][from_cs, to_cs]
      # only change the row of the CS we are going from
      inner_trans_matrix_list[[i]][from_cs,] <- scalar_from_to(inner_trans_matrix_list[[i-1]],
                                                               from_cs,
                                                               to_cs,
                                                               scalar_change_i,
                                                               method)[from_cs,]
    }
  }
  # make the end transition matrix permeate to the end of the time period
  for(i in (over_n_iterations+1):total_time){inner_trans_matrix_list[[i]] <- inner_trans_matrix_list[[over_n_iterations+1]]}

  return(inner_trans_matrix_list)
}

#' takes an Inner Transition Matrix and which inner transition to rescale by scalar_change, for example
#' a 10% reduction would be scalar_change = 0.9
#' @param inner_trans_matrix matrix input
#' @param from_cs integer. The Core Segment to change from. Equivalent to *row* of inner_trans_matrix.
#' @param to_cs integer. The Core Segment to change to. Equivalent to *column* of inner_trans_matrix.
#' @param scalar_change numeric, the amount to scale the transition from from_cs to to_cs
#' @param method either "take from no change" or "take proportionally from other changes" - how to get the
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

#' check the inner transition matrix is valid, and if it is only a matrix convert it
#' into a list of size total_time where every element is the same matrix
#' @param inner_trans_matrix_list the inner transition matrix list, or perhaps matrix
#' @param total_time the time window
#' @param warnings T/F - whether to output warning messages
#' @export
check_inner_trans <- function(inner_trans_matrix_list,
                              total_time,
                              warnings=TRUE){

  # if it is a matrix, convert to a list and warn the user
  if(is.matrix(inner_trans_matrix_list)){
    if(warnings){warning("assuming inner trans matrix is constant as only one given")}
    inner_trans_matrix <- inner_trans_matrix_list
    inner_trans_matrix_list <- list()
    for(i in 1:total_time){inner_trans_matrix_list[[i]] <- inner_trans_matrix}
  }
  # if not list or matrix then no dice
  if(!is.list(inner_trans_matrix_list)){
    stop("input must be a list or matrix")}
  # check validity of every element
  for(i in 1:total_time){valid_inner_trans_matrix(inner_trans_matrix_list[[i]])}
  return(inner_trans_matrix_list)
}


#' checks whether the inner transition matrix is valid - outputs errors or
#' warnings if not
#' @param inner_trans_matrix an inner transition matrix
#' @export
valid_inner_trans_matrix <- function(inner_trans_matrix){
  # is it the right class
  if(!is.matrix(inner_trans_matrix)){
    stop("input is not a matrix")}
  # check it's a square matrix
  if(dim(inner_trans_matrix)[1] != dim(inner_trans_matrix)[2]){
    stop("matrix needs to be square")}
  # check its 5x5 but only warn, not error
  if(dim(inner_trans_matrix)[1] != 5){
    warning(paste0(
      "Usually a 5x5 matrix is inputted, this is a ",
      dim(inner_trans_matrix)[1], " by ", dim(inner_trans_matrix)[2]))}
  # check its numeric inside
  if(!is.numeric(inner_trans_matrix)){
    stop("matrix inputs need to be numeric")
  }
  # check RowSums are to 1
  row_sums <- rowSums(inner_trans_matrix) %>% round(10)
  if(!all(row_sums == 1)){
    col_sums <- colSums(inner_trans_matrix) %>% round(10)
    if(all(col_sums==1)){
      stop("you might have inputted matrix wrong way round - try t() to transpose")
    } else {
    stop("row sums of inner_trans_matrix doesn't equal 1")}
  }
}


