

sample_next_transition <- function(pop_vec, Q){
  rate_matrix <- pop_vec * Q
  r <- nrow(rate_matrix)
  c <- ncol(rate_matrix)
  # prepopulate with Inf so is not the min ever
  sample_matrix <- matrix(data = rep(Inf, r*c),
                          nrow = r,
                          ncol = c)

  # go through each element
  for(i in 1:r){
    for(j in 1:c){
      if(!is.na(rate_matrix[i,j])){
        if(i!=j){
          if(rate_matrix[i,j] != 0){
            sample_matrix[i,j] <- rexp(n=1,rate=rate_matrix[i,j])
          }
        }
      }
    }
  }

  # find the min
  min_time <- min(sample_matrix,na.rm=T)
  min_time_loc <- which(sample_matrix == min_time, arr.ind=T)

  change_vec <- rep(0, r)
  names(change_vec) <- names(pop_vec)
  change_vec[min_time_loc[1]] <- -1
  if(min_time_loc[2]!=41){
    # can only do this if it's not a death that's happened
    change_vec[min_time_loc[2]] <- 1
  }

  return(list(time = min_time, change_vec = change_vec))

}
