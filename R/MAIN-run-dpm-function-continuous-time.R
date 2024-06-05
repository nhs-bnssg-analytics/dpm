#' Main DPM function under continuous time, which is what the _ct stands for in
#' the function name.
#' @import dplyr
#' @export
run_dpm_ct <- function(
    initial_population,
    inner_trans_rate_matrix,
    monthly_entrants_exits,
    start_time = 0,
    seed = 1,
    start_month
){
  set.seed(seed) # for consistent randomness

  if(is.data.frame(initial_population)){
    # convert to vector - order is important
    initial_population <- arrange(initial_population,age_cs_state)
    initial_pop_vec <- initial_population$initial_pop
    names(initial_pop_vec) <- initial_population$age_cs_state
  }
  if(is.vector(initial_population)){initial_pop_vec = initial_population}


  max_time <- monthly_entrants_exits %>% pull(month) %>% max() + 1

  # Small function that takes a character string and returns as XXX.XXX
  # eg
  # pad_me("0.2") = "000.200"
  # pad_me("234.5") = "234.500"
  # pad_me("34.24") = "034.240"
  pad_me <- function(x) {
    parts <- strsplit(x, "\\.")[[1]]
    left <- sprintf("%03d", as.integer(parts[1]))
    right <- sprintf("%-03s", ifelse(length(parts) > 1, parts[2], ""))
    right <- substr(paste0(right, "000"), 1, 3)
    right <- stringr::str_replace_all(right," ", "0")
    paste0(left, ".", right)
  }

  # parameter setting
  pop_vec <- initial_pop_vec
  vec_hist <- matrix(c(start_time,pop_vec),nrow=1)
  colnames(vec_hist) <- c("time",names(pop_vec))
  TIME <- start_time
  prev_time <- TIME
  save_folder <- here::here("data",paste0("dpm_ct_",start_month, "seed",seed))
  if (!file.exists(save_folder)) {
    dir.create(save_folder, recursive = TRUE)
  }  # We'll save in roughly day-level outputs to prevent data frames getting mahoosive.
  # Note that the units is months for time here, so a day is roughly 0.033
  save_interval <- 12/365
  next_save <- start_time + save_interval

  while (TIME < max_time){
    # first check we don't need to save
    if(TIME >= next_save){
      saveRDS(vec_hist,
              file.path(save_folder,paste0("vec_hist",pad_me(as.character(round(TIME,3))),"r.rds")))
      print(paste0("saved ",nrow(vec_hist), " row at ",TIME))
      # wipe the history as it's saved
      vec_hist <- vec_hist[0,]
      # work out next save point
      next_save <- next_save + save_interval
    }

    # then check we don't need to add in the monthly shiz.
    if(floor(TIME) != floor(prev_time)){
      entrants_exits <- monthly_entrants_exits %>%
        filter(month == floor(TIME))
      births_vec <- entrants_exits %>%
        filter(event == "births") %>%
        arrange(age_cs_state) %>%
        pull(value)
      immigrations_vec <- entrants_exits %>%
        filter(event == "immigrations") %>%
        arrange(age_cs_state) %>%
        pull(value)
      emigrations_vec <- entrants_exits %>%
        filter(event == "emigrations") %>%
        arrange(age_cs_state) %>%
        pull(value)

      pop_vec <- pop_vec + births_vec + immigrations_vec - emigrations_vec

      vec_hist <- rbind(vec_hist, c(floor(TIME),births_vec))
      vec_hist <- rbind(vec_hist, c(floor(TIME),immigrations_vec))
      vec_hist <- rbind(vec_hist, c(floor(TIME),-emigrations_vec))
    }

    # then can apply the inner transitions (including death)
    next_transition <- sample_next_transition(pop_vec, inner_trans_rate_matrix)
    prev_time <- TIME
    TIME <- TIME + next_transition$time
    pop_vec <- pop_vec + next_transition$change_vec

    vec_hist <- rbind(vec_hist, c(TIME, next_transition$change_vec))
  }

}
