#' Main DPM function under continuous time, which is what the _ct stands for in
#' the function name.
#' @import dplyr
#' @export
run_dpm_ct <- function(
    initial_population,
    inner_trans_matrix,
    monthly_entrants_exits,
    start_time = 0,
    seed = 1
){
  set.seed(seed) # for consistent randomness

  if(1==1){
    # This should be brought outside of this function and standardised somehow.
    # It gets you to inner_trans_rate_matrix
    warning("using predefined transition rate matrix!")
    inner_trans_rate_tbl <-
      readr::read_csv(paste0(
        "C:/GitHub/Core-Segments-Over-Time/outputs/",
        "2024-05-14_transition_rate_matrix_using2020-12-01to2024-02-01.csv"),
        show_col_types=F) |>
      mutate(age_cs_state_prev = ordered(
        age_seg_state_prev,
        levels=levels(initial_population$age_cs_state)),
        age_cs_state_orig = ordered(
          age_seg_state_orig,
          levels=levels(initial_population$age_cs_state))) |>
      select(age_cs_state_prev,
             age_cs_state_orig,
             transition_rate_estimate) |>
      arrange(age_cs_state_orig, age_cs_state_prev)

    inner_trans_rate_matrix <- inner_trans_rate_tbl |>
      pivot_wider(names_from=age_cs_state_orig,
                  values_from=transition_rate_estimate) |>
      select(-age_cs_state_prev) |>
      as.matrix()
    rownames(inner_trans_rate_matrix) <- inner_trans_rate_tbl$age_cs_state_prev |>
      unique()
  }


  if(is.data.frame(initial_population)){
    # convert to vector - order is important
    initial_population <- arrange(initial_population,age_cs_state)
    initial_pop_vec <- initial_population$initial_pop
    names(initial_pop_vec) <- initial_population$age_cs_state
  }
  if(is.vector(initial_population)){initial_pop_vec = initial_population}


  initial_pop_vec
  inner_trans_rate_matrix
  monthly_entrants_exits

  max_time <- monthly_entrants_exits %>% pull(month) %>% max() + 1

  # parameter setting
  pop_vec <- initial_pop_vec
  vec_hist <- matrix(c(start_time,pop_vec),nrow=1)
  colnames(vec_hist) <- c("time",names(pop_vec))
  TIME <- start_time
  prev_time <- TIME
  save_folder <- here::here("data",paste0("dpm_ct_seed",seed))
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
              file.path(save_folder,paste0("vec_hist",TIME,".rds")))
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
