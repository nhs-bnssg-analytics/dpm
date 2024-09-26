
#' Add age group column to a data frame
#' using ONS standards
#' @param my_tbl a tibble with the column age
#' @param age_column_name a character vector of the name of the age column
#' @export
add_age_group_column <- function(my_tbl,age_column_name="age"){

  if(!(age_column_name %in% names(my_tbl))){
    stop("age column not found")
  }
  if("a" %in% names(my_tbl) | "aa" %in% names(my_tbl)){
    stop("column 'a' or 'aa' would be overwritten as function is executed")
  }

  group_name <- paste0(age_column_name, "_group")

  my_tbl <- my_tbl %>%
    # column a is standin for age, aa is standin for age_group
    rename(a := {{age_column_name}}) %>%
    mutate(aa = case_when(
      is.na(a) ~ as.character(NA),
      a >= 90 ~ "90+",
      TRUE ~ paste0(
        as.character(floor(a/5)*5),"-",
        as.character(floor(a/5)*5+4)))) %>%
    mutate(aa = ifelse(aa %in% c("0-4","5-9"),
                       paste0(0,substr(aa,1,1),"-",0,substr(aa,3,3)),
                       aa)) %>%
    mutate(aa=factor(aa)) %>%
    rename({{age_column_name}} := a,
           {{group_name}} := aa)

  return(my_tbl)
}

#' import dplyr
# Function to convert age bands to decade age bands
#' @export
convert_to_decade <- function(df, age_column_name) {

  df <- df %>%
    rename(a := {{age_column_name}}) %>%
    mutate(a := case_when(
      is.na(a) ~ a,
      # DPM parlance
      a %in% c("15-19","20-24","25-29") ~ "17-29",
      a %in% c("30-34","35-39") ~ "30-39",
      a %in% c("40-44","45-49") ~ "40-49",
      a %in% c("50-54","55-59") ~ "50-59",
      a %in% c("60-64","65-69") ~ "60-69",
      a %in% c("70-74","75-79") ~ "70-79",
      a %in% c("80-84","85-89") ~ "80-89",
      a == "90+" ~ "90+",
      TRUE ~ "NO AGE MATCH"
    )) %>%
    rename({{age_column_name}} := a)
  return(df)
}

get_valid_transitions_tbl <- function(){

  factor_levels <-
    c("17-29__CS1", "17-29__CS2", "17-29__CS3", "17-29__CS4", "17-29__CS5",
      "30-39__CS1", "30-39__CS2", "30-39__CS3", "30-39__CS4", "30-39__CS5",
      "40-49__CS1", "40-49__CS2", "40-49__CS3", "40-49__CS4", "40-49__CS5",
      "50-59__CS1", "50-59__CS2", "50-59__CS3", "50-59__CS4", "50-59__CS5",
      "60-69__CS1", "60-69__CS2", "60-69__CS3", "60-69__CS4", "60-69__CS5",
      "70-79__CS1", "70-79__CS2", "70-79__CS3", "70-79__CS4", "70-79__CS5",
      "80-89__CS1", "80-89__CS2", "80-89__CS3", "80-89__CS4", "80-89__CS5",
      "90+__CS1"  , "90+__CS2"  , "90+__CS3"  , "90+__CS4"  , "90+__CS5"  ,
      "NA__Died")

  # Generating the list of accepted moves between age_seg_state options
  valid_lookup_tbl <- structure(list(
    age_cs_state_prev = structure(
      c(1L, 1L, 1L, 1L,
        1L, 1L, 2L, 2L, 2L, 2L, 2L, 2L, 3L, 3L, 3L, 3L, 3L, 3L, 4L, 4L,
        4L, 4L, 4L, 4L, 5L, 5L, 5L, 5L, 5L, 5L, 1L, 1L, 1L, 1L, 1L, 2L,
        2L, 2L, 2L, 2L, 3L, 3L, 3L, 3L, 3L, 4L, 4L, 4L, 4L, 4L, 5L, 5L,
        5L, 5L, 5L, 6L, 6L, 6L, 6L, 6L, 6L, 7L, 7L, 7L, 7L, 7L, 7L, 8L,
        8L, 8L, 8L, 8L, 8L, 9L, 9L, 9L, 9L, 9L, 9L, 10L, 10L, 10L, 10L,
        10L, 10L, 6L, 6L, 6L, 6L, 6L, 7L, 7L, 7L, 7L, 7L, 8L, 8L, 8L,
        8L, 8L, 9L, 9L, 9L, 9L, 9L, 10L, 10L, 10L, 10L, 10L, 11L, 11L,
        11L, 11L, 11L, 11L, 12L, 12L, 12L, 12L, 12L, 12L, 13L, 13L, 13L,
        13L, 13L, 13L, 14L, 14L, 14L, 14L, 14L, 14L, 15L, 15L, 15L, 15L,
        15L, 15L, 11L, 11L, 11L, 11L, 11L, 12L, 12L, 12L, 12L, 12L, 13L,
        13L, 13L, 13L, 13L, 14L, 14L, 14L, 14L, 14L, 15L, 15L, 15L, 15L,
        15L, 16L, 16L, 16L, 16L, 16L, 16L, 17L, 17L, 17L, 17L, 17L, 17L,
        18L, 18L, 18L, 18L, 18L, 18L, 19L, 19L, 19L, 19L, 19L, 19L, 20L,
        20L, 20L, 20L, 20L, 20L, 16L, 16L, 16L, 16L, 16L, 17L, 17L, 17L,
        17L, 17L, 18L, 18L, 18L, 18L, 18L, 19L, 19L, 19L, 19L, 19L, 20L,
        20L, 20L, 20L, 20L, 21L, 21L, 21L, 21L, 21L, 21L, 22L, 22L, 22L,
        22L, 22L, 22L, 23L, 23L, 23L, 23L, 23L, 23L, 24L, 24L, 24L, 24L,
        24L, 24L, 25L, 25L, 25L, 25L, 25L, 25L, 21L, 21L, 21L, 21L, 21L,
        22L, 22L, 22L, 22L, 22L, 23L, 23L, 23L, 23L, 23L, 24L, 24L, 24L,
        24L, 24L, 25L, 25L, 25L, 25L, 25L, 26L, 26L, 26L, 26L, 26L, 26L,
        27L, 27L, 27L, 27L, 27L, 27L, 28L, 28L, 28L, 28L, 28L, 28L, 29L,
        29L, 29L, 29L, 29L, 29L, 30L, 30L, 30L, 30L, 30L, 30L, 26L, 26L,
        26L, 26L, 26L, 27L, 27L, 27L, 27L, 27L, 28L, 28L, 28L, 28L, 28L,
        29L, 29L, 29L, 29L, 29L, 30L, 30L, 30L, 30L, 30L, 31L, 31L, 31L,
        31L, 31L, 31L, 32L, 32L, 32L, 32L, 32L, 32L, 33L, 33L, 33L, 33L,
        33L, 33L, 34L, 34L, 34L, 34L, 34L, 34L, 35L, 35L, 35L, 35L, 35L,
        35L, 31L, 31L, 31L, 31L, 31L, 32L, 32L, 32L, 32L, 32L, 33L, 33L,
        33L, 33L, 33L, 34L, 34L, 34L, 34L, 34L, 35L, 35L, 35L, 35L, 35L,
        36L, 36L, 36L, 36L, 36L, 36L, 37L, 37L, 37L, 37L, 37L, 37L, 38L,
        38L, 38L, 38L, 38L, 38L, 39L, 39L, 39L, 39L, 39L, 39L, 40L, 40L,
        40L, 40L, 40L, 40L),
      levels = factor_levels, class = c("ordered","factor")),
    age_cs_state_orig = structure(
      c(1L,
        2L, 3L, 4L, 5L, 41L, 1L, 2L, 3L, 4L, 5L, 41L, 1L, 2L, 3L, 4L,
        5L, 41L, 1L, 2L, 3L, 4L, 5L, 41L, 1L, 2L, 3L, 4L, 5L, 41L, 6L,
        7L, 8L, 9L, 10L, 6L, 7L, 8L, 9L, 10L, 6L, 7L, 8L, 9L, 10L, 6L,
        7L, 8L, 9L, 10L, 6L, 7L, 8L, 9L, 10L, 6L, 7L, 8L, 9L, 10L, 41L,
        6L, 7L, 8L, 9L, 10L, 41L, 6L, 7L, 8L, 9L, 10L, 41L, 6L, 7L, 8L,
        9L, 10L, 41L, 6L, 7L, 8L, 9L, 10L, 41L, 11L, 12L, 13L, 14L, 15L,
        11L, 12L, 13L, 14L, 15L, 11L, 12L, 13L, 14L, 15L, 11L, 12L, 13L,
        14L, 15L, 11L, 12L, 13L, 14L, 15L, 11L, 12L, 13L, 14L, 15L, 41L,
        11L, 12L, 13L, 14L, 15L, 41L, 11L, 12L, 13L, 14L, 15L, 41L, 11L,
        12L, 13L, 14L, 15L, 41L, 11L, 12L, 13L, 14L, 15L, 41L, 16L, 17L,
        18L, 19L, 20L, 16L, 17L, 18L, 19L, 20L, 16L, 17L, 18L, 19L, 20L,
        16L, 17L, 18L, 19L, 20L, 16L, 17L, 18L, 19L, 20L, 16L, 17L, 18L,
        19L, 20L, 41L, 16L, 17L, 18L, 19L, 20L, 41L, 16L, 17L, 18L, 19L,
        20L, 41L, 16L, 17L, 18L, 19L, 20L, 41L, 16L, 17L, 18L, 19L, 20L,
        41L, 21L, 22L, 23L, 24L, 25L, 21L, 22L, 23L, 24L, 25L, 21L, 22L,
        23L, 24L, 25L, 21L, 22L, 23L, 24L, 25L, 21L, 22L, 23L, 24L, 25L,
        21L, 22L, 23L, 24L, 25L, 41L, 21L, 22L, 23L, 24L, 25L, 41L, 21L,
        22L, 23L, 24L, 25L, 41L, 21L, 22L, 23L, 24L, 25L, 41L, 21L, 22L,
        23L, 24L, 25L, 41L, 26L, 27L, 28L, 29L, 30L, 26L, 27L, 28L, 29L,
        30L, 26L, 27L, 28L, 29L, 30L, 26L, 27L, 28L, 29L, 30L, 26L, 27L,
        28L, 29L, 30L, 26L, 27L, 28L, 29L, 30L, 41L, 26L, 27L, 28L, 29L,
        30L, 41L, 26L, 27L, 28L, 29L, 30L, 41L, 26L, 27L, 28L, 29L, 30L,
        41L, 26L, 27L, 28L, 29L, 30L, 41L, 31L, 32L, 33L, 34L, 35L, 31L,
        32L, 33L, 34L, 35L, 31L, 32L, 33L, 34L, 35L, 31L, 32L, 33L, 34L,
        35L, 31L, 32L, 33L, 34L, 35L, 31L, 32L, 33L, 34L, 35L, 41L, 31L,
        32L, 33L, 34L, 35L, 41L, 31L, 32L, 33L, 34L, 35L, 41L, 31L, 32L,
        33L, 34L, 35L, 41L, 31L, 32L, 33L, 34L, 35L, 41L, 36L, 37L, 38L,
        39L, 40L, 36L, 37L, 38L, 39L, 40L, 36L, 37L, 38L, 39L, 40L, 36L,
        37L, 38L, 39L, 40L, 36L, 37L, 38L, 39L, 40L, 36L, 37L, 38L, 39L,
        40L, 41L, 36L, 37L, 38L, 39L, 40L, 41L, 36L, 37L, 38L, 39L, 40L,
        41L, 36L, 37L, 38L, 39L, 40L, 41L, 36L, 37L, 38L, 39L, 40L, 41L),
      levels = factor_levels, class = c("ordered","factor"))),
    row.names = c(NA, -415L),
    class = c("tbl_df", "tbl", "data.frame")) |>
  mutate(valid_move = T)

return(valid_lookup_tbl)
}


#' add_age_cs_state_col
#' Add in the age_cs_state column to a data frame
#' @param my_df data frame
#' @export
add_age_cs_state_col <- function(my_df){

  # if there isn't age_cs_state  but there is two cols that
  # combine to make it, then add it
  if(!("age_cs_state" %in% names(my_df)) &
     "state_name" %in% names(my_df) &
     "age_group" %in% names(my_df)){
    my_df <- my_df |>
      mutate(age_cs_state = paste0(age_group,"__",state_name))
  }
  if(!("age_cs_state" %in% names(my_df))){
    stop("need cols age_cs_state or both age_group and state_name")
  }

  # make the new variable an ordinal factor variable
  valid_transitions_tbl_levels <- get_valid_transitions_tbl() %>%
    pull(age_cs_state_prev) %>%
    levels()

  my_df <- my_df |>
    mutate(age_cs_state =
             ordered(age_cs_state,
                     levels=valid_transitions_tbl_levels))

  return(my_df)
}
