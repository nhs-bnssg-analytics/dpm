
#' Add age group column to a data frame
#' using ONS standards
#' @param my_tbl a tibble with the column age
#' @param age_column_name a character vector of the name of the age column
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
                       paste0(0,substr(aa),0,substr(aa,3,3)),
                       aa)) %>%
    mutate(aa=factor(aa)) %>%
    rename({{age_column_name}} := a,
           {{group_name}} := aa)

  return(my_tbl)
}

#' import dplyr
# Function to convert age bands to decade age bands
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
