
#' connection to the SQL server
#' @export
#' @import DBI
#' @import odbc
get_sql_con <- function(){
  sql_con <- DBI::dbConnect(odbc::odbc(),
                            Driver = "SQL Server",
                            Server = "Xsw-00-ash01",
                            Database = "MODELLING_SQL_AREA",
                            Trusted_Connection = "True",
                            timeout = 120)
  return(sql_con)
}

# below are defined as functions so can be repopulated or changed in this place
# only for that to propogate through the code base. Returns are still connections
# using dbplyr, they are not in the R environment
get_sql_table_source_new_cambridge_score <- function(sql_con){
  dplyr::tbl(sql_con,
      dbplyr::in_schema(
        sql("MODELLING_SQL_AREA"),
        sql("dbo.New_Cambridge_Score")))
}
get_sql_table_source_swd_activity <- function(sql_con){
  dplyr::tbl(sql_con,
      dbplyr::in_schema(
        sql("MODELLING_SQL_AREA"),
        sql("dbo.swd_activity")))
}
get_sql_table_source_primary_care_attr <- function(sql_con){
  dplyr::tbl(sql_con,
      dbplyr::in_schema(
        sql("MODELLING_SQL_AREA"),
        sql("dbo.primary_care_attributes")))
}
get_sql_table_source_deaths <- function(sql_con){
  dplyr::tbl(sql_con,
      dbplyr::in_schema(
        sql("ABI"),
        sql("Civil_Registration.Mortality")))
}
get_sql_table_source_pds <- function(sql_con){
  dplyr::tbl(sql_con,
      dbplyr::in_schema(
        sql("ABI"),
        sql("PDS.Patient")))
}
get_sql_table_source_clean_nhs_numbers <- function(sql_con){
  dplyr::tbl(sql_con,
      dbplyr::in_schema(
        sql("MODELLING_SQL_AREA"),
        sql("dbo.LS_Patient_NHS_Numbers_Clean")))
}
get_sql_table_source_population <- function(sql_con){
  dplyr::tbl(sql_con,
      dbplyr::in_schema(
        sql("Analyst_SQL_Area"),
        sql("dbo.tbl_BNSSG_Datasets_GP_Pop_SingleYear")))
}
