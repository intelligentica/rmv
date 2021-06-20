#' Create RSQLite VMS Data Base
#'
#' @param file the .vms file
#' @param add if TRUE append to existent data, FALSE (default)
#'
#' @return Doesn't return a value
#' @description creates an SQLite database for VMS data. It creates also additional tables
#' that could be used for other purposes of analyzing and cleaning the data.
#'
#' @export
#'
vms_create_db <- function(vms_file, add = FALSE){

  filename = get_file_name(vms_file)
  filepath = get_file_path(vms_file)
  path = paste0(filepath,"/",filename,"_vms.sqlite")

  conn = dbConnect(RSQLite::SQLite(), path)
  data = read.table(vms_file,header = T,sep = ";",dec = ".")

  if(add==FALSE){
    dbExecute(conn, "drop table if exists ping")
    dbWriteTable(conn, name="ping", data, append=add, row.names=F)
    cat("\n - ping table created successfully!\n")
  }else{
    dbWriteTable(conn, name="ping", data, append=add, row.names=F)
    cat("\n - ping table created successfully!\n")
  }

  dbExecute(conn, "drop table if exists warn")
  dbExecute(conn,"CREATE TABLE warn(p_id INT, W_DUPL INT, W_HARB INT,W_LAND INT, W_COHE INT)")
  cat("\n - warn table created successfully!\n")

  dbExecute(conn, "drop table if exists track")
  dbExecute(conn,"CREATE TABLE track(I_NCEE VARCHAR(30), LAT REAL, LON REAL, DATE REAL, SPE REAL,
            HEA REAL, W_HARB INT, T_NUM INT, P_ID INT)")
  cat("\n - track table created successfully!\n")

  dbExecute(conn, "drop table if exists intrp")
  dbExecute(conn,"CREATE TABLE intrp(I_NCEE VARCHAR(30), LAT REAL, LON REAL, DATE REAL, SPE REAL,
          HEA REAL, W_HARB INT, T_NUM INT, P_ID INT, P_INT INT, T_ID INT)")
  cat("\n - intrp table created successfully!\n")

  dbExecute(conn, "drop table if exists p_depth")
  dbExecute(conn,"CREATE TABLE p_depth(i_id INT, vess_id INT, DEPTH REAL)")
  cat("\n - p_depth table created successfully!\n")

  dbExecute(conn, "drop table if exists p_area")
  dbExecute(conn,"CREATE TABLE p_area(vess_id VARCHAR(30), t_num INT, AREA INT)")
  cat("\n - p_area table created successfully!\n")

  dbExecute(conn, "drop table if exists vms_lb")
  dbExecute(conn,"CREATE TABLE vms_lb(vessel VARCHAR(30), track INT, logbook INT, log_id INT, met_des CHAR)")
  cat("\n - vms_lb table created successfully!\n")

  dbExecute(conn, "drop table if exists p_fish")
  dbExecute(conn,"CREATE TABLE p_fish(i_id INT, F_SPE INT, F_DEP INT, F_DIS INT, FISH INT)")
  cat("\n - p_fish table created successfully!\n")

  cat("\n\n   ---   VMS DataBase Deploy Completed!   ---\n\n")
  dbDisconnect(conn = conn)
}
