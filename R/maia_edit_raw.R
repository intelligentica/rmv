#' Format MAIA csv File into Rmaiavms Formmat
#'
#' @param file path to the landings data
#' @param vms_db_path path to the vms database
#' @importFrom DBI dbConnect dbGetQuery dbExecute dbDisconnect dbWriteTable
#' @importFrom data.table rbindlist
#' @importFrom sp spDistsN1 over SpatialPoints spDists spTransform CRS coordinates
#' @importFrom RSQLite SQLite
#' @importFrom chron chron
#' @importFrom maps map
#' @importFrom grDevices extendrange
#' @importFrom marmap getNOAA.bathy
#' @importFrom fields Tps Krig
#' @importFrom graphics abline points
#' @importFrom stats predict
#' @importFrom utils globalVariables
#' @importFrom dplyr filter mutate arrange mutate
#' @importFrom tidyr separate
#' @return this function doesn't return a value
#' @export
#'
maia_edit_raw <- function(file, vms_db_path){

  conn = DBI::dbConnect(RSQLite::SQLite(),vms_db_path)
  tracks = DBI::dbGetQuery(conn, "select * from track order by DATE")

  incee = DBI::dbGetQuery(conn, "select distinct I_NCEE from track")[,1]

  raw_df_maia = read.csv(file)

  raw_df_maia = raw_df_maia %>%
    dplyr::filter(Matricule %in% incee)

  numlines <- nrow(raw_df_maia)

  if(nrow(tracks)>0){
    cat("\n\n   ---   Raw Logbooks Editing Started!   ---\n","\nProcessing ", numlines, " raw logbooks...\n",sep = "")
    logbook <- data.frame(
      "vessREF" = character(numlines),
      "s_utc" = numeric(numlines),
      "e_utc" = numeric(numlines),
      "specie" = character(numlines),
      "qty" = numeric(numlines),
      "gear" = numeric(numlines),
      "metier" = numeric(numlines)
    )

    logbook["vessREF"] = raw_df_maia["Matricule"]
    logbook["specie"] = raw_df_maia["Espece"]
    logbook["qty"] = raw_df_maia["Poids"]
    logbook["metier"] = 0
    logbook["gear"] = 0

    raw_df_maia <- raw_df_maia %>%
      dplyr::mutate(Date = as.Date(Date)) %>%
      dplyr::arrange(Date)

    tracks$FULL_DATIM = fromChronToDate(tracks$DATE)
    tracks = tracks %>%
      tidyr::separate(col = FULL_DATIM,into = c("FULL_DATE","FULL_TIME"),sep=" ")

    tracks = tracks %>%
      dplyr::mutate(FULL_DATE = as.Date(FULL_DATE,format = "%m-%d-%Y"))

    timenow = Sys.time()
    for(i in 1:numlines){

      vessid = raw_df_maia$Matricule[i]
      tracks_i = tracks %>%
        dplyr::filter(I_NCEE==vessid) %>%
        dplyr::arrange(DATE)

      max_t_num = max(tracks_i[,"T_NUM"])
      vms_i = which.min(abs(tracks_i$FULL_DATE - raw_df_maia$Date[i]))

      currentT_NUM = tracks_i[vms_i, "T_NUM"]

      dfCurrentT_NUM = tracks_i %>%
        dplyr::filter(T_NUM == currentT_NUM) %>%
        dplyr::arrange(DATE)

      if(currentT_NUM < max_t_num){
        nextT_NUM = currentT_NUM + 1
        dfNextT_NUM = tracks_i %>%
          dplyr::filter(T_NUM == nextT_NUM) %>%
          dplyr::arrange(DATE)
      }
      if(currentT_NUM > 1){
        previousT_NUM = currentT_NUM - 1
        dfPreviousT_NUM = tracks_i %>%
          dplyr::filter(T_NUM == previousT_NUM) %>%
          dplyr::arrange(DATE)
      }

      if((currentT_NUM>1)&(currentT_NUM < max_t_num)){
        ## Check if fish belong to current next track
        if(dfCurrentT_NUM[nrow(dfCurrentT_NUM),"FULL_DATE"]>raw_df_maia$Date[i]){

          logbook$s_utc[i] <- dfPreviousT_NUM[1,"DATE"]
          logbook$e_utc[i] <- dfPreviousT_NUM[nrow(dfPreviousT_NUM),"DATE"]

        }else{
          if(dfNextT_NUM[nrow(dfNextT_NUM),"FULL_DATE"]>raw_df_maia$Date[i]){

            logbook$s_utc[i] <- dfCurrentT_NUM[1,"DATE"]
            logbook$e_utc[i] <- dfCurrentT_NUM[nrow(dfCurrentT_NUM),"DATE"]

          }else{

            logbook$s_utc[i] <- dfNextT_NUM[1,"DATE"]
            logbook$e_utc[i] <- dfNextT_NUM[nrow(dfNextT_NUM),"DATE"]

          }
        }
      }

      if(currentT_NUM ==1){
        if(dfNextT_NUM[nrow(dfNextT_NUM),"FULL_DATE"]>raw_df_maia$Date[i]){

          logbook$s_utc[i] <- dfCurrentT_NUM[1,"DATE"]
          logbook$e_utc[i] <- dfCurrentT_NUM[nrow(dfCurrentT_NUM),"DATE"]

        }else{

          logbook$s_utc[i] <- dfNextT_NUM[1,"DATE"]
          logbook$e_utc[i] <- dfNextT_NUM[nrow(dfNextT_NUM),"DATE"]

        }
      }

      if(currentT_NUM ==max_t_num){
        if(dfCurrentT_NUM[nrow(dfCurrentT_NUM),"FULL_DATE"]>raw_df_maia$Date[i]){

          logbook$s_utc[i] <- dfPreviousT_NUM[1,"DATE"]
          logbook$e_utc[i] <- dfPreviousT_NUM[nrow(dfPreviousT_NUM),"DATE"]

        }else{

          logbook$s_utc[i] <- dfCurrentT_NUM[1,"DATE"]
          logbook$e_utc[i] <- dfCurrentT_NUM[nrow(dfCurrentT_NUM),"DATE"]

        }
      }
      if(i%%1000==0){
        cat(i," out of ",numlines,"\n")
      }
    }
    cat("Time Spent : ",Sys.time() - timenow, "\n")
    ## SAVE THE RESULTED FILE
    cat("\n   ---   Saving Formatted File   ---\n")
    filename = get_file_name(file)
    filepath = get_file_path(file)
    path = paste0(filepath,"/",filename,".logbook")
    write.table(logbook, file = path,sep = ";", dec = ".", quote = FALSE, row.names = FALSE)
    cat("\n   ---   Done!   ---\n")
  }else{
    cat("You should calculate the tracks first.")
  }
  DBI::dbDisconnect(conn = conn)
}
