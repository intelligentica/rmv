#' Clean the VMS data in the SQLite database
#'
#' @param vms_db_path path to the SQLite database
#' @param map_file_path shapefile of the land
#' @param harb_file_path shapefile of the positions of harbours
#' @param inHarbBuff distance from harbour that considered in harbour
#' @importFrom rgdal readOGR
#' @importFrom RSQLite SQLite
#' @importFrom DBI dbConnect dbGetQuery dbExecute dbWriteTable dbDisconnect
#' @importFrom sp spDistsN1 over SpatialPoints spDists
#' @importFrom chron chron
#' @importFrom magrittr %>%
#' @return Edit the database warn table
#' @export
#'
vms_clean_db <- function(vms_db_path , map_file_path, harb_file_path, inHarbBuff = 2){

  ## READING HARBOR SHAPEFILE
  harb_data <- rgdal::readOGR(harb_file_path,verbose = F)
  XCOORD <- harb_data@coords[,1]
  YCOORD <- harb_data@coords[,2]
  ## ADDING LAS PALMAS PORT
  XCOORD = c(XCOORD,-15.4167)
  YCOORD = c(YCOORD,28.1500)

  ## READING MAP SHAPEFILE
  themap_data <- rgdal::readOGR(map_file_path,verbose = F)

  ## CONNECT TO THE DATABASE
  conn = DBI::dbConnect(RSQLite::SQLite(), vms_db_path)

  ## IF THERE IS A TABLE CALLED WARN EXIST DROP IT AND CREATE A NEW ONE
  DBI::dbExecute(conn, "DROP TABLE IF EXISTS warn")
  DBI::dbExecute(conn, "CREATE TABLE warn(p_id INT, W_DUPL INT, W_HARB INT, W_LAND INT, W_COHE INT)")

  cat("\n\n   ---   Ping Cleaning Started   ---\n")

  ## SELECT DISTINCT VESSEL IDS
  incee = DBI::dbGetQuery(conn, 'select distinct I_NCEE from ping')

  ## LOOP OVER EACH VESSEL ID
  for (i in 1:nrow(incee)){

    cat("\nVessel: ", i, " of ", nrow(incee), spe = "")

    ## SELECT ROWID AND ALL COLUMNS OF THE CURRENT VESSEL
    vessel <- DBI::dbGetQuery(conn, paste0("select ROWID, * from ping where I_NCEE = '",incee[i,1],"' order by DATE"))
    numlines <- nrow(vessel)
    max_speed <- max(vessel$SPE,na.rm = TRUE)
    cat(" with ", numlines, " pings", sep = "")

    if (numlines == 0) {
      cat(" - Skipped!", sep = "")
      next
    }else{

      ann_data <- data.frame(
        "p_id" = numeric(numlines),
        "W_DUPL" = NA,
        "W_HARB" = NA,
        "W_LAND" = NA,
        "W_COHE" = integer(numlines)
      )
      ann_data["p_id"] <- vessel["rowid"]

      ## CHECK DUPLICATES
      dupl <- duplicated(vessel[, 2:7])

      ## CALCULATE THE MINIMAL DISTANCE AMONG ALL HARBORS
      hdist = lapply(1:numlines, function(j){
        sp::spDistsN1(pts = cbind(XCOORD, YCOORD), pt = as.matrix(c(vessel[j, "LON"], vessel[j, "LAT"])),longlat = TRUE) %>%
          min()
        }) %>%
        unlist

      ## CHECK POINTS ON LAND
      onland <- sp::over(sp::SpatialPoints(c(vessel["LON"], vessel["LAT"])), themap_data)
      land <- lapply(1:numlines, function(x){
        ifelse(hdist[x]>3 & !is.na(onland[x,1]),yes = 1, no = 0)
      }) %>%
        unlist

      ## CHECK POINTS IN HARBOR
      harb = lapply(1:numlines, function(x){
        ifelse(hdist[x] <= inHarbBuff,yes = 1, no = 0)
      }) %>%
        unlist

      cohe <- numeric(numlines)

      ## WE WILL GO PING BY PING AND SEE IF THE POINT IS IN HARBOR
      for (j in 1:numlines){
        # CHECK PINGS COHERENCE
        if (j == 1) {   # IF IT IS THE FIST DATA POINT FOR THIS VESSEL
          if (numlines > 1) {
            if (!dupl[j+1]) { # IF THE NEXT DATA POINT IS NOT A DUPLICATED ONE DO:

              ## CALCULATE THE DISTANCE TO THE NEXT PING FOR THIS VESSEL
              succdist <- sp::spDists(matrix(c(vessel[j, "LON"], vessel[j, "LAT"]), ncol = 2),
                                  matrix(c(vessel[j + 1, "LON"], vessel[j + 1, "LAT"]), ncol = 2),
                                  longlat = TRUE)

              ## CALCULATE THE LAG BETWEEN THE TWO SUCCESSIVE PINGS IN HOURS
              succlag <- (vessel[j + 1, "DATE"] - vessel[j, "DATE"]) * 24

              ## IF THE DIFFERENCE IN TIME BETWEEN THE TWO SUCCESSIVE PINGS IS ZERO,
              ## TURN IT INTO 0.01 TO AVOIND DIVIDING BY ZERO
              if (succlag == 0) {
                succlag <- 0.01
              }

              ## CALCULATE THE VELOCITY BASED ON DISTANCE AND TIME BETWEEN THE TWO PINGS
              succvel <- succdist / succlag
              ifelse(succvel < max_speed, cohe[j] <- 2, cohe[j] <- 0)

            }else{ ## IF THIS NEXT PING IS MARKED AS A DUPLICATE
              cohe[j] <- 2
            }
          }else{ ## IF (nrow(ann_data) == 1)
            cohe[j] <- 3
          }
          next
        }

        ## IF WE REACH THE LAST PING FOR THIS VESSEL
        if (j == numlines) {
          if (!dupl[j]) { ## IF IT IS NOT A DUPLICATE

            ## CALCULATE THE DISTANCE TO THE PREVIOUS PING
            predist <- sp::spDists(matrix(c(vessel[j, "LON"], vessel[j, "LAT"]), ncol = 2),
                               matrix(c(vessel[j - 1, "LON"], vessel[j - 1, "LAT"]), ncol = 2),
                               longlat = TRUE)

            ## CALCULATE THE LAG IN TIME TO THE PREVIOUS PING
            prelag <- (vessel[j, "DATE"] - vessel[j - 1, "DATE"]) * 24

            if (prelag == 0) {
              prelag <- 0.01
            }
            prevel <- predist / prelag
            ifelse(prevel < max_speed, cohe[j] <- 1, cohe[j] <- 0)

          }else{ ## IF IT IS A DUPLICATE
            cohe[j] <- 1
          }
          next
        }

        ## IF THE PING IS NOT THE FIRST OR THE LAST
        if (j > 1 & j < numlines) {

          ## IF IT IS MARKED AS DUPLICATE
          if (dupl[j]) {

            ## W_COHE WILL BE EQUAL THE PREVIOUS PING COHE
            cohe[j] <- cohe[j-1]
            next
          }

          ## IF THE PREVIOUS PING IS A DUPLICATE
          if (dupl[j-1]){
            succdist <- sp::spDists(matrix(c(vessel[j, "LON"], vessel[j, "LAT"]), ncol = 2),
                                matrix(c(vessel[j + 1, "LON"], vessel[j + 1, "LAT"]), ncol = 2),
                                longlat = TRUE)

            succlag <- (vessel[j + 1, "DATE"] - vessel[j, "DATE"]) * 24
            if (succlag == 0) {
              succlag <- 0.01
            }
            succvel <- succdist / succlag
            ifelse(succvel < max_speed, cohe[j] <- 2, cohe[j] <- 0)
            next
          }

          ## IF THE NEXT PING IS A DUPLICATE
          if (dupl[j+1]) {

            predist <- sp::spDists(matrix(c(vessel[j, "LON"], vessel[j, "LAT"]), ncol = 2),
                               matrix(c(vessel[j - 1, "LON"], vessel[j - 1, "LAT"]), ncol = 2),
                               longlat = TRUE)

            prelag <- (vessel[j, "DATE"] - vessel[j - 1, "DATE"]) * 24
            if (prelag == 0) {
              prelag <- 0.01
            }
            prevel <- predist / prelag
            ifelse(prevel < max_speed, cohe[j] <- 1, cohe[j] <- 0)
            next
          }

          succdist <- sp::spDists(matrix(c(vessel[j, "LON"], vessel[j, "LAT"]), ncol = 2),
                              matrix(c(vessel[j + 1, "LON"], vessel[j + 1, "LAT"]), ncol = 2),
                              longlat = TRUE)
          succlag <- (vessel[j + 1, "DATE"] - vessel[j, "DATE"]) * 24
          if (succlag == 0) {
            succlag <- 0.01
          }
          succvel <- succdist / succlag

          predist <- sp::spDists(matrix(c(vessel[j, "LON"], vessel[j, "LAT"]), ncol = 2),
                             matrix(c(vessel[j - 1, "LON"], vessel[j - 1, "LAT"]), ncol = 2),
                             longlat = TRUE)
          prelag <- (vessel[j, "DATE"] - vessel[j - 1, "DATE"]) * 24
          if (prelag == 0) {
            prelag <- 0.01
          }
          prevel <- predist / prelag

          if (prevel < max_speed) {
            cohe[j] <- 1
          }
          if (succvel < max_speed) {
            cohe[j] <- 2
          }
          if (prevel < max_speed & succvel < max_speed) {
            cohe[j] <- 3
          }
          if (prevel > max_speed & succvel > max_speed) {
            cohe[j] <- 0
          }
        }
      }

      cat(" - Completed!", sep = "")

      ann_data$W_DUPL <- dupl
      ann_data$W_HARB <- harb
      ann_data$W_LAND <- land
      ann_data$W_COHE <- cohe

      ## INSERT DATA INTO THE WARN TABLE
      # sqldf("insert into warn select * from ann_data", dbname = vms_db_path)
      DBI::dbWriteTable(conn, name="warn", value = ann_data, append=T,row.names=FALSE)

    }
  }

  cat("\n\n   ---   End Ping Cleaning   ---\n", sep = "")

  tot_p <- 100 / DBI::dbGetQuery(conn,"select count(*) from warn")[1, ]
  no_p1 <- DBI::dbGetQuery(conn,"select count(*) from warn where W_DUPL = 1")[1, ]
  no_p2 <- DBI::dbGetQuery(conn,"select count(*) from warn where W_HARB = 1")[1, ]
  no_p3 <- DBI::dbGetQuery(conn,"select count(*) from warn where W_LAND = 1")[1, ]
  no_p4 <- DBI::dbGetQuery(conn,"select count(*) from warn where W_COHE = 0")[1, ]

  cat("\n - Found ", no_p1, " (", round(tot_p * no_p1, 2), "% of total)", " duplicated pings!", sep = "")
  cat("\n - Found ", no_p2, " (", round(tot_p * no_p2, 2), "% of total)", " pings in harbour!", sep = "")
  cat("\n - Found ", no_p3, " (", round(tot_p * no_p3, 2), "% of total)", " pings on land!", sep = "")
  cat("\n - Found ", no_p4, " (", round(tot_p * no_p4, 2), "% of total)", " not coherent pings!\n\n", sep = "")

  DBI::dbDisconnect(conn = conn)

}
