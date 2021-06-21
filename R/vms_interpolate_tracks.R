#' Interpolates VMS tracks into low resolution
#'
#' @param vms_db_path path to VMS SQLite database
#' @param int_fre interpolation frequency in minutes
#' @importFrom DBI dbConnect dbGetQuery dbExecute dbDisconnect dbWriteTable
#' @importFrom data.table rbindlist
#' @importFrom sp spDistsN1 over SpatialPoints spDists spTransform CRS coordinates
#' @importFrom  RSQLite SQLite
#' @importFrom chron chron
#' @return This function doesn't have a return value
#' @export
#'
vms_interpolate_tracks <- function(vms_db_path, int_fre = 10){

  conn = DBI::dbConnect(RSQLite::SQLite(),vms_db_path)
  numvalues = DBI::dbGetQuery(conn, "select count(*) from track")

  if(numvalues > 0){

    ## CREATE THE intrp TABLE
    DBI::dbExecute(conn, "drop table if exists intrp")
    DBI::dbExecute(conn,"CREATE TABLE intrp(I_NCEE VARCHAR(30), LAT REAL, LON REAL, DATE REAL,
            SPE REAL, HEA REAL, W_HARB INT, T_NUM INT, P_ID INT, P_INT INT, T_ID INT)")

    cat("\n   ---   Interpolation Started   ---\n", sep = "")

    ## GET THE UNIQUE VESSEL REFERECENCES
    incee = DBI::dbGetQuery(conn, "select distinct I_NCEE from track")
    num_incee <- nrow(incee)
    ## LOOPING OVER EACH ONE OF THE VESSELS

    for (v in 1:num_incee){

      ## EXTRACT THE track DATA FOR THE CURRENT VESSEL
      vessel <- DBI::dbGetQuery(conn,paste0("select ROWID, * from track where I_NCEE = '",incee[v,1],"' order by DATE"))

      if (nrow(vessel) == 0){ ## IF THERE IS NO DATA FOR THIS VESSEL, SKIP
        cat(" - Skipped, no pings", sep = "")
      }else{
        maxtrk_n <- max(vessel["T_NUM"])
        cat("\nVessel: ", incee[v, 1], " - ", v, " of ", num_incee, " vessels with ", maxtrk_n, " tracks\nInterpolating...", sep = "")

        ## LOOPING OVER EACH ONE OF THE TRACKS
        for (ind in 1:maxtrk_n){

          cat(".", sep = "")
          ## EXTRACT THE CURRENT TRACK FOR THE CURRENT VESSEL
          tracks <- vessel[which(vessel["T_NUM"] == ind), ]
          ## KEEP ONLY TRACK DATA THAT HAVE DATE IN IT
          tracks <- tracks[!is.na(tracks["DATE"]), ]
          ## CALL MY KEEPED TRACK tracki
          tracki <- tracks

          ## COUNT THE NUMBER OF ROWS FOR THIS TRACK OF THIS VESSEL
          numrow <- nrow(tracki)
          ## IF THE NUMBER OF ROW IS GREATER THAN 4 DO:
          if (numrow > 4){

            ## EXTRACT THE LAITIUDE AND LONGITUDE AND PUT THEM INTO A DATAFRAME COO_LL
            COO_LL <- as.data.frame(cbind(tracki[, "LON"], tracki[, "LAT"]))
            ## DEFINE A sign_chk VARIABLE WITH FALSE IN IT
            sign_chk <- FALSE

            if ((abs(sum(sign(COO_LL[, 1]))) != nrow(COO_LL)) & (max(abs(COO_LL[, 1])) > 90)) {
              COO_LL[which(COO_LL[, 1] < 0), 1] <- COO_LL[which(COO_LL[, 1] < 0), 1] + 360
              sign_chk <- TRUE
            }

            ## RENAME THE COLUMNS
            colnames(COO_LL) <- c("X", "Y")
            attr(COO_LL, "projection") <- "LL"

            mid_poi <- min(COO_LL[, 1]) + ((max(COO_LL[, 1]) - min(COO_LL[, 1])) / 2)
            utm_zone <- long2UTM(mid_poi)
            SP_UTM <- spTransform(
              SpatialPoints(COO_LL, proj4string = sp::CRS("+proj=longlat +datum=WGS84")),
              sp::CRS(paste("+proj=utm +zone=", utm_zone, "+datum=WGS84",sep = ""))
            )

            num_ro <- nrow(SP_UTM@coords)
            COO_UTM <- data.frame(
              "X" = numeric(num_ro),
              "Y" = numeric(num_ro),
              "zone" = numeric(num_ro)
            )
            COO_UTM$X <- sp::coordinates(SP_UTM)[, 1] / 1000
            COO_UTM$Y <- sp::coordinates(SP_UTM)[, 2] / 1000

            tracki["LON"] <- COO_UTM[, "X"]
            tracki["LAT"] <- COO_UTM[, "Y"]
            hp <- data.frame("LON" = numeric(numrow), "LAT" = numeric(numrow))
            hcrm <- data.frame("LON" = numeric(numrow), "LAT" = numeric(numrow))
            hdri <- data.frame("LON" = numeric(numrow), "LAT" = numeric(numrow))
            hdrim <- data.frame("LON" = numeric(1), "LAT" = numeric(1))
            tmai <- data.frame("LON" = numeric(numrow), "LAT" = numeric(numrow))
            elle <- numeric(numrow)
            NAXNA <- tracki[, "LON"]
            NAYNA <- tracki[, "LAT"]
            dts <- diff(tracki[, "DATE"])
            dts[which(dts == 0)] <- dts[which(dts == 0)] + 0.0000001
            dts <- dts * 24
            ltrack <- nrow(tracki) - 2

            temp <- 0.5 * (cbind(diff(NAXNA)[-1], diff(NAYNA)[-1]) / dts[-1] + cbind(diff(NAXNA)[-(ltrack + 1)], diff(NAYNA)[-(ltrack + 1)]) / dts[-(ltrack + 1)])

            hcr <- as.data.frame(rbind(rep(0, 2), temp, rep(0, 2)))
            colnames(hcr) <- c("LON", "LAT")
            oldnorms <- sqrt(apply(hcr^2, 1, sum))
            distances <- sqrt((NAXNA[-1] - NAXNA[-length(NAXNA)])^2 + (NAYNA[-1] - NAYNA[-length(NAYNA)])^2)
            newnorms <- c(NA, (distances[-length(distances)] + distances[-1]) / (dts[-length(dts)] + dts[-1]), NA)
            hcrm <- as.data.frame(matrix(rep(newnorms / oldnorms, 2), ncol = 2) * hcr)
            colnames(hcrm) <- c("LON", "LAT")
            hp["LON"] <- tracki["SPE"] * cos((450 - tracki["HEA"]) * (2 * pi) / 360)
            hp["LAT"] <- tracki["SPE"] * sin((450 - tracki["HEA"]) * (2 * pi) / 360)
            hdri["LON"] <- hcrm["LON"] - hp["LON"]
            hdri["LAT"] <- hcrm["LAT"] - hp["LAT"]
            hdrim["LON"] <- median(hdri[which(!is.na(hdri["LON"])), "LON"])
            hdrim["LAT"] <- median(hdri[which(!is.na(hdri["LAT"])), "LAT"])
            tmai["LON"] <- (hdrim[1, "LON"] + hp["LON"])
            tmai["LAT"] <- (hdrim[1, "LAT"] + hp["LAT"])
            temp_inte <- 0.0006944445 * int_fre
            tms <- 1:(ceiling(1 / temp_inte) * (ceiling(tracks[nrow(tracks), "DATE"]) - floor(tracks[1, "DATE"]))) * (temp_inte)
            floo <- floor(tracks[1, "DATE"])
            dec <- tracks["DATE"] - floo
            int <- sort(c(tms[which(tms > min(dec[, "DATE"]) & tms < max(dec[, "DATE"]))], dec[, "DATE"]))
            numpoi <- length(int)
            newpoi <- data.frame(
              "I_NCEE" = numeric(numpoi),
              "LAT" = numeric(numpoi),
              "LON" = numeric(numpoi),
              "DATE" = numeric(numpoi),
              "SPE" = numeric(numpoi),
              "HEA" = numeric(numpoi),
              "W_HARB" = integer(numpoi),
              "T_NUM" = numeric(numpoi),
              "P_ID" = numeric(numpoi),
              "P_INT" = numeric(numpoi),
              "T_ID" = numeric(numpoi)
            )
            newpoi["T_ID"] <- NA
            newpoi["P_ID"] <- NA
            newpoi["I_NCEE"] <- tracki[1, "I_NCEE"]
            newpoi["T_NUM"] <- ind
            newpoi["DATE"] <- int + floo
            newpoi["P_INT"] <- 0
            newpoi[1, "LON"] <- tracki[1, "LON"]
            newpoi[1, "LAT"] <- tracki[1, "LAT"]
            newpoi[1, "SPE"] <- tracki[1, "SPE"]
            newpoi[1, "HEA"] <- tracki[1, "HEA"]
            newpoi[1, "W_HARB"] <- tracki[1, "W_HARB"]
            newpoi[1, "T_ID"] <- tracki[1, "rowid"]
            newpoi[1, "P_ID"] <- tracki[1, "P_ID"]
            newpoi[nrow(newpoi), "LON"] <- tracki[nrow(tracki), "LON"]
            newpoi[nrow(newpoi), "LAT"] <- tracki[nrow(tracki), "LAT"]
            newpoi[nrow(newpoi), "SPE"] <- tracki[nrow(tracki), "SPE"]
            newpoi[nrow(newpoi), "HEA"] <- tracki[nrow(tracki), "HEA"]
            newpoi[nrow(newpoi), "W_HARB"] <- tracki[nrow(tracki), "W_HARB"]
            newpoi[nrow(newpoi), "T_ID"] <- tracki[nrow(newpoi), "rowid"]
            newpoi[nrow(newpoi), "P_ID"] <- tracki[nrow(newpoi), "P_ID"]
            dup <- which(newpoi[, "DATE"] %in% tracks[, "DATE"])
            newpoi[dup, "LON"] <- tracki[which(tracks["DATE"] == newpoi[dup, "DATE"]), "LON"]
            newpoi[dup, "LAT"] <- tracki[which(tracks["DATE"] == newpoi[dup, "DATE"]), "LAT"]
            newpoi[dup, "SPE"] <- tracki[which(tracks["DATE"] == newpoi[dup, "DATE"]), "SPE"]
            newpoi[dup, "HEA"] <- tracki[which(tracks["DATE"] == newpoi[dup, "DATE"]), "HEA"]
            newpoi[dup, "T_ID"] <- tracki[which(tracks["DATE"] == newpoi[dup, "DATE"]), "rowid"]
            newpoi[dup, "P_ID"] <- tracki[which(tracks["DATE"] == newpoi[dup, "DATE"]), "P_ID"]

            for (que in 1:(nrow(dec) - 1)){

              sus <- which(int > dec[que, "DATE"] & int < dec[que + 1, "DATE"])
              dimen <- length(sus)
              if (dimen > 0) {
                if (tracki[que + 1, "LON"] == tracki[que, "LON"] & tracki[que + 1, "LAT"] == tracki[que, "LAT"]) {
                  newpoi[sus, "LON"] <- tracki[que, "LON"]
                  newpoi[sus, "LAT"] <- tracki[que, "LAT"]
                  newpoi[sus, "P_INT"] <- 1
                }
                else {
                  if (que == 1 | que == (nrow(dec) - 1)) {
                    for (repe in 1:dimen)
                    {
                      essei <- data.frame("LON" = numeric(1), "LAT" = numeric(1))
                      deltati <- (dec[que + 1, "DATE"] - dec[que, "DATE"]) * 24
                      coe <- (int[sus[repe]] - dec[que, "DATE"]) * 24 / deltati
                      xi <- cbind(tracki[que, "LON"], tracki[que, "LAT"])
                      xip1 <- cbind(tracki[que + 1, "LON"], tracki[que + 1, "LAT"])
                      essei["LON"] <- xi[1, 1] + (coe * (xip1[1, 1] - xi[1, 1]))
                      essei["LAT"] <- xi[1, 2] + (coe * (xip1[1, 2] - xi[1, 2]))
                      newpoi[sus[repe], "LON"] <- essei["LON"]
                      newpoi[sus[repe], "LAT"] <- essei["LAT"]
                      newpoi[sus[repe], "P_INT"] <- 1
                    }
                  } else {
                    for (repe in 1:dimen)
                    {
                      essei <- data.frame("LON" = numeric(1), "LAT" = numeric(1))
                      difft <- (tracki[que + 1, "DATE"] - tracki[que, "DATE"]) * 24
                      invdifft <- 1 / (difft)
                      normTimes <- ((newpoi[sus[repe], "DATE"] - tracki[que, "DATE"]) * 24) / difft
                      xi <- cbind(tracki[que, "LON"], tracki[que, "LAT"])
                      xip1 <- cbind(tracki[que + 1, "LON"], tracki[que + 1, "LAT"])
                      essei["LON"] <- H0(normTimes) * xi[1] + H1(normTimes) * xip1[1] + H2(normTimes) * difft * tmai[que, "LON"] + H3(normTimes) * difft * tmai[que + 1, "LON"]
                      essei["LAT"] <- H0(normTimes) * xi[2] + H1(normTimes) * xip1[2] + H2(normTimes) * difft * tmai[que, "LAT"] + H3(normTimes) * difft * tmai[que + 1, "LAT"]
                      dX <- invdifft * dH0(normTimes) * xi[1] + invdifft * dH1(normTimes) * xip1[1] + dH2(normTimes) * tmai[que, "LON"] + dH3(normTimes) * tmai[que + 1, "LON"]
                      dY <- invdifft * dH0(normTimes) * xi[2] + invdifft * dH1(normTimes) * xip1[2] + dH2(normTimes) * tmai[que, "LAT"] + dH3(normTimes) * tmai[que + 1, "LAT"]
                      norms <- sqrt(dX^2 + dY^2)
                      thetas <- numeric(1)
                      thetas[dX >= 0] <- atan(dY[dX >= 0] / dX[dX >= 0])
                      thetas[dX < 0] <- -atan(dY[dX < 0] / dX[dX < 0]) + pi
                      thetas[(dX == 0) & (dY == 0)] <- 0
                      newpoi[sus[repe], "LON"] <- essei["LON"]
                      newpoi[sus[repe], "LAT"] <- essei["LAT"]
                      newpoi[sus[repe], "SPE"] <- norms
                      new_hea <- 450 - ((thetas) * 180 / pi)
                      newpoi[sus[repe], "HEA"] <- ifelse(new_hea < 360, new_hea, new_hea - 360)
                      newpoi[sus[repe], "P_INT"] <- 1
                    }
                  }
                }
              }
            }

            utm <- as.data.frame(cbind(newpoi["LON"], newpoi["LAT"]))
            utm_poi <- sp::SpatialPoints(utm * 1000, sp::CRS(paste("+proj=utm +zone=",
                                                           utm_zone,
                                                           "+datum=WGS84",
                                                           sep = ""
            )))

            ll <- sp::spTransform(utm_poi, sp::CRS("+proj=longlat +datum=WGS84"))
            newpoi[, c("LON", "LAT")] <- sp::coordinates(ll)

            if (sign_chk) {
              newpoi[which(newpoi[, 1] > 180), 1] <- newpoi[which(newpoi[, 1] > 180), 1] - 360
            }
            DBI::dbWriteTable(conn,name = 'intrp',value = newpoi,append=TRUE, row.names = F)
          }
        }
        cat(" Completed!\n")
      }
    }
    cat("\n   ---   End Interpolation   --- \n\n")
  }else{
    cat("Track data not available\n\nExecute Track Cut first!")
  }
  DBI::dbDisconnect(conn = conn)
}
