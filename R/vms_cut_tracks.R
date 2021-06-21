#' Cut VMS Pings into Tracks
#'
#' @param use_bp_out parameter for outlier manipulation
#' @param use_med_out parameter for outlier manipulation
#' @param use_ud_freq parameter for outlier manipulation
#' @param vms_db_path VMS SQLite database path
#' @importFrom DBI dbConnect dbGetQuery dbExecute dbDisconnect dbWriteTable
#' @importFrom data.table rbindlist
#' @importFrom sp spDistsN1 over SpatialPoints spDists
#' @importFrom  RSQLite SQLite
#' @importFrom chron chron
#' @return This function doesn't have a return value
#' @export
#'
vms_cut_tracks <- function(vms_db_path,use_bp_out =TRUE, use_med_out = TRUE, use_ud_freq = 120){
  conn = DBI::dbConnect(RSQLite::SQLite(),vms_db_path)
  nbrows = DBI::dbGetQuery(conn, "select count(*) from warn")[1,1]

  if(nbrows!=0){

    DBI::dbExecute(conn, "drop table if exists track")
    DBI::dbExecute(conn,"create table track(I_NCEE VARCHAR(30),
                                       LAT REAL,
                                       LON REAL,
                                       DATE REAL,
                                       SPE REAL,
                                       HEA REAL,
                                       W_HARB INT,
                                       T_NUM INT,
                                       P_ID INT)")
    cat("\n  ---   Track Cutting Started  ---  \n")
    incee <- DBI::dbGetQuery(conn, "select distinct I_NCEE from ping")
    num_incee <- nrow(incee)

    for(v in 1:num_incee){

      cat("\nVessel: ", v, " of ", num_incee, spe = "")
      vessel = DBI::dbGetQuery(conn, paste0("select I_NCEE, LAT, LON, DATE,
                                        SPE, HEA, warn.* from ping, warn
                                        where ping.ROWID = warn.p_id and
                                       I_NCEE = '",incee[v,1],"' and
                                       W_DUPL = 0 and W_COHE != 0 and
                                       W_LAND = 0 order by DATE"))

      numlines = nrow(vessel)

      if(numlines != 0){

        cat(" with ",numlines," pings", sep = "")

        track_data <- data.frame("I_NCEE" = character(numlines),
                                 "LAT" = numeric(numlines),
                                 "LON" = numeric(numlines),
                                 "DATE" = numeric(numlines),
                                 "SPE" = numeric(numlines),
                                 "HEA" = numeric(numlines),
                                 "W_HARB" = integer(numlines),
                                 "T_NUM" = integer(numlines),
                                 "P_ID" = numeric(numlines))

        track_data["I_NCEE"] = vessel["I_NCEE"]
        track_data["LAT"] = vessel["LAT"]
        track_data["LON"] = vessel["LON"]
        track_data["DATE"] = vessel["DATE"]
        track_data["SPE"] = vessel["SPE"]
        track_data["HEA"] = vessel["HEA"]
        track_data["W_HARB"] = vessel["W_HARB"]
        track_data["P_ID"] = vessel["p_id"]

        ## Select the third quantile of the speed of this vessel
        spe3qua <- quantile(as.numeric(vessel[vessel$SPE!=0, "SPE"]), probs = 0.75)

        ## Select all the positions of this vessel that were considered as in harbor
        useharb = unique(cbind(
          vessel[vessel["W_HARB"]==1, "LON"],
          vessel[vessel["W_HARB"]==1, "LAT"]
        ))

        ## Define the tracking number
        tr_num <- 1

        ## Start looping over all the pings of this vessel

        for(j in 1:numlines){

          tr_lin = nrow(track_data)
          delta = tr_lin - numlines

          if((vessel[j, "W_HARB"]==1) | (vessel[j, "W_LAND"]==1)){

            track_data[j + delta, "T_NUM"] = 0
            next
          }else{

            track_data[j + delta, "T_NUM"] = tr_num

            ## Looking if this vessel has previous positions marked as harbor
            ## If not : go to next iteration
            if(nrow(useharb) == 0){
              next
            }

            ## Calculate the distance between the vessel and the nearest harbor
            ## Case of 1 harbor point:
            if(nrow(useharb) == 1){
              dist = sp::spDists(cbind(useharb[, 1], useharb[, 2]), cbind(vessel[j, "LON"], vessel[j, "LAT"]),longlat = TRUE)
            }

            ## Case of multiple harbor points:
            if(nrow(useharb)>1){
              dist = sp::spDistsN1(useharb[, 1:2], cbind(vessel[j, "LON"], vessel[j, "LAT"]),longlat = TRUE)
            }

            ## What the smallest distance and which harbor point is that.
            hdist = min(dist)
            nearh = which.min(dist)

            # If this is not the first ping of the current vessel
            if(j > 1){
              ## Check if the previous ping was actually in harbor
              if(vessel[j - 1, "W_HARB"] == 1){
                ## Let's determine an intermediary point that is in harbor and near to the current ping
                newdate = vessel[j, "DATE"] - ((1/24)*(hdist / spe3qua))
                ## Let's create this fictional ping, this will define the beginning of a trip
                tmp = data.frame("I_NCEE" = vessel[j, "I_NCEE"],
                                 "LAT" = useharb[nearh, 2],
                                 "LON" = useharb[nearh, 1],
                                 "DATE" = ifelse(vessel[j - 1, "DATE"] < newdate & newdate!= Inf & newdate != -Inf,newdate,vessel[j - 1, "DATE"]),
                                 "SPE" = 0,
                                 "HEA" = 0,
                                 "W_HARB" = 1,
                                 "T_NUM" = tr_num,
                                 "P_ID" = NA)
                track_data = data.table::rbindlist(l = list(
                  track_data[1:(j - 1 + delta),],
                  tmp,
                  track_data[(j + delta):nrow(track_data), ]
                ))
              }
            }
            tr_lin = nrow(track_data)
            delta = tr_lin - numlines
            ## End of this condition, let's update delta

            ## Check if this is not the last ping
            if(j < nrow(vessel)){
              ## Check if the next ping is in harbor
              if(vessel[j + 1, "W_HARB"] == 1){
                ## What will be the time when reaching the nearest point that is in harbor
                newdate = vessel[j, "DATE"] + ((1 / 24) * (hdist / spe3qua))
                ## Let's create this fictional ping that will mark the end of the trip
                tmp = data.frame("I_NCEE" = vessel[j, "I_NCEE"],
                                 "LAT" = useharb[nearh, 2],
                                 "LON" = useharb[nearh, 1],
                                 "DATE" = ifelse(vessel[j + 1, "DATE"] > newdate & newdate != Inf & newdate != -Inf, newdate, vessel[j + 1, "DATE"]),
                                 "SPE" = 0,
                                 "HEA" = 0,
                                 "W_HARB" = 1,
                                 "T_NUM" = tr_num,
                                 "P_ID" = NA)
                track_data = data.table::rbindlist(l = list(
                  track_data[1:(j + delta), ],
                  tmp,
                  track_data[(j + 1 + delta):nrow(track_data),]
                ))

                tr_num = tr_num + 1
              }
            }
          }
        }

        ## Finished the loop over all the pings of this current vessel

        ## Let's remove all additional pings that are in harbor and keep
        ## the one that defines the beginning of a trip and the end of it.
        track_data = track_data[which(track_data[, "T_NUM"] > 0),]

        ## We are still with the same vessel
        ## Now let's try to filter unusual pings based on frequency
        ## Let's check we still have data after we delete T_NUM == 0

        if( nrow(track_data) > 0){

          pingFreq = diff(track_data$DATE)
          ## If the option for handling outliers is activated do the following
          ## Using boxplot function we get the outliers

          if(use_bp_out){

            ## Get the values that are considered as outliers using boxplot function
            ## Calculate it using in sea pings / not in harbor
            out = boxplot(diff(track_data$DATE[track_data$W_HARB==0]), plot = F)$out
            ## If there are values in `out`
            if(length(out) > 0){

              ## if using median option is TRUE do the following
              ## Select the outliers values that are greater than the median of ping frequencies and
              ## greater than the usual frequency which is for Morocco vessels is 2 hours
              if(use_med_out){
                da_to = which(out > median(pingFreq) & out > ((1/24) * (use_ud_freq / 60)))
              }else{
                ## If median criteria option is not selected, just compare frequencies to 2 hours (the usual frequency)
                da_to = which(out > ((1/24) * (use_ud_freq / 60)))
              }

              ## Now check if there are values in da_to
              if(length( da_to ) > 0){

                ## Select the minimum value of the outliers
                ## This will be used as a reference point to classify outliers
                outmin = min(out[da_to])

                ## Let's determine the outliers in ping frequency
                ## Select index of frequency values that are greater than the min outlier or
                ## 3 hours (a limit specified by user) and are not in harbor
                outliers <- which((diff(track_data$DATE) >= outmin |
                                     diff(track_data$DATE) >= 0.125) &
                                    track_data$W_HARB[-c(nrow(track_data))] != 1)
                ## If we have values that verify these conditions start cutting these pings into trips
                if(length(outliers) > 0){

                  cat("\n  ---   Splitting tracks  ---  \n",sep = "")

                  ## Loop over all the outlier values and try correct them
                  for(k in 1:length(outliers)){

                    cat(":", sep = "")
                    if (nrow(useharb) == 0) {
                      next
                    }

                    if (nrow(useharb) > 1) {
                      dist1 <- sp::spDistsN1(useharb[, 1:2],
                                         as.matrix(c(track_data$LON[outliers[k]], track_data$LAT[outliers[k]])),
                                         longlat = TRUE
                      )
                    }

                    if (nrow(useharb) == 1) {
                      dist1 <- sp::spDists(cbind(useharb[, 1], useharb[, 2]),(cbind(track_data$LON[outliers[k]], track_data$LAT[outliers[k]])), longlat = TRUE)
                    }

                    hdist1 <- min(dist1)
                    nearh1 <- which.min(dist1)
                    time1 <- hdist1 / spe3qua

                    dist2 <- sp::spDists(cbind(useharb[nearh1, 1], useharb[nearh1, 2]),
                                     (cbind(track_data[(outliers[k] + 1), "LON"], track_data[(outliers[k] + 1), "LAT"])), longlat = TRUE)

                    dist2 <- sp::spDists(cbind(useharb[nearh1, 1], useharb[nearh1, 2]),
                                     (cbind(track_data$LON[(outliers[k] + 1)], track_data$LAT[(outliers[k] + 1)])), longlat = TRUE)

                    hdist2 <- min(dist2)
                    nearh2 <- which.min(dist2)
                    time2 <- hdist2 / spe3qua

                    estDate = track_data[(outliers[k]), "DATE"] + (1/24)*time1 + (1/24)*time2 + 0.04166667

                    if(track_data[(outliers[k] + 1), "DATE"] < estDate){ ## TIME IS NOT SUFFICIENT TO GO TO PORT AND COME BACK
                      next
                    }else{

                      newdate1 <- track_data[(outliers[k]), "DATE"] + ((1 / 24) * time1)
                      newdate2 <- track_data[(outliers[k] + 1), "DATE"] - ((1 / 24) * time2)

                      tmp1 = data.frame("I_NCEE" = track_data[(outliers[k]), "I_NCEE"],
                                        "LAT" = useharb[nearh[1], 2],
                                        "LON" = useharb[nearh[1], 1],
                                        "DATE" = newdate1,
                                        "SPE" = 0,
                                        "HEA" = 0,
                                        "W_HARB" = 1,
                                        "T_NUM" = track_data[(outliers[k]), "T_NUM"],
                                        "P_ID" = NA)

                      tmp2 = data.frame("I_NCEE" = track_data[(outliers[k] + 1), "I_NCEE"],
                                        "LAT" = useharb[nearh2[1], 2],
                                        "LON" = useharb[nearh2[1], 1],
                                        "DATE" = newdate2,
                                        "SPE" = 0,
                                        "HEA" = 0,
                                        "W_HARB" = 1,
                                        "T_NUM" = track_data[(outliers[k] + 1), "T_NUM"],
                                        "P_ID" = NA)

                      track_data = data.table::rbindlist(l = list(
                        track_data[1:(outliers[k]), ],
                        tmp1,
                        tmp2,
                        track_data[(outliers[k] + 1):nrow(track_data), ]
                      ))

                      track_data$T_NUM[(outliers[k] + 2):nrow(track_data)] <- track_data$T_NUM[(outliers[k] + 2):nrow(track_data)] + 1
                      outliers <- outliers + 2
                    }
                  }
                }
              }else{
                cat(" -  No Ping Frequency outliers found ", sep = "")
              }
            }else{
              cat(" -  No Ping Frequency outliers found ", sep = "")
            }
          }else{## If use_bp_out parameter was not selected as TRUE
            ## We will base our outliers analysis on

            ## Check if median criteria was selected
            if(use_med_out){
              outliers = which(pingFreq > median(pingFreq) | pingFreq > ((1/24) * (use_ud_freq / 60)))
            }else{
              outliers = which(pingFreq > ((1/24) * (use_ud_freq / 60)))
            }
            ## Select the outlier frequencies that are actually registred when in harbor
            in_harb = which(track_data$W_HARB[outliers]==1)
            ## Keep only in sea outliers
            outliers = outliers[-in_harb]
            ## Check if there are some outliers to analyze
            if(length(outliers) > 0){

              cat(" - Splitting Tracks ", sep = "")

              ## Loop over all the outliers
              for(k in 1:length(outliers)){

                cat(":", sep = "")

                if (nrow(useharb) == 0) {
                  next
                }
                if (nrow(useharb) > 1) {
                  dist1 <- sp::spDistsN1(useharb[, 1:2], as.matrix(c(track_data[outliers[k], "LON"], track_data[outliers[k], "LAT"])),longlat = TRUE)
                }

                if (nrow(useharb) == 1) {
                  dist1 <- sp::spDists(cbind(useharb[, 1], useharb[, 2]),(cbind(track_data[outliers[k], "LON"], track_data[outliers[k], "LAT"])), longlat = TRUE)
                }

                hdist1 <- min(dist1)
                nearh1 <- which.min(dist1)
                time1 <- hdist1 / spe3qua

                dist2 <- sp::spDists(cbind(useharb[nearh1, 1], useharb[nearh1, 2]),
                                 (cbind(track_data[(outliers[k] + 1), "LON"], track_data[(outliers[k] + 1), "LAT"])), longlat = TRUE)
                hdist2 <- min(dist2)
                nearh2 <- which.min(dist2)
                time2 <- hdist2 / spe3qua

                estDate = track_data[(outliers[k]), "DATE"] + (1/24)*time1 + (1/24)*time2 + 0.04166667

                if(track_data[(outliers[k] + 1), "DATE"] < estDate){
                  next
                }else{

                  newdate1 <- track_data[(outliers[k]), "DATE"] + ((1 / 24) * time1)
                  newdate2 <- track_data[(outliers[k] + 1), "DATE"] - ((1 / 24) * time2)

                  tmp1 = data.frame("I_NCEE" = track_data[(outliers[k]), "I_NCEE"],
                                    "LAT" = useharb[nearh[1], 2],
                                    "LON" = useharb[nearh[1], 1],
                                    "DATE" = newdate1,
                                    "SPE" = 0,
                                    "HEA" = 0,
                                    "W_HARB" = 1,
                                    "T_NUM" = track_data[(outliers[k]), "T_NUM"],
                                    "P_ID" = NA)

                  tmp2 = data.frame("I_NCEE" = track_data[(outliers[k] + 1), "I_NCEE"],
                                    "LAT" = useharb[nearh2[1], 2],
                                    "LON" = useharb[nearh2[1], 1],
                                    "DATE" = newdate2,
                                    "SPE" = 0,
                                    "HEA" = 0,
                                    "W_HARB" = 1,
                                    "T_NUM" = track_data[(outliers[k] + 1), "T_NUM"],
                                    "P_ID" = NA)

                  track_data = data.table::rbindlist(l = list(
                    track_data[1:(outliers[k]), ],
                    tmp1,
                    tmp2,
                    track_data[(outliers[k] + 1):nrow(track_data), ]
                  ))

                  track_data$T_NUM[(outliers[k] + 2):nrow(track_data)] <- track_data$T_NUM[(outliers[k] + 2):nrow(track_data)] + 1
                  outliers <- outliers + 2
                }
              }
            }
          }
          DBI::dbWriteTable(conn, name="track", value = track_data, append=T,row.names=FALSE)
        }
      }else{
        cat(" - Skipped, no pings", sep = "")
      }
    }
    cat("\n\n   ---   End Track Cutting   ---\n", sep = "")
  }else{
    cat("Warning data not available\n\nExecute DB Cleaning first!")
  }

  DBI::dbDisconnect(conn = conn)
}
