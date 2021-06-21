#' Format TACSAT VMS csv File into Rmaiavms Formmat
#'
#' @param vms_tacsat_file csv file of VMS data in TACSAT format
#' @param latzone latitude range of the studied zone
#' @param longzone longitude range of the studied zone
#' @param file_to_append file to append
#' @param append logical, if TRUE append old file with the new data
#' @importFrom tools file_ext
#' @importFrom chron chron
#' @importFrom utils read.csv read.table write.table
#' @importFrom graphics boxplot
#' @importFrom stats median quantile
#' @return VMS file in rmaiavms format
#' @export
#'
vms_edit_raw <- function(vms_tacsat_file, latzone = c(-90,90),longzone = c(-180,180), append = FALSE, file_to_append = ""){

  if(append){
    if(file_ext(file_to_append)!=".vms"){
      print("The file extention to append should be '.vms'. This file is generated using the 'vms_edit_raw() function.")
    }
  }

  rawvms <- read.csv(file = vms_tacsat_file)

  cat("\n\n   ---   Raw Pings Editing Started!   ---\n\n")
  numlines <- nrow(rawvms)
  cat("Processing ",numlines," raw pings...\n")
  vmsdata <- data.frame(
    "I_NCEE" = character(numlines),
    "LAT" = numeric(numlines),
    "LON" = numeric(numlines),
    "DATE" = numeric(numlines),
    "SPE" = numeric(numlines),
    "HEA" = numeric(numlines)
  )

  ve_ref <- which(names(rawvms) == "VE_REF")
  si_lati <- which(names(rawvms) == "SI_LATI")
  si_long <- which(names(rawvms) == "SI_LONG")
  si_date <- which(names(rawvms) == "SI_DATE")
  si_time <- which(names(rawvms) == "SI_TIME")
  si_sp <- which(names(rawvms) == "SI_SP")
  si_he <- which(names(rawvms) == "SI_HE")

  vmsdata[, "I_NCEE"] <- rawvms[, ve_ref]

  # CHECK AND REMOVE MISSING VALUES IN LATITUDE
  na_lat = checkMissing(rawvms[,si_lati])
  cat("\n  -  ",length(na_lat)," NAs found in Latitude.")
  if(length(na_lat) > 0){
    rawvms = rawvms[-na_lat,]
    vmsdata = vmsdata[-na_lat,]
  }

  # CHECK AND REMOVE OUT OF RANGE VALUES IN LATITUDE
  out_lat = checkLatitude(rawvms[,si_lati])
  cat("\n  -  ",length(out_lat)," Latitudes out of range [-90, 90].")
  if(length(out_lat) > 0){
    rawvms = rawvms[-out_lat,]
    vmsdata = vmsdata[-out_lat,]
  }
  out_lat = checkLatitude(rawvms[,si_lati],minmax = latzone)
  cat("\n  -  ",length(out_lat)," Latitudes out of range [",latzone[1],",", latzone[2],"].")
  vmsdata[, "LAT"] = rawvms[,si_lati]

  # CHECK AND REMOVE MISSING VALUES IN LONGITUDE
  na_lon = checkMissing(rawvms[,si_long])
  cat("\n  -  ",length(na_lon)," NAs found in Longitude")
  if(length(na_lon) > 0){
    rawvms = rawvms[-na_lon,]
    vmsdata = vmsdata[-na_lon,]
  }

  # CHECK AND REMOVE OUT OF RANGE VALUES IN LONGITUDE
  out_lon = checkLongitude(rawvms[,si_long])
  cat("\n  -  ",length(out_lon)," Longitudes out of range [-180, 180].")
  if(length(out_lon) > 0){
    rawvms = rawvms[-out_lon,]
    vmsdata = vmsdata[-out_lon,]
  }
  out_lon = checkLongitude(rawvms[,si_long],minmax = longzone)
  cat("\n  -  ",length(out_lon)," Longitudes out of range [",longzone[1],",", longzone[2],"].")
  vmsdata[, "LON"] = rawvms[,si_long]


  # # CHECK INVERTED LATITUDE LONGITUDE VALUES
  # out_lon = checkInvertedLatitudeLongitude(rawvms[,si_lati],rawvms[,si_long], latzone = latzone,longzone = longzone)
  # cat("\n  -  ",length(out_lon)," Inverted Latitude Longitude")

  # CHECK SIMULANEOUS OUT OF RANGE COORDINATES
  both_out = checkSimulaneousOutOfRange(rawvms[,si_lati],rawvms[,si_long], latzone =latzone,longzone = longzone)
  cat("\n  -  ",length(both_out)," both Latitudes and Longitudes are out of range \n\t\t[",longzone[1],",", longzone[2],"] for longitude and \n\t\t[",latzone[1],",", latzone[2],"] for latitude")

  # CHECK AND REMOVE MISSING VALUES IN DATE
  na_date = checkMissing(rawvms[,si_date])
  cat("\n  -  ",length(na_date)," NAs found in Date")
  if(length(na_date) > 0){
    rawvms = rawvms[-na_date,]
    vmsdata = vmsdata[-na_date,]
  }

  # CHECK AND REMOVE MISSING VALUES IN TIME
  na_time = checkMissing(rawvms[,si_time])
  cat("\n  -  ",length(na_time)," NAs found in Time")
  if(length(na_time) > 0){
    rawvms = rawvms[-na_time,]
    vmsdata = vmsdata[-na_time,]
  }

  vmsdata[,"DATE"] = dateTimeConverter(rawvms[,si_date],rawvms[,si_time])

  # CHECK AND REMOVE MISSING VALUES IN SPEED
  na_speed = checkMissing(rawvms[,si_sp])
  cat("\n  -  ",length(na_speed)," NAs found in Speed")
  if(length(na_speed) > 0){
    rawvms = rawvms[-na_speed,]
    vmsdata = vmsdata[-na_speed,]
  }
  vmsdata[,"SPE"] = kno2kmh(rawvms[,si_sp])

  # CHECK AND REMOVE MISSING VALUES IN HEADING
  na_heading = checkMissing(rawvms[,si_he])
  cat("\n  -  ",length(na_heading)," NAs found in Heading")
  if(length(na_heading) > 0){
    rawvms = rawvms[-na_heading,]
    vmsdata = vmsdata[-na_heading,]
  }
  vmsdata[,"HEA"] = rawvms[,si_he]

  #####
  cat("\n\nRemoved ", round(100*((numlines - nrow(vmsdata))/numlines), 2),
      "% of data, that is ", numlines - nrow(vmsdata), " pings\n")
  cat("\n   ---   Raw Pings Editing Complete!   ---\n")

  ## SAVE THE RESULTED FILE
  cat("\n   ---   Saving Formatted File   ---\n")
  filename = get_file_name(vms_tacsat_file)
  filepath = get_file_path(vms_tacsat_file)
  path = paste0(filepath,"/",filename,".vms")
  write.table(vmsdata, file = path,sep = ";", dec = ".", quote = FALSE, row.names = FALSE)
  cat("\n   ---   Done!   ---\n")

}
