#' Check Missing Values
#'
#' @param vec vector of values (n>=1)
#' @return index of missing values
#'
checkMissing <- function(vec){
  na_vec = which(is.na(vec))
  return(na_vec)
}


#' Check If Latitude is In The Right Interval
#' @param lat latitude in decimal format
#' @param minmax range of latitude
#' @return the index of each value out of range [-90, 90]
#'
#' @export
#'
checkLatitude <- function(lat, minmax = c(-90, 90)){
  if((min(minmax) < -90) | (max(minmax) > 90)){
    cat("Latitude range is wrong!")
    return(-1)
  }
  out_lat = which((lat < min(minmax)) | (lat > max(minmax)))
  return(out_lat)
}


#' Check If Longitude is In The Right Interval
#'
#' @param lon longitude in decimal format
#' @param minmax range of longitude
#' @return the index of each value out of range [-180, 180]
#'
#' @export
#'
checkLongitude <- function(lon, minmax = c(-180,180)){
  if((min(minmax) < -180) | (max(minmax) > 180)){
    cat("Latitude range is wrong!")
    return(-1)
  }
  out_lon = which((lon < min(minmax)) | (lon > max(minmax)))
  return(out_lon)
}



#' Check If Longitude and Latitude Are Inverted
#'
#' @param lat latitude
#' @param lon longitude
#' @param latzone range of valid latitudes
#' @param longzone range of valid longitudes
#'
#' @return index of inverted values
#' @export
#'
checkInvertedLatitudeLongitude <- function(lat,lon, latzone = c(-90,90),longzone = c(-180,180)){
  if((min(longzone) < -180) | (max(longzone) > 180)){
    cat("Longitude range is wrong!")
    return(-1)
  }
  if((min(latzone) < -90) | (max(latzone) > 90)){
    cat("Latitude range is wrong!")
    return(-1)
  }
  out_lon = which(((lon >= min(latzone)) & (lon <= max(latzone)))&((lat >= min(longzone)) & (lat <= max(longzone))))
  return(out_lon)
}


#' Check If Longitude and Latitude Are Simultaneously out of range
#'
#' @param lat latitude
#' @param lon longitude
#' @param latzone range of valid latitudes
#' @param longzone range of valid longitudes
#'
#' @return index of abnormal values
#' @export
#'
checkSimulaneousOutOfRange <- function(lat,lon, latzone = c(-90,90),longzone = c(-180,180)){
  if((min(longzone) < -180) | (max(longzone) > 180)){
    cat("Longitude range is wrong!")
    return(-1)
  }
  if((min(latzone) < -90) | (max(latzone) > 90)){
    cat("Latitude range is wrong!")
    return(-1)
  }
  out_lon = which(((lon < min(longzone)) | (lon > max(longzone)))&((lat < min(latzone)) & (lat > max(latzone))))
  return(out_lon)
}


#' Convert DateTime To Chron
#'
#' @param date date value
#' @param time time value
#' @return chron datetime value
#'
#' @export
#'
dateTimeConverter <- function(date,time){
  data_frm <- c(dates = "y-m-d", times = "h:m:s")
  dt=as.numeric(chron::chron(as.character(date), as.character(time), data_frm,origin. = c(month=1,day=1,year=1970)))
  return(dt)
}


#' Knots/h to Km/h
#'
#' @param speed speed in knots per hour
#' @return speed in kilometer per hour
#'
#' @export
#'
kno2kmh <- function(speed) {
  speed <- speed * 1.852
  return(speed)
}


#' Last Index Of
#'
#' @param string a string to be checked
#' @param character a character that we look for
#' @description returns the last index of a character in a string.
#' @return index
#'
lastIndexOf <- function(string, character){
  return(regexpr(paste0("\\",character,"[^\\",character,"]*$"), string)[1])
}

#' Extract File Name
#'
#' @param file complete path with name of file
#' @return file name
#' @description Extract the file name from a path
#' @export
#'
get_file_name <- function(file){
  filename = basename(file)
  n = lastIndexOf(filename,".")
  return(stringr::str_sub(string = filename,start = 1,end = n-1))
}


#' Extract File Directory
#'
#' @param file complete path with name of file
#' @return
#' @description Extract the file directory from a path
#' @export
#'
get_file_path <- function(file){
  return(dirname(file))
}
