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
#' @return the index of each value out of range.
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
#' @return the index of each value out of range.
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
#' @importFrom chron chron
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
#' @importFrom stringr str_sub
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
#' @return file path
#' @description Extract the file directory from a path
#' @export
#'
get_file_path <- function(file){
  return(dirname(file))
}

#' Covert Chron Numeric Date Format to Date Time
#'
#' @param x numeric value of date
#'
#' @return Date Time
#' @export
#'
fromChronToDate <- function(x){
  date = format(as.POSIXct((x) * 86400, origin = "1970-01-01", tz = "UTC"), "%m-%d-%Y %H:%M:%S")
  return(date)
}


#' Longitude to UTM
#' @param long longitude
#' @return UTM values
#' @export
#'
long2UTM <- function(long) {
  (floor((long + 180) / 6) %% 60) + 1
}


# Original spline basis
H0 <- function(x) return(2 * x^3 - 3 * x^2 + 1)
H1 <- function(x) return(-2 * x^3 + 3 * x^2)
H2 <- function(x) return(x^3 - 2 * x^2 + x)
H3 <- function(x) return(x^3 - x^2)
# Derivatives
dH0 <- function(x) return(6 * x^2 - 6 * x)
dH1 <- function(x) return(-6 * x^2 + 6 * x)
dH2 <- function(x) return(3 * x^2 - 4 * x + 1)
dH3 <- function(x) return(3 * x^2 - 2 * x)



o_Tps <- function(x, Y, m = NULL, p = NULL, scale.type = "range",
                  lon.lat = FALSE, miles = TRUE, ...) {
  x <- as.matrix(x)
  d <- ncol(x)
  if (is.null(p)) {
    if (is.null(m)) {
      m <- max(c(2, ceiling(d / 2 + 0.1)))
    }
    p <- (2 * m - d)
    if (p <= 0) {
      stop(" m is too small  you must have 2*m -d >0")
    }
  }
  Tpscall <- match.call()
  if (!lon.lat) {
    Tpscall$cov.function <- "Thin plate spline radial basis functions (Rad.cov) "
    Krig(x, Y,
         cov.function = Rad.cov, m = m, scale.type = scale.type,
         p = p, GCV = TRUE, ...
    )
  }
  else {
    # use different coding of the radial basis fucntions to use with great circle distance.
    Tpscall$cov.function <- "Thin plate spline radial basis functions (RadialBasis.cov) using great circle distance "
    Krig(x, Y,
         cov.function = stationary.cov, m = m, scale.type = scale.type,
         GCV = TRUE, cov.args = list(
           Covariance = "RadialBasis",
           M = m, dimension = 2, Distance = "rdist.earth",
           Dist.args = list(miles = miles)
         ), ...
    )
  }
}


recu_dep <- function(xmin, xmax, ymin, ymax, resolut = 2, the_db = "") {
  if (the_db != "") {
    co_ce <- DBI::dbGetQuery(conn, paste0("select count(*) from intrp where LON > ",xmin,
                                     " and LON < ",xmax," and LAT > ",ymin," and LAT < ", ymax))

    if (co_ce[1, 1] == 0) {
      cat("\n   -     Skipped block: x(", xmin, ",", xmax, ")*y(", ymin, ",", ymax, ")     -\n", sep = "")
    } else {
      xrange <- c(xmin, xmax)
      yrange <- c(ymin, ymax)
      # 	map("worldHires", xlim = xrange, ylim = yrange, bg = "darkorange2", col = "black", fill = T)
      # 	map.axes()
      l_x <- xmax - xmin
      l_y <- ymax - ymin
      if (l_x <= 0.25 | l_y <= 0.25) {
        cat("\n   -     New valid block: x(", xmin, ",", xmax, ")*y(", ymin, ",", ymax, ")     -\n", sep = "")
        deppoi <- DBI::dbGetQuery(conn, paste0("select ROWID, * from intrp where LON > ",xmin,
                                          " and LON < ",xmax," and LAT > ",ymin," and LAT < ",ymax))

        cat("\n   -     Analyzing ", nrow(deppoi), " points     -\n\n", sep = "")

        bat_blo <- marmap::getNOAA.bathy(xmin - 0.1,
                                 xmax + 0.1,
                                 ymin - 0.1,
                                 ymax + 0.1,
                                 resolution = resolut
        )
        plot(bat_blo, image = T)
        points(deppoi[, "LON"], deppoi[, "LAT"], pch = 20, col = "firebrick")

        xlon <- rep(as.numeric(rownames(bat_blo)), length(as.numeric(colnames(bat_blo))))
        ylat <- rep(as.numeric(colnames(bat_blo)), each = length(as.numeric(rownames(bat_blo))))
        zdep <- as.numeric(bat_blo)
        cat("\n   -     Calculating Spline...     -", sep = "")
        SplineD <- o_Tps(cbind(xlon, ylat), zdep, lon.lat = TRUE)
        rm(bat_blo, zdep, xlon, ylat)
        cat("\n   -     Predicting depth", sep = "")
        if (nrow(deppoi) <= 10000) {
          cat(" ", sep = "")
          dept <- as.numeric(predict(SplineD, deppoi[, c("LON", "LAT")]))
          dep_v <- as.data.frame(cbind(deppoi[, "rowid"], deppoi[, "I_NCEE"], dept))
          DBI::dbWriteTable(conn,name = "p_depth",value = dep_v, append = T, row.names = F)
          rm(dept, dep_v)
          gc()
        } else {
          nPin <- ceiling(nrow(deppoi) / 10000)
          for (pi in 1:nPin)
          {
            cat(".", sep = "")
            r1 <- 10000 * (pi - 1) + 1
            r2 <- min(nrow(deppoi), r1 + 10000 - 1)
            dept <- as.numeric(predict(SplineD, deppoi[r1:r2, c("LON", "LAT")]))
            dep_v <- as.data.frame(cbind(deppoi[r1:r2, "rowid"], deppoi[r1:r2, "I_NCEE"], dept))
            DBI::dbWriteTable(conn,name = "p_depth",value = dep_v, append = T, row.names = F)
            rm(dept, dep_v)
            gc()
          }
        }
        cat(" - Completed!     -\n", sep = "")
        rm(SplineD)
        gc()
      } else {
        xmin_2 <- xmin + ((xmax - xmin) / 2)
        ymin_2 <- ymin + ((ymax - ymin) / 2)
        cat("\n   -     Splitting block...     -\n", sep = "")
        recu_dep(xmin, xmin_2, ymin, ymin_2, resolut = resolut, the_db)
        recu_dep(xmin_2, xmax, ymin, ymin_2, resolut = resolut, the_db)
        recu_dep(xmin, xmin_2, ymin_2, ymax, resolut = resolut, the_db)
        recu_dep(xmin_2, xmax, ymin_2, ymax, resolut = resolut, the_db)
      }
    }
  } else {
    cat("\n\n   -     Error! Bad Database File      -\n", sep = "")
  }
}
