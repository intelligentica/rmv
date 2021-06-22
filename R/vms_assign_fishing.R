#' Title
#'
#' @param vms_db_path vms databse path
#' @param harb_file_path harbor shapefile
#' @param min_vel_km minimum velocity of fishing
#' @param max_vel_km maximum velocity of fishing
#' @param min_depth_mt min depth of fishing
#' @param max_depth_mt max depth of fishing
#' @param harb_dist_km harbor distance
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
#' @importFrom utils globalVariables read.table
#' @importFrom dplyr filter mutate arrange mutate
#' @importFrom tidyr separate
#' @importFrom intervals Intervals
#' @importFrom rgdal readOGR
#' @return
#' @export
#'
#' @examples
vms_assign_fishing <- function(vms_db_path, harb_file_path, min_vel_km = 3.7, max_vel_km = 11.11,min_depth_mt=-5000,max_depth_mt=-5,harb_dist_km=16){

  conn = DBI::dbConnect(RSQLite::SQLite(),vms_db_path)
  harbs <- rgdal::readOGR(harb_file_path)

  cat("\n\n   ---   Fishing Point Analysis Started   ---\n", sep = "")

  DBI::dbExecute(conn,"drop table if exists p_fish")
  DBI::dbExecute(conn,"CREATE TABLE p_fish(i_id INT, F_SPE INT, F_DEP INT, F_DIS INT, FISH INT)")

  incee_vms <- DBI::dbGetQuery(conn,"select distinct I_NCEE from intrp")
  num_vess <- nrow(incee_vms)

  for (i in 1:num_vess) {

    cat("\nVessel: ", incee_vms[i, 1], " - N.", i, " of ", num_vess, sep = "")
    query = paste0("select vessel, track from vms_lb where vessel = '",incee_vms[i,1],"'")
    match <- DBI::dbGetQuery(conn, query)
    num_track <- nrow(match)

    if (num_track == 0) {
      cat(" - Skipped, VMS-LogBook Match not found!")
      next
    }
    for (k in 1:num_track) {

      cat(".", sep = "")
      eff_tra <- match[k, "track"]
      query = paste0("select * from intrp, p_depth where intrp.I_NCEE = '",incee_vms[i,1],"' and intrp.T_NUM = ",eff_tra," and intrp.rowid = p_depth.i_id")
      sin_tra <- DBI::dbGetQuery(conn, query)

      if (nrow(sin_tra) == 0) {
        cat("-", sep = "")
        next
      }

      sin_tra <- cbind(sin_tra, 0, 0, 0, 0)
      colnames(sin_tra)[(ncol(sin_tra) - 3):ncol(sin_tra)] <- c("F_SPE", "F_DEP", "F_DIS", "FISH")

      # cat(" - Checking Speed... ", sep = "")
      sin_tra[which((sin_tra[, "SPE"]) >= min_vel_km & (sin_tra[, "SPE"] ) <= max_vel_km), "F_SPE"] <- 1
      # cat("Depth... ", sep = "")
      sin_tra[which(sin_tra[, "DEPTH"] >= min_depth_mt & sin_tra[, "DEPTH"] <= max_depth_mt), "F_DEP"] <- 1

      # cat("Distance... ", sep = "")
      for (j in 1:nrow(sin_tra)) {
        nea_har <- which(sp::spDistsN1(harbs, as.numeric(sin_tra[j, c("LON", "LAT")]), longlat = TRUE) < harb_dist_km)
        if (length(nea_har) == 0) {
          sin_tra[j, "F_DIS"] <- 1
        }
      }
      fis_poi <- which(sin_tra[, "F_SPE"] == 1 & sin_tra[, "F_DEP"] == 1 & sin_tra[, "F_DIS"] == 1)
      if (length(fis_poi) != 0) {
        cat("+", sep = "")
        sin_tra[fis_poi, "FISH"] <- 1
      }
      result <- sin_tra[, c("i_id", "F_SPE", "F_DEP", "F_DIS", "FISH")]
      rm(sin_tra)
      gc()

      DBI::dbWriteTable(conn, "p_fish", result, append = TRUE, row.names = F)
      rm(result)
      gc()
    }
  }
  cat("\n\n   ---   END Fishing Point Analysis   ---\n\n", sep = "")

  DBI::dbDisconnect(conn = conn)
}
