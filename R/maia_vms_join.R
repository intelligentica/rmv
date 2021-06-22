
#' Joining VMS and MAIA data
#'
#' @param logbook_db_path MAIA database path
#' @param vms_db_path VMS database path
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
#' @return no return
#' @export
#'
maia_vms_join <- function(logbook_db_path, vms_db_path){

  connvms = DBI::dbConnect(RSQLite::SQLite(), vms_db_path)
  connlb = DBI::dbConnect(RSQLite::SQLite(), logbook_db_path)

  cat("\n   ---   VMS-Logbook Matching Started   ---\n", sep = "")

  incee_vms <- DBI::dbGetQuery(connvms, "select distinct I_NCEE from track")
  incee_lb <- DBI::dbGetQuery(connlb, "select distinct vessREF from elobo")

  DBI::dbExecute(connvms, "drop table if exists vms_lb")
  query <- "CREATE TABLE vms_lb(vessel VARCHAR(30), track INT, logbook INT, log_id INT)"

  DBI::dbExecute(connvms, query)
  vess <- incee_lb[incee_lb[, 1] %in% incee_vms[, 1], 1]
  num_vess <- length(vess)

  if (num_vess > 0) {
    for (i in 1:num_vess){

      cat("\n   -   Vessel: ", vess[i], " - N.", i, " of ", num_vess, sep = "")
      vms_data <- DBI::dbGetQuery(connvms, paste0("select * from track where I_NCEE = '",vess[i],"' order by DATE"))
      lb_data <- DBI::dbGetQuery(connlb,paste0("select ROWID, * from elobo where vessREF = '",
                                          vess[i],"'  order by s_utc, e_utc"))
      if (nrow(vms_data) == 0 | nrow(lb_data) == 0) {
        cat(" - Skipped, not enough data!\n")
        next
      }else{
        num_track <- length(unique(vms_data$T_NUM))
        res_over <- data.frame(
          "vessel" = vess[i],
          "track" = unique(vms_data$T_NUM),
          "logbook" = numeric(num_track),
          "log_id" = numeric(num_track)
        )
        cat(" - ", num_track, " tracks ", sep = "")

        for (k in 1:num_track){
          trakap <- which(vms_data$T_NUM == k)
          if (length(trakap) == 0) {
            cat("-", sep = "")
            next
          }
          cat(".", sep = "")
          min_tr <- min(vms_data[trakap, "DATE"])
          max_tr <- max(vms_data[trakap, "DATE"])

          if (!is.na(min_tr) | !is.na(max_tr)) {
            if (min_tr != max_tr) {
              int_tr <- intervals::Intervals(c(min_tr, max_tr))

              int_lb <- intervals::Intervals(cbind(lb_data[, "s_utc"], lb_data[, "e_utc"]))

              interval <- intervals::interval_intersection(int_tr, int_lb)

              overlap <- (intervals::interval_overlap(int_tr, int_lb))

              best <- which.max(apply(as.matrix(interval), 1, diff))

              log_num <- unlist(overlap[[1]][best])

              if (length(log_num) != 0) {
                cat("+", sep = "")
                res_over[k, "logbook"] <- log_num
                res_over[k, "log_id"] <- lb_data[log_num, "rowid"]
              }
            }
          }
        }

        no_lobo <- which(res_over[, "logbook"] == 0 & res_over[, "log_id"] == 0)
        if (length(no_lobo) != 0) {
          res_over <- res_over[-no_lobo, ]
          if (nrow(res_over) == 0) {
            cat(" *", sep = "")
            next
          }
        }
        DBI::dbWriteTable(connvms,name = "vms_lb", value = res_over, append=TRUE, row.names = FALSE)
      }
    }
  }else{
    cat("\n\n   ---   STOP   -   No VMS-LogBook Vessel Name Match Found!   ---\n\n", sep = "")
  }

  no_ves <- length(DBI::dbGetQuery(connvms,"select distinct vessel from vms_lb")[, 1])
  no_elog <- length(DBI::dbGetQuery(connvms,"select distinct log_id from vms_lb")[, 1])
  no_mat <- nrow(DBI::dbGetQuery(connvms,"select distinct vessel, track from vms_lb"))
  no_cov <- nrow(DBI::dbGetQuery(connvms,"select distinct I_NCEE, T_NUM from track"))
  no_lobo <- DBI::dbGetQuery(connlb,"select count(*) from elobo")[1, ]

  cat("\n\n   -     VMS DB tracks: ", no_cov, "  -  LB DB logs: ", no_lobo,"  -  estimated VMS Coverage: ", round(100 / no_cov * no_lobo, 2), "%     -", sep = "")
  cat("\n   -     VMS-LB match: ", no_mat, "  -  LB*VMS track: ", round(no_elog / no_mat, 2),"  -  real VMS Coverage: ", round(100 / no_cov * no_mat, 2), "%     -", sep = "")
  cat("\n\n   ---   END VMS-Logbook Matching   ---\n\n", sep = "")

  DBI::dbDisconnect(connvms)
  DBI::dbDisconnect(connlb)

}
