#' Assign Depth to VMS Pings
#'
#' @param vms_db_path path the SQLite database of VMS data
#' @param use_res Resolution of data
#' @param use_alg Algorithm to use "Slow & Light" (default) or "Fast & Heavy"
#' @importFrom DBI dbConnect dbGetQuery dbExecute dbDisconnect dbWriteTable
#' @importFrom data.table rbindlist
#' @importFrom sp spDistsN1 over SpatialPoints spDists spTransform CRS coordinates
#' @importFrom  RSQLite SQLite
#' @importFrom chron chron
#' @importFrom maps map
#' @importFrom grDevices extendrange
#' @importFrom marmap getNOAA.bathy
#' @importFrom fields Tps Krig
#' @importFrom graphics abline points
#' @importFrom stats predict
#' @importFrom utils globalVariables
#' @return this function doesn't return a value
#' @export
#'
vms_assign_bathy <- function(vms_db_path, use_res = 2, use_alg = "Slow & Light"){

  conn = DBI::dbConnect(RSQLite::SQLite(), vms_db_path)
  thebo <<- as.numeric(DBI::dbGetQuery(conn,"select max(LON), min(LON), max(LAT), min(LAT) from intrp"))

  maps::map("world", xlim = grDevices::extendrange(thebo[2:1], f = 0.1),
            ylim = grDevices::extendrange(thebo[4:3], f = 0.1),
            col = "honeydew3", bg = "lightsteelblue1", fill = T)
  maps::map.axes()

  abline(v = thebo[1], col = "firebrick")
  abline(v = thebo[2], col = "firebrick")
  abline(h = thebo[3], col = "firebrick")
  abline(h = thebo[4], col = "firebrick")

  cat("\n  - ", thebo, sep = "|")

  nbrLines = DBI::dbGetQuery(conn, "select count(*) from intrp")[1, 1]

  if(nbrLines > 0){

    DBI::dbExecute(conn, "drop table if exists p_depth")
    DBI::dbExecute(conn, "CREATE TABLE p_depth(i_id INT, vess_id VARCHAR(30), DEPTH REAL)")

    if (use_alg == "Slow & Light") {

      xmax <- thebo[1]
      xmin <- thebo[2]
      ymax <- thebo[3]
      ymin <- thebo[4]

      xrange <- grDevices::extendrange(thebo[1:2], f = 0.05)
      yrange <- grDevices::extendrange(thebo[3:4], f = 0.05)

      l_x <- xrange[2] - xrange[1]
      l_y <- yrange[2] - yrange[1]

      area <- l_x * l_y

      max_dim <- 0.25

      l_xp <- sqrt(max_dim / (l_y / l_x))
      l_yp <- sqrt(max_dim / (l_x / l_y))

      xblock <- ceiling(l_x / l_xp)
      yblock <- ceiling(l_y / l_yp)

      cat("\n---   Area divided in ", xblock * yblock, " blocks   ---\n")

      cou <- 1
      for (m in 1:yblock){
        for (n in 1:xblock){

          new_xmin <- xmin + (l_xp * (n - 1))
          new_xmax <- xmin + (l_xp * n)
          new_ymin <- ymin + (l_yp * (m - 1))
          new_ymax <- ymin + (l_yp * m)

          pings <- DBI::dbGetQuery(conn, paste0("select ROWID, * from intrp where LON > ",new_xmin,
                                           " and LON < ",new_xmax," and LAT > ",new_ymin,
                                           " and LAT < ",new_ymax))

          if (nrow(pings) == 0) {
            cat("Skipped block [", m, ",", n, "]\n","N. ", cou, " of ", xblock * yblock, " blocks",sep = "")
            cou <- cou + 1
            next
          }

          cat("Analyzing block [", m, ",", n, "]\n","N. ", cou, " of ", xblock * yblock, " blocks",sep = "")

          bat_blo <- marmap::getNOAA.bathy(new_xmin - 0.1,
                                           new_xmax + 0.1,
                                           new_ymin - 0.1,
                                           new_ymax + 0.1,
                                           resolution = use_res)

          ##########
          blues <- c("lightsteelblue4", "lightsteelblue3", "lightsteelblue2", "lightsteelblue1")
          plot(bat_blo, image = TRUE, land = TRUE, lwd = 0.1, bpal = list(c(0, max(bat_blo), "grey"), c(min(bat_blo), 0, blues)))
          plot(bat_blo, deep = 0, shallow = 0, step = 0, lwd = 0.4, add = TRUE)
          ##########

          points(pings[, "LON"], pings[, "LAT"], pch = 20, col = "firebrick")

          xlon <- rep(as.numeric(rownames(bat_blo)), length(as.numeric(colnames(bat_blo))))
          ylat <- rep(as.numeric(colnames(bat_blo)), each = length(as.numeric(rownames(bat_blo))))
          zdep <- as.numeric(bat_blo)

          cat("Calculating Spln\n", sep = "")
          SplineD <- fields::Tps(cbind(xlon, ylat), zdep, lon.lat = TRUE)
          rm(bat_blo, zdep, xlon, ylat)

          cat("Predicting dpt", sep = "")

          if (nrow(pings) <= 10000) {
            cat(" - ", sep = "")
            dept <- as.numeric(predict(SplineD, pings[, c("LON", "LAT")]))
            dep_v <- data.frame("i_id" = pings[, "rowid"], "vess_id" = pings[, "I_NCEE"], "DEPTH" = dept)
            DBI::dbWriteTable(conn,name = "p_depth",value = dep_v, append = T, row.names = F)
            rm(dept, dep_v)
          } else {
            nPin <- ceiling(nrow(pings) / 10000)
            for (pi in 1:nPin)
            {
              cat(".", sep = "")
              r1 <- 10000 * (pi - 1) + 1
              r2 <- min(nrow(pings), r1 + 10000 - 1)
              dept <- as.numeric(predict(SplineD, pings[r1:r2, c("LON", "LAT")]))
              dep_v <- data.frame("i_id" = pings[r1:r2, "rowid"],"vess_id" = pings[r1:r2, "I_NCEE"], "DEPTH" = dept)
              DBI::dbWriteTable(conn,name = "p_depth",value = dep_v, append = T, row.names = F)
              rm(dept, dep_v)
            }
          }
          cat(" - Completed!\n", sep = "")
          rm(SplineD)
          cou <- cou + 1
        }
      }
      cat("\n\n   ---   End Assign Depth   ---\n", sep = "")
    }else {
      xmax <- thebo[1]
      xmin <- thebo[2]
      ymax <- thebo[3]
      ymin <- thebo[4]

      recu_dep(
        xmin = thebo[2],
        xmax = thebo[1],
        ymin = thebo[4],
        ymax = thebo[3],
        resolut = use_res,
        the_db = vms_db_path
      )
    }
    cat("VMS DB Depth Annotation Completed!")
  }else{
    cat("Interpolated track data not available\n\nRun Interpolation first!")
  }

  DBI::dbDisconnect(conn = conn)
}
