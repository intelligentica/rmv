#' Create Relational VMS Data Base
#'
#' @param file the .vms file
#' @param fao_species_code FAO codes for species
#' @param add if TRUE append to existent data, FALSE (default)
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
#' @return Doesn't return a value
#' @export
#' @description creates an SQLite database for VMS data. It creates also additional tables
#' that could be used for other purposes of analyzing and cleaning the data.
#'
maia_create_db <- function(file,fao_species_code="./fao_codes/FAO_CODES_SPECIES.csv", add = FALSE){

  filename = get_file_name(file)
  filepath = get_file_path(file)
  path = paste0(filepath,"/",filename,"_lb.sqlite")

  conn = DBI::dbConnect(RSQLite::SQLite(), path)
  data = utils::read.table(file,header = T,sep = ";",dec = ".")

  if(add==FALSE){
    DBI::dbExecute(conn, "drop table if exists logbook")
    DBI::dbWriteTable(conn, name="logbook", data, append=FALSE, row.names=F)
    cat("\n - logbook table created successfully!\n")
  }else{
    DBI::dbWriteTable(conn, name="logbook", data, append=TRUE, row.names=F)
    cat("\n - logbook table created successfully!\n")
  }

  DBI::dbExecute(conn, "drop table if exists elobo")
  DBI::dbExecute(conn,"CREATE TABLE elobo(vessREF VARCHAR(30), s_utc REAL, e_utc REAL, FAO_SPECIES INT)")
  cat("\n - elobo table created successfully!\n")

  DBI::dbExecute(conn, "drop table if exists lb_cla")
  DBI::dbExecute(conn,"CREATE TABLE lb_cla(vessel VARCHAR(30), log_num INT, met_fo INT, met_des CHAR)")
  cat("\n - lb_cla table created successfully!\n")

  distin <- DBI::dbGetQuery(conn,"select distinct vessREF, s_utc, e_utc, gear, metier from logbook order by s_utc, e_utc")

  cat("\n\n   ---   Logbook Editing Started   ---\n", sep = "")

  numrow <- nrow(distin)
  species <- DBI::dbGetQuery(conn,"select distinct specie from logbook order by specie")
  tocn <- which(species[, 1] == "")
  ifelse(length(tocn) > 0, species <- species[-tocn, 1], species <- species[, 1])

  fao_lst <- utils::read.table(fao_species_code, sep = ",", dec = ".")
  fao_lst$V2 <- toupper(fao_lst$V2)
  species = toupper(species)

  tocl <- which(!(species %in% fao_lst[, 2]))
  if (length(tocl) > 0) {
    species <- species[-tocl]
  }
  code_species = character(length(species))
  for(i in 1:length(species)){
    code_species[i] = fao_lst[which(species[i]==fao_lst[,2]),1]
  }
  spclst <- matrix(data = 0, nrow = 1, ncol = length(species))
  colnames(spclst) <- paste("FAO_", code_species, sep = "")
  proto_log <- data.frame(
    "vessREF" = character(1),
    "s_utc" = numeric(1),
    "e_utc" = numeric(1),
    "gear" = character(1),
    "metier" = character(1)
  )

  proto_log <- cbind(proto_log, spclst)
  hea_str <- paste("vessREF VARCHAR(30), ", paste(colnames(proto_log[2:3]), "REAL", sep = " ", collapse = ", ")
                   , ", ", paste(colnames(proto_log[4:5]), "CHAR", sep = " ", collapse = ", "), sep = "")
  cre_str <- paste(colnames(proto_log[6:ncol(proto_log)]), "INT", sep = " ", collapse = ", ")
  fin_str <- paste(hea_str, cre_str, sep = ", ")

  DBI::dbExecute(conn, "drop table if exists elobo")
  query <- paste("CREATE TABLE elobo(", fin_str, ")", sep = "")
  DBI::dbExecute(conn, query)

  timenow = Sys.time()
  for (n in 1:numrow){
    cat("\nLogbook ", n, " of ", numrow, sep = "")
    if (distin[n, "s_utc"] == "NA" | distin[n, "e_utc"] == "NA" | distin[n, "s_utc"] > distin[n, "e_utc"]) {
      cat(" - Skipped, invalid Date")
      next
    }
    vess_id <- distin[n, "vessREF"]
    utc_rs <- distin[n, "s_utc"]
    utc_re <- distin[n, "e_utc"]
    temp <- DBI::dbGetQuery(conn,paste0("select specie, qty from logbook where vessREF = '",
                                   vess_id,"' and s_utc >= ",utc_rs," and e_utc <= ",utc_re))
    if (nrow(temp) == 0) {
      cat(" - Skipped", sep = "")
      next
    }
    tort <- which(temp[, 1] == "")
    if (length(tort) > 0) {
      temp <- temp[-tort, ]
    }
    if (nrow(temp) == 0) {
      cat(" - Skipped!", sep = "")
      next
    }
    nclm <- (which(species %in% toupper(temp[, 1]))) + 5
    if (max(as.numeric(table(temp[, 1]))) > 1){
      temp <- as.data.frame(cbind(names(tapply(as.numeric(temp[, 2]), temp[, 1], sum)),
                                  as.numeric(tapply(as.numeric(temp[, 2]), temp[, 1], sum))))
    }
    new_log <- data.frame("vessREF" = numeric(1), "s_utc" = numeric(1), "e_utc" = numeric(1),
                          "gear" = character(1), "metier" = character(1))
    new_log <- cbind(new_log, spclst)
    new_log["vessREF"] <- vess_id
    new_log["e_utc"] <- utc_re
    new_log["s_utc"] <- utc_rs
    new_log["gear"] <- distin[n, "gear"]
    new_log["metier"] <- distin[n, "metier"]
    new_log[nclm] <- new_log[nclm] + abs(as.numeric(temp[, 2]))
    DBI::dbWriteTable(conn = conn, name = "elobo",value = new_log,append=TRUE, row.names = FALSE)
  }
  cat("Time Spent : ",Sys.time() - timenow, "\n")
  cat("\n\n   ---   End Logbook Editing   ---\n\n", sep = "")
  DBI::dbDisconnect(conn = conn)
}
