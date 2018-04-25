

#' Create FR Hydro setup for Weekly Margins simulation
#'
#' @param path_capa_hydro Path to hydro capacities CSV file.
#' @param path_hydro Path to hydro usine XML file.
#' @param start Starting date of simulation.
#' @param dispo_pump Pumpage availability.
#' @param opts
#'   List of simulation parameters returned by the function
#'   \code{antaresRead::setSimulationPath}
#'
#' @export
#'
#' @importFrom antaresEditObject createArea createCluster createLink
#'  createBindingConstraint removeBindingConstraint propertiesLinkOptions
#' @importFrom antaresRead getAreas readBindingConstraints
#' @importFrom data.table fread fwrite as.data.table
#' @importFrom utils write.table
#'
#' @examples
#' \dontrun{
#'
#' # todo
#'
#' }
create_wm_hydro_fr <- function(path_capa_hydro, path_hydro, start,
                               dispo_pump = c(3200, 3200, 3020, 2860, 3020, 3180, 3180),
                               opts = antaresRead::simOptions()) {

  #date_i = date de debut de la semaine a etudier
  date_i <- as.Date(start)
  date_f <- date_i + 6

  # dispo_pump <- c(3200, 3200, 3020, 2860, 3020, 3180, 3180)

  # input path
  inputPath <- opts$inputPath

  if (!"lac" %in% antaresRead::getAreas()){
    opts <- createArea(name = "lac", overwrite = TRUE, opts = opts)
    cat("Creating a new area called lac\n")
  }

  # CAPACITE MAX DU LAC+TURB?
  capa_hydro <- fread(file = path_capa_hydro)
  capa_max_hydro <- sum(capa_hydro$PValleeMobilisable, na.rm= TRUE)

  #####
  # Creer un cluster "lac_groupe" avec capacite nominale egal a la capacite maximale du lac+turbinage

  opts <- createCluster(
    area = "lac",
    cluster_name = "generator",
    group = "other",
    unitcount = 1L,
    nominalcapacity = capa_max_hydro,
    `min-down-time` = 1L,
    `marginal-cost` = 0.010000,
    `market-bid-cost` = 0.010000,
    overwrite = TRUE,
    opts = opts
  )


  #Creer un lien fr-lac
  opts <- createLink(
    from = "fr",
    to = "lac",
    propertiesLink = propertiesLinkOptions(
      hurdles_cost = FALSE,
      transmission_capacities = "enabled"
    ),
    dataLink = NULL,
    overwrite = TRUE,
    opts = opts
  )

  # Creer le fichier lac.txt et sauvegarder dans input/links/fr/
  # La capacite des liens est defini par le max a turbiner pendant les heures de pointe

  matrix_ntc_lac <- as.data.table(matrix(data = c(rep(0, 8760*5)), ncol = 5))
  matrix_ntc_lac[1:168, 2] <- rep(capa_max_hydro, 168*1)

  fwrite(
    x = matrix_ntc_lac, row.names = FALSE, col.names = FALSE, sep = "\t",
    file = file.path(inputPath, "/links/fr/lac.txt")
  )


  # On modifie les liens STEP turb, pour laisser la capacite a zero --> deja pris en compte dans le noeud lac
  matrix_ntc_turb <- as.data.table(matrix(data = c(rep(0, 8760*5)), ncol = 5))

  fwrite(
    x = matrix_ntc_turb,
    row.names = FALSE,
    col.names = FALSE,
    sep = "\t",
    file = file.path(inputPath, "/links/fr/turb_d.txt")
  )

  fwrite(
    x = matrix_ntc_turb,
    row.names = FALSE,
    col.names = FALSE,
    sep = "\t",
    file = file.path(inputPath, "/links/fr/turb_w.txt")
  )


  # On modifie les liens STEP pump_w, pour laisser la capacite a zero --> on considere qu'il y aura que du pompage journalier
  matrix_ntc_pump_w <- as.data.table(matrix(data = c(rep(0, 8760*5)), ncol = 5))

  fwrite(
    x = matrix_ntc_pump_w,
    row.names = FALSE,
    col.names = FALSE,
    sep = "\t",
    file = file.path(inputPath, "/links/fr/pump_w.txt")
  )


  # On modifie les liens STEP pump_d, on considere la capa_max du pompage comme la dispo max pendant la nuit
  matrix_ntc_pump_d <- as.data.table(matrix(data = c(rep(0, 8760*5)), ncol = 5))
  matrix_ntc_pump_d[1:168, 1] <- as.data.table(matrix(data = c(rep(dispo_pump[1],24),
                                                              rep(dispo_pump[2],24),
                                                              rep(dispo_pump[3],24),
                                                              rep(dispo_pump[4],24),
                                                              rep(dispo_pump[5],24),
                                                              rep(dispo_pump[6],24),
                                                              rep(dispo_pump[7],24)), ncol = 1))

  fwrite(
    x = matrix_ntc_pump_d,
    row.names = FALSE,
    col.names = FALSE,
    sep = "\t",
    file = paste0(inputPath, "/links/fr/pump_d.txt")
  )

  # Construction du Binding Constraint pour fixer l'energie à turbiner par LAC+TURBINAGE STEP
  # pendant une semaine, et de l'energie à pomper par le STEP


  # vallees <- read_hydro_vallee(path_hydro)
  usines <- read_hydro_usine(path_hydro)

  list_pump <- c("S.BISP","CHEY6P","G.MAIP","REVI5P","MTEZIP","COCHEP")
  list_turb <- c("S.BISH","CHEY6H","G.MAIH","REVI5H","MTEZIH","COCHEH")

  usines_lac <- usines[!groupe_hydro  %in% c(list_pump, list_turb) & selection == 1
                       , lapply(.SD, sum), by = list(datetime,date), .SDcols = c("puis","rprim", "rsec", "rhyd")]

  usines_pump <- usines[selection == 1 & groupe_hydro %in% list_pump
                        , lapply(.SD, sum), by = list(datetime,date), .SDcols = c("puis","rprim", "rsec", "rhyd")]

  usines_turb <- usines[selection == 1 & groupe_hydro %in% list_turb
                        , lapply(.SD, sum), by = list(datetime,date), .SDcols = c("puis","rprim", "rsec", "rhyd")]


  pump_day <- usines_pump[, lapply(.SD, sum), by = "date", .SDcols = "puis"]
  turb_day <- usines_turb[, lapply(.SD, sum), by = "date", .SDcols = "puis"]
  lac_day <- usines_lac[, lapply(.SD, sum), by = "date", .SDcols = "puis"]

  pump_day$puis <- pump_day$puis*2
  turb_day$puis <- turb_day$puis*2
  lac_day$puis <- lac_day$puis*2

  pump_week <- round(sum(pump_day[date >= date_i & date <= date_f, ]$puis) * -1,2)
  turb_week <- round(sum(turb_day[date >= date_i & date <= date_f, ]$puis), 2)
  lac_week <- round(sum(lac_day[date >= date_i & date <= date_f, ]$puis), 2)

  equal_lac <- as.data.table(matrix(0, ncol = 1, nrow = 366))
  equal_lac[1:7] <- rep(round((lac_week + turb_week)/7, 2), 7)
  names(equal_lac) <- "equal"

  equal_pump <- as.data.table(matrix(0, ncol = 1, nrow = 366))
  equal_pump[1:7] <- rep(round((pump_week)/7, 2), 7)
  names(equal_pump) <- "equal"


  # noms binding constraints
  namesbc <- names(readBindingConstraints(opts = opts))

  if(!"fr_lac_energie_hebdo" %in% namesbc){
    opts <- createBindingConstraint(
      name = "fr_lac_energie_hebdo",
      values = equal_lac,
      enabled = TRUE,
      timeStep = "weekly",
      operator = "equal",
      coefficients = c("fr%lac" = -1),
      opts = opts
    )
  } else {
    opts <- removeBindingConstraint("fr_lac_energie_hebdo", opts = opts)
    opts <- createBindingConstraint(
      name = "fr_lac_energie_hebdo",
      values = equal_lac,
      enabled = TRUE,
      timeStep = "weekly",
      operator = "equal",
      coefficients = c("fr%lac" = 1),
      opts = opts
    )
  }

  if(!"fr_pump_energie_hebdo" %in% namesbc){
    opts <- createBindingConstraint(
      name = "fr_pump_energie_hebdo",
      values = equal_pump,
      enabled = TRUE,
      timeStep = "weekly",
      operator = "equal",
      coefficients = c("fr%pump_d" = 1),
      opts = opts
    )
  } else {
    opts <- removeBindingConstraint("fr_pump_energie_hebdo", opts = opts)
    opts <- createBindingConstraint(
      name = "fr_pump_energie_hebdo",
      values = equal_pump,
      enabled = TRUE,
      timeStep = "weekly",
      operator = "equal",
      coefficients = c("fr%pump_d" = 1),
      opts = opts
    )
  }


  matrix_null <- NULL

  write.table(
    x = matrix_null,
    row.names = FALSE,
    col.names = FALSE,
    sep = "\t",
    file = file.path(inputPath, "/hydro/common/capacity/maxpower_fr.txt")
  )

  write.table(
    x = matrix_null,
    row.names = FALSE,
    col.names = FALSE,
    sep = "\t",
    file = file.path(inputPath, "/hydro/prepro/fr/energy.txt")
  )

  write.table(
    x = matrix_null,
    row.names = FALSE,
    col.names = FALSE,
    sep = "\t",
    file = file.path(inputPath, "/hydro/series/fr/mod.txt")
  )


}
