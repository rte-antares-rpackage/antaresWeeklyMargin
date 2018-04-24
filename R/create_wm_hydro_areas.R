
#' Create Others Areas Hydro setup for Weekly Margins simulation
#'
#' @param start Starting date of simulation.
#' @param simulation_source Path to source simulation.
#' @param opts
#'   List of simulation parameters returned by the function
#'   \code{antaresRead::setSimulationPath}
#'
#' @export
#'
#' @importFrom antaresRead readClusterDesc setSimulationPath simOptions getAreas readInputTS getLinks readBindingConstraints
#' @importFrom antaresEditObject removeLink createLink propertiesLinkOptions createCluster createBindingConstraint
#' @importFrom data.table fwrite as.data.table copy
#' @importFrom utils write.table
#'
#' @examples
#' \dontrun{
#'
#' # todo
#'
#' }
create_wm_hydro_areas <- function(start, simulation_source = "PDH 2017-2018 - Copie/", opts = antaresRead::simOptions()) {

  inputPath <- opts$inputPath

  input_pdh <- antaresRead::setSimulationPath(path = simulation_source, simulation = "input")

  #date_i = date de debut de la semaine a etudier
  date_i <- as.Date(start)
  date_f <- date_i + 7

  areas <- c("at","be","ch","es","gb","ie","it","pt")
  # areas <- c("de")

  max_lac <- 0

  if (!"lac" %in% antaresRead::getAreas(opts = opts)){
    opts <- createArea(name = "lac", overwrite = TRUE, opts = opts)
    cat(sprintf("Creating a new area called lac"))
  }


  max_reservoir <- antaresRead::readInputTS(hydroStorageMaxPower = "all", opts = input_pdh)
  max_reservoir <- max_reservoir[area %in% areas
                                 & time > as.POSIXct(paste0(date_i, "00:00:00"))
                                 & time <= as.POSIXct(paste0(date_f, "00:00:00"))]

  matrix_ntc_lac <- as.data.table(matrix(data = c(rep(as.integer(0), 8760*5)), ncol = 5))

  i <- areas[1]

  # Configuration des liens areas/lac
  for (i in areas){
    print(i)
    links_etude <- antaresRead::getLinks(opts = opts)

    if (paste0(i, " - lac") %in% links_etude ||  paste0("lac - ", i) %in% links_etude){
      opts <- removeLink(from = i, to = "lac", opts = opts)
    } else {
      opts <- createLink(
        from = i,
        to = "lac",
        propertiesLink = propertiesLinkOptions(
          hurdles_cost = FALSE,
          transmission_capacities = "enabled"
        ),
        dataLink = NULL,
        overwrite = TRUE,
        opts = opts
      )
    }

    if (i < "lac"){

      matrix_lac_area <- copy(matrix_ntc_lac)
      matrix_lac_area[1:168, 2] <- max_reservoir[area == i]$hstorPMaxHigh
      max_lac_area <- max(matrix_lac_area[2])

      fwrite(
        x = matrix_lac_area,
        row.names = FALSE,
        col.names = FALSE,
        sep = "\t",
        file = file.path(inputPath, "links", i, "lac.txt")
      )

    } else {

      matrix_lac_area <- copy(matrix_ntc_lac)
      matrix_lac_area[1:168, 1] <- max_reservoir[area == i]$hstorPMaxHigh
      max_lac_area <- max(matrix_lac_area[1])

      fwrite(
        x = matrix_lac_area,
        row.names = FALSE,
        col.names = FALSE,
        sep = "\t",
        file = file.path(inputPath, "links", "lac", paste0(i, ".txt"))
      )
    }

    max_lac <- max_lac_area + max_lac
  }

  print(max_lac)
  # Changement de nominal capacity du lac_generator
  if (nrow(readClusterDesc(opts = opts)[area == "lac",]) != 0 ) {
    cluster_lac <- readClusterDesc(opts = opts)[area == "lac",]
    capa_lac_fr <- cluster_lac$nominalcapacity

    # Test avec capa_lac_autres qui reste a determiner

    capa_cluster_lac <- capa_lac_fr+max_lac
    print(capa_cluster_lac)

    opts <- createCluster(
      area = "lac",
      cluster_name = "generator",
      group = "other",
      unitcount = 1L,
      nominalcapacity = capa_cluster_lac,
      `min-down-time` = 1L,
      `marginal-cost` = 0.010000,
      `market-bid-cost` = 0.010000,
      overwrite = TRUE,
      opts = opts
    )
    cat(sprintf("Nominal Capacity of lac_generator has been modified"))
  } else {
    cat(sprintf("Cluster lac_generator does not exist"))



    # Creation des binding constraints pour l'energie turbine par le noeud lac

    for (i in areas) {
      print(i)

      energy_lac <- readEnergy(area = i, opts = input_pdh)
      energy_lac <- energy_lac[date >= date_i & date < date_f]$expectation

      #calcul de l'energie a turbiner par semaine par le reservoir lac (*1000 parce que les donnees sont en GWh)
      equal_lac <- as.data.table(matrix(0, ncol=1, nrow=366))
      equal_lac[1:7] <- round(energy_lac*1000,2)
      names(equal_lac) <- "equal"

      namesbc <- names(readBindingConstraints(opts = opts))

      nom_bc <- paste0(i, "_lac_energie_hebdo")

      if (!paste0(i, "_lac_energie_hebdo") %in% namesbc){
        if (i < "lac"){
          createBindingConstraint(
            name = nom_bc,
            values = equal_lac,
            enabled = TRUE,
            timeStep = "weekly",
            operator = "equal",
            coefficients = setNames(1, paste0(i, "%lac")),
            opts = opts
          )
        } else {
          createBindingConstraint(
            name = nom_bc,
            values = equal_lac,
            enabled = TRUE,
            timeStep = "weekly",
            operator = "equal",
            coefficients = setNames(1, paste0("lac%",i)),
            opts = opts
          )
        }
      }

      print(i)
      # antaresWaterValues::resetHydroStorage(i, opts = opts)
      matrix_null <- NULL
      print("reset hydro storage")

      write.table(
        x = matrix_null,
        row.names = FALSE,
        col.names = FALSE,
        sep = "\t",
        file = file.path(inputPath, "hydro", "common", "capacity", paste0("maxpower_", i, ".txt"))
      )

      write.table(
        x = matrix_null,
        row.names = FALSE,
        col.names = FALSE,
        sep = "\t",
        file = file.path(inputPath, "hydro", "prepro", i, "energy.txt")
      )

      write.table(
        x = matrix_null,
        row.names = FALSE,
        col.names = FALSE,
        sep = "\t",
        file = file.path(inputPath, "hydro", "series", i, "mod.txt")
      )


    }
  }
}




