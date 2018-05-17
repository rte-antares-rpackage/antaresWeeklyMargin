library(antaresRead)
library(antaresEditObject)

# set path to your simulation
opts <- setSimulationPath(path = "test_S09_rgf_270218/", simulation = "input")

# build scenarion builder matrix
sbuilder_load <- scenarioBuilder(
  n_scenario = 51,
  n_mc = 2040,
  areas_rand = c("lu_be","lu_de","pump_d","pump_w","turb_d","turb_w", "lac", "fr") # "lac"
)
# check the result
sbuilder_load[, 1:6]
dim(sbuilder_load)

# update in Antares (only Load series)
updateScenarioBuilder(ldata = sbuilder_load, series = "load")


sbuilder_wind <- scenarioBuilder(
  n_scenario = 51,
  n_mc = 2040,
  areas_rand = c("lu_be","lu_de","pump_d","pump_w","turb_d","turb_w", "lac", "ch") # "lac"
)
# check the result
sbuilder_wind[, 1:6]
dim(sbuilder_wind)

# update in Antares (only Load series)
updateScenarioBuilder(ldata = sbuilder_wind, series = "wind")



sbuilder_solar <- scenarioBuilder(
  n_scenario = 51,
  n_mc = 2040,
  areas_rand = c("lu_be","lu_de","pump_d","pump_w","turb_d","turb_w", "lac", "ie","ni") # "lac"
)
# check the result
sbuilder_solar[, 1:6]
dim(sbuilder_solar)

# update in Antares (only Load series)
updateScenarioBuilder(ldata = sbuilder_solar, series = "solar")



##########

opts <- updateOptimizationSettings(number.of.cores.mode = "maximum", opts = opts)

####

updateGeneralSettings(nbyears = 2040, simulation.end = 7, year.by.year = TRUE, opts = opts)
