

library(antaresEditObject)
library(antaresWeeklyMargin)
# library(antaresWaterValues)
library(antaresRead)
library(data.table)

# opts <- antaresRead::setSimulationPath(path = "test_case/", simulation = "input")
# inputPath <- opts$inputPath

areas_etude <- antaresRead::getAreas(opts = opts)
eff_areas <- c(areas_etude[grep("dsr",areas_etude)], areas_etude[grep("rs",areas_etude)])

for (j in eff_areas){
  opts <- removeArea(j, opts = opts)
}

pathIni <- file.path(opts$inputPath, "bindingconstraints/bindingconstraints.ini")
bindingConstraints <- readIniFile(pathIni, stringsAsFactors = FALSE)
namesbc <- unlist(lapply(bindingConstraints, `[[`, "name"), use.names = FALSE)

eff_bc <- c(namesbc[grep("fr_step", namesbc)], namesbc[grep("_fb", namesbc)], namesbc[grep("dsr", namesbc)])

for (i in eff_bc){
  opts <- antaresEditObject::removeBindingConstraint(i, opts = opts)
  print(i)
}

  
