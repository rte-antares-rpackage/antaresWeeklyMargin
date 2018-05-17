

# dev - clear areas -------------------------------------------------------


library(antaresRead)
library(antaresEditObject)
library(antaresWeeklyMargin)


opts <- antaresRead::setSimulationPath(path = "../test_antares/test_case/", simulation = "input")
inputPath <- opts$inputPath


area2remove <- antaresRead::getAreas(opts = opts, select = c("dsr", "rs"), regexpSelect = TRUE)
for (area in area2remove){
  opts <- removeArea(name = area, opts = opts)
  cat(sprintf("Area %s successfully removed\n", area))
}
antaresRead::getAreas(opts = opts)


bc2remove <- names(readBindingConstraints(opts = opts))
bc2remove <- grep(pattern = "fr_step|_fb|dsr", x = bc2remove, value = TRUE)

for (bc in bc2remove){
  opts <- antaresEditObject::removeBindingConstraint(name = bc, opts = opts)
  at(sprintf("BindingConstraint %s successfully removed\n", bc))
}



antaresRead::getAreas(opts = opts, select = c("truc"), regexpSelect = TRUE)


clear_wm_study(opts = opts)

