

if (FALSE) {

  library(xml2)

  # Hydro
  path <- "tests/testthat/files/xml_hydro/9222_PuissancesHydrauliquesHebdo-fcu121217_20171212-1120.xml"
  x <- read_xml(x = path)
  xml_remove(xml_find_all(x = x, xpath = "Vallee")[3:41])
  xml_remove(xml_find_all(x = x, xpath = "Usine")[3:200])
  write_xml(x = x, file = "tests/testthat/files/xml_hydro/PuissancesHydrauliquesHebdo.xml")
  unlink(path)

  # Thermal
  path <- "tests/testthat/files/xml_thermal/9221_PuissancesThermiquesHebdo-fcu131217_20171213-1136.xml"
  x <- read_xml(x = path)
  xml_remove(xml_find_all(x = x, xpath = "Tranche")[3:86])
  write_xml(x = x, file = "tests/testthat/files/xml_thermal/PuissancesThermiquesHebdo.xml")
  unlink(path)

}
