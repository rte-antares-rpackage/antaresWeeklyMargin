
# TEST NTC ----

library(antaresWeeklyMargin)
library(antaresRead)

# set path to your simulation
opts <- setSimulationPath(path = "test_case/", simulation = "input")

# NTC data
ntc <- data.table::fread("../xml_hydro/inputs/Export_NTC_HEBDO_20180106_20180119.csv")

# Create links in Antares
create_wm_ntc(ntc, start = "2018-01-12")


startday <- "samedi"
start <- as.Date("2018-01-12")
endday <- as.numeric(format(start, "%u")) - 1

date_ini_ntc <- get_previous(startday, date = start)
# date_ini_ntc <- as.POSIXct(date_ini_ntc, tz = "Europe/Paris")
date_fin_ntc <- get_previous(startday, date = start + 7) - 1
# date_fin_ntc <- as.POSIXct(date_fin_ntc, tz = "Europe/Paris")

library(data.table)
ntc_planet <- copy(ntc)

date_debut <- as.POSIXct(x = ntc_planet$DATE[1], format = "%d/%m/%Y")

ntc_planet <- ntc_planet[, date_heure := seq.POSIXt(from = date_debut, length.out = nrow(ntc_planet), by = "1 hour")]

ntc_planet <- ntc_planet[as.Date(date_heure, tz = "Europe/Paris") >= date_ini_ntc & as.Date(date_heure, tz = "Europe/Paris") <= date_fin_ntc]
ntc_planet




# TEST ROR / MISC ----


library(antaresWeeklyMargin)

oa <- read_forfait_oa(path = "../xml_hydro/inputs/hydro_forfait/")

create_wm_ror(data = oa, start = "2018-01-04")


create_wm_misc(data = oa, start = "2018-01-04")



start <- as.Date("2018-01-04")
startday <- "samedi"
endday <- as.numeric(format(start, "%u")) - 1

#on recupere les dates de debut et
date_ini_fil <- get_previous(startday, date = start)
date_fin_fil <- get_previous(endday, date = start)
get_hydraulique_fil_de_l_eau_eclusee(from = date_ini_fil, to = date_fin_fil)
