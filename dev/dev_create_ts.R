

# TEST CREATION TS LOAD/SOLAR/WIND


library(antaresRead)
library(antaresWeeklyMargin)

# set path to your simulation
opts <- setSimulationPath(path = "test_case/", simulation = "input")


# Meteologica forecast
formet <- read_meteologica2(path = "../xml_hydro/inputs/PrevisionMeteologica//Europe")

# Create time series
create_wm_ts(data = formet, start = "2018-04-01", opts = opts)



test <- data.table::fread("test_case/input/wind/series/wind_gb.txt")
str(test)
test


?uniqueN
library(data.table)
uniqueN(formet[, list(country, type)])


formet[, .N, by = country]
  formet[, list(min(datetime), max(datetime)), by = country]
datat <- copy(formet)

vars_en <- sprintf("ENS%02d", 0:50)
datat <- datat[country == "UK", (vars_en) := lapply(.SD, sum), by = list(datetime, country, type), .SDcols = vars_en]
datat[, .N, by = country]


library(data.table)

start <- as.Date("2018-04-01")
data <- copy(formet)
data <- data[as.Date(datetime, tz = "Europe/Paris") >= start]
data <- data[as.Date(datetime, tz = "Europe/Paris") < start + 7]
vars_en <- sprintf("ENS%02d", 0:50)
data <- data[country == "UK", (vars_en) := lapply(.SD, sum), by = list(datetime, country, type), .SDcols = vars_en]
data <- unique(data, by = c("datetime", "country", "type"))




austria <- read_meteologica2(path = "../xml_hydro/inputs/PrevisionMeteologica//Europe/Austria/PowerDemand/Forecast/ECMWF-ENS/Total/Hourly/Austria_PowerDemand_Forecast_ECMWF-ENS_Total_Hourly_201803290000.csv")


