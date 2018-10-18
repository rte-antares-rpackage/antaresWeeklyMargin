

library(antaresWeeklyMargin)

previous_sam <- as.Date("2018-08-25")
next_sam <- previous_sam + 7

# data cnes
prevs <- read_cnes(path = "../test_antares/S35/inputs/cnes/PREV_220818_11h00/")
prevs <- prevs[format(datetime, "%M") == "00"]
prevs <- prevs[datetime >= previous_sam & datetime < next_sam]
prevs

# Nouvelle méthode : calcul automatique des pics et creux
new_offset <- antaresWeeklyMargin:::compute_offset(prevs)
new_offset

# Ancienne méthode : spécification des heures de pics et creux
old_offset <- antaresWeeklyMargin:::prev_offset(prevs, offset_options = offset_opts(
  peak_morning = c(NA, NA, "13:00", "13:00", "13:00", "13:00", "13:00"),
  peak_evening = c(NA, NA, "19:00", "19:00", "19:00", "19:00", "23:00"),
  offpeak_night = c(NA, NA, "04:00", "04:00", "04:00", "04:00", "04:00"),
  offpeak_day = c(NA, NA, "22:00", "22:00", "22:00", "22:00", "22:00")
))
old_offset





#   -----------------------------------------------------------------------


prevs


data_prevs <- copy(prevs)

# moyennes de toutes les prevs
data_prevs[, mean_prev := round(rowMeans(.SD)), .SDcols = setdiff(names(data_prevs), c("datetime", "prevCN","prevu"))]
data_prevs[, offset_prev := prevu - mean_prev]

# data_prevs <- data_prevs[, list(datetime, mean_prev, offset_prev)]

# PEAK
data_prevs[hour(datetime) >= 7 & hour(datetime) <= 14, offset_peak := datetime[which.max(mean_prev)], by = list(format(datetime, "%Y-%m-%d"))]
data_prevs[hour(datetime) >= 18 & hour(datetime) <= 23, offset_peak := datetime[which.max(mean_prev)], by = list(format(datetime, "%Y-%m-%d"))]

# OFFPEAK
data_prevs[hour(datetime) >= 3 & hour(datetime) <= 7, offset_offpeak := datetime[which.min(mean_prev)], by = list(format(datetime, "%Y-%m-%d"))]
data_prevs[hour(datetime) >= 15 & hour(datetime) <= 23, offset_offpeak := datetime[which.min(mean_prev)], by = list(format(datetime, "%Y-%m-%d"))]

# indice des pics et creux
data_prevs[, is_peak := datetime == offset_peak]
data_prevs[, is_offpeak := datetime == offset_offpeak]
data_prevs[is.na(is_peak), is_peak := FALSE]
data_prevs[is.na(is_offpeak), is_offpeak := FALSE]
# pas de pics/creux le weekend
data_prevs[lubridate::wday(datetime, week_start = 1) %in% c(6, 7), is_peak := FALSE]
data_prevs[lubridate::wday(datetime, week_start = 1) %in% c(6, 7), is_offpeak := FALSE]

# ajustement des valuers de l'offset
data_prevs[!(is_peak), offset_prev := NA_real_]
data_prevs[(is_offpeak), offset_prev := 0]
data_prevs[lubridate::wday(datetime, week_start = 1) %in% c(6, 7), offset_prev := 0]


# interpolation de l'offset
data_prevs[, offset_prev := round(zoo::na.spline(offset_prev))]

vars_prev <- grep(pattern = "^prev", x = names(data_prevs), value = TRUE)
vars_prev <- setdiff(vars_prev, c("prevCN", "prevu"))

# ajout de l'offset
data_prevs[, (vars_prev) := .SD + offset_prev, .SDcols = vars_prev]

# maj moyenne des prevs
data_prevs[, mean_prev := round(rowMeans(.SD)), .SDcols = vars_prev]

data_prevs



round(zoo::na.spline(data_prevs$offset_prev))
data_prevs
data_prevs$offset_prev

# cbind(data_prevs$offset_prev, tab$offset_prev)
round(zoo::na.spline(tab$offset_prev))


#   -----------------------------------------------------------------------


tab_offset <- prev_premis[, list(datetime, mean)]
# tab_offset <- tab_offset[, offset_prev := c(rep(0, 168))]
tab_offset

# 
# Peak matin : 7h – 14h
# Peak soir : 18h – 23h
# Creux matin : 03h00 – 07h00
# Creux soir : 15h00 – 23h00
# 
# Peak : il faut récupérer la valeur max de consommation
# Creux : il faut récupérer la valeur min de consommation

# PEAK
# PEAK
tab_offset[hour(datetime) >= 7 & hour(datetime) <= 14, offset_peak := datetime[which.max(mean)], by = format(datetime, "%Y-%m-%d")]
# tab_offset[hour(datetime) >= 7 & hour(datetime) <= 14, is_offset := datetime == offset_peak_m]
tab_offset[hour(datetime) >= 18 & hour(datetime) <= 23, offset_peak := datetime[which.max(mean)], by = format(datetime, "%Y-%m-%d")]
# tab_offset[hour(datetime) >= 18 & hour(datetime) <= 23 & (!(is_offset) | is.na(is_offset)), is_offset := datetime == offset_peak_n]

# OFFPEAK
tab_offset[hour(datetime) >= 3 & hour(datetime) <= 7, offset_offpeak := datetime[which.min(mean)], by = format(datetime, "%Y-%m-%d")]
# tab_offset[hour(datetime) >= 3 & hour(datetime) <= 7 & (!(is_offset) | is.na(is_offset)), is_offset := datetime == offset_offpeak_m]
tab_offset[hour(datetime) >= 15 & hour(datetime) <= 23, offset_offpeak := datetime[which.min(mean)], by = format(datetime, "%Y-%m-%d")]
# tab_offset[hour(datetime) >= 15 & hour(datetime) <= 23 & (!(is_offset) | is.na(is_offset)), is_offset := datetime == offset_offpeak_n]

# tab_offset[, `:=`(offset_peak_m = NULL, offset_peak_n = NULL, offset_offpeak_m = NULL, offset_offpeak_n = NULL)]
tab_offset[, is_peak := datetime == offset_peak]
tab_offset[, is_offpeak := datetime == offset_offpeak]

# tab_offset[is.na(is_offset), is_offset := FALSE]
tab_offset[is.na(is_peak), is_peak := FALSE]
tab_offset[is.na(is_offpeak), is_offpeak := FALSE]
# tab_offset[lubridate::wday(datetime, week_start = 1) %in% c(6, 7), is_offset := FALSE]
tab_offset[lubridate::wday(datetime, week_start = 1) %in% c(6, 7), is_peak := FALSE]
tab_offset[lubridate::wday(datetime, week_start = 1) %in% c(6, 7), is_offpeak := FALSE]



tab_offset[, offset_prev := diff]
tab_offset[!(is_peak), offset_prev := NA_real_]
tab_offset[(is_offpeak), offset_prev := 0]
tab_offset[lubridate::wday(datetime, week_start = 1) %in% c(6, 7), offset_prev := 0]

tab_offset
tab_offset$offset_prev


cbind(tab_offset$offset_prev, tab$offset_prev)



