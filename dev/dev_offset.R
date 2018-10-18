library(antaresWeeklyMargin)
# start <- as.Date(start)
previous_sam <- as.Date("2018-08-25")
next_sam <- previous_sam + 7


prevs <- read_cnes(path = "../test_antares/S35/inputs/cnes/PREV_220818_11h00/")
prevs <- prevs[format(datetime, "%M") == "00"]
prevs <- prevs[datetime >= previous_sam & datetime < next_sam]
prevs


offset <- antaresWeeklyMargin:::prev_offset(prevs)

offset_options <- antaresWeeklyMargin:::offset_opts()

library(data.table)

dat_prev <- copy(prevs)

peak_morning <- offset_options$peak_morning
peak_evening <- offset_options$peak_evening
offpeak_night <- offset_options$offpeak_night
offpeak_day <- offset_options$offpeak_day

prev_premis <- dat_prev[, !c("prevCN","prevu")]
prev_premis <- prev_premis[, mean := round(apply(.SD, 1, mean), 0), by="datetime"]

prev_u <- dat_prev[, c("datetime","prevu")]

diff <- prev_u$prevu - prev_premis$mean
prev_u <- cbind(prev_u, diff)
offset_prev <- NULL

# offset_cnes <- c(0,0,200,-300,-1000,-200,-800)

tab <- as.data.table(dat_prev$datetime)
names(tab) <- c("datetime")

tab <- tab[, date := as.Date(datetime, tz = "Europe/Paris" )]
tab <- tab[, offset_prev := c(rep(0, 168))]

for(i in 1:7){
  jour <- lubridate::wday(tab$date[i + 23 * (i - 1)], week_start = getOption("lubridate.week.start", 1))
  
  if(jour == 6 || jour == 7){
    offset_aux <- c(rep(0, 24))
    tab$offset_prev[(1 + (24 * (i - 1))):(24 * i)] <- offset_aux[1:24]
  } else {
    h_peak_morning <- as.numeric(stringr::str_sub(peak_morning[i], 1, 2))
    h_peak_evening <- as.numeric(stringr::str_sub(peak_evening[i], 1, 2))
    h_offpeak_night <- as.numeric(stringr::str_sub(offpeak_night[i], 1, 2))
    h_offpeak_day <- as.numeric(stringr::str_sub(offpeak_day[i], 1, 2))
    
    offset_aux <- c(rep(NA, 24))
    offset_aux[h_peak_morning + 1] <- diff[i * 24 - 1]
    offset_aux[h_peak_evening + 1] <- diff[i * 24 - 1]
    offset_aux[h_offpeak_night + 1] <- 0
    offset_aux[h_offpeak_day + 1] <- 0
    
    tab$offset_prev[(1 + (24 * (i - 1))):(24 * i)] <- offset_aux[1:24]
  }
}

tab <- tab[, offset_new := c(round(zoo::na.spline(tab$offset_prev), 0))]
tab <- tab[, -c("date")]

prev_modif <- copy(prev_premis)
prev_modif <- prev_modif[, -c("datetime","mean")]

prev_modif_new <- prev_modif + tab$offset_new
prev_modif_new  <- cbind(datetime = prev_u$datetime,prev_modif_new) 
prev_modif_new  <- prev_modif_new [, mean := round(apply(.SD, 1, mean),0), by = "datetime"]

conso_modif <- cbind(prev_modif_new, prev_u = prev_u$prevu)



