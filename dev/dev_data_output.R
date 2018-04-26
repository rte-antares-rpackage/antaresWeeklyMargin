

# Preparation donnees output ----------------------------------------------

library(antaresWeeklyMargin)
library(antaresRead)
library(data.table)

opts <- setSimulationPath(path = "test_output/test_S09/", simulation = 3)

mono_data <- process_data_mono(start = "2018-02-24", date = "2018-03-01 17:00:00", area = "fr", nb_MC = 2040, opts = opts)

mono_data <- readRDS(file = "test_output/mono.rds")
str(mono_data)

draw_mono(data = mono_data$mono_be)
draw_mono(data = mono_data$mono_cwe)

myfun <- function(data) {
  area <- deparse(substitute(data))
  stringr::str_extract(string = area, pattern = "(?<=_)[:alpha:]+$")
}
myfun(dat)

library(dygraphs)
dygraph(data = mono_data$mono_be, main = "Monotone des flux FR - BE") %>%
  dySeries(name = names(mono_data$mono_be)[2], label = "Flux BE") %>%
  dyHighlight(
    highlightCircleSize = 5,
    highlightSeriesOpts = list(strokeWidth = 2)
  )%>%
  dyAxis(
    name = "y", label = "",
    axisLabelFormatter = htmlwidgets::JS("function(d) {return d + ' MW';}")
  )%>%
  dyAxis(
    name = "x", label = "% de sc\u00e9narios", valueRange = c(0, 100), rangePad = 5,
    axisLabelFormatter = htmlwidgets::JS("function(d) {return d + '%';}")
  )


dyHighlight(highlightSeriesOpts = list(strokeWidth = 3))


# ----

nb_MC <- 2040
date_i <- "2018-02-24"
date_debut <- as.POSIXct(paste0(date_i," 00:00:00"), format="%Y-%m-%d %H:%M:%S", tz="UTC")
new_time <- data.table(DateTime=as.POSIXct(format(seq(date_debut, by=("+1 hour"), length.out = 168)),
                                           format="%Y-%m-%d %H:%M:%S", tz="UTC"))
pays <- "fr"
links_fr <- getLinks(areas="fr", exclude = c("lac","pump_d", "turb_d","pump_w", "turb_w" ))

getAreas()
#Extraire les données de sortie d'ANTARES pour chaque pays
data_pays <- readAntares(areas = "fr",
                         links = links_fr,
                         select = c("FLOW LIN."),
                         mcYears = "all",
                         linkCapacity = TRUE)


# areas <- readAntares(areas = "fr") # equivalent to readAntares(areas="all")
# links <- readAntares(links="ch - fr")



# pays_area <- data_pays$areas
pays_links <- data_pays$links
pays_links <- pays_links[order(mcYear),]
pays_links$time <- new_time$DateTime
#
# saveRDS(pays_links, file="~/Marges_Hebdo/xml_hydro/pays_links.Rdata")
#
#
# pays_links <- readRDS("news/pays_links.Rdata")

flux <- pays_links[, c("time", "mcYear", "link","FLOW LIN.")]



#########################""

# flux_be <- dcast.data.table(flux[link == "be - fr",], time ~ mcYear, value.var = "FLOW LIN.")
# #flux_be$time <- new_time$DateTime
#
#   pays_links[link == "be - fr",]
#
# names(flux_be)
#
# pal_couleurs <- c(rep("blue", nb_MC))
#
# graph_margin <- dygraph(flux_be, paste0("Flux ")) %>%
#   dyRangeSelector() %>%
#   dyLegend(show="always") %>%
#   dyHighlight(highlightCircleSize = 3)%>%
#   dyLegend(show = "always")%>%
#   dyCSS(css="css_dygraph1.txt")%>%
#   dyOptions(colors = c(pal_couleurs)) %>%
#   dyOptions(useDataTimezone = TRUE)  %>%
#   dyAxis("y", label = "MW")
#
# graph_margin

#####################
#Pour la monotone

date_etude <- as.POSIXct("2018-03-01 17:00:00", format="%Y-%m-%d %H:%M:%S", tz="UTC")


flux_etude <- flux[time == date_etude, c("link", "mcYear","FLOW LIN.")]
flux_etude <- dcast.data.table(flux_etude, mcYear ~ link, value.var = "FLOW LIN.")
names(flux_etude)
num_row <- round((1:nrow(flux_etude))/nrow(flux_etude)*100,1)

### BELGIQUE
be_flux <- apply(flux_etude[,c("be - fr")], 1, sum)*-1
be_flux <- be_flux[order(be_flux, decreasing = TRUE)]

mono_be <- as.data.table(cbind(num_row,be_flux))


### ALLEMAGNE
de_flux <- apply(flux_etude[,c("de - fr")], 1, sum)*-1
de_flux <- de_flux[order(de_flux, decreasing = TRUE)]

mono_de <- as.data.table(cbind(num_row,de_flux))

### CWE
cwe_flux <- apply(flux_etude[,c("be - fr","de - fr")], 1, sum)*-1
cwe_flux <- cwe_flux[order(cwe_flux, decreasing = TRUE)]

mono_cwe <- as.data.table(cbind(num_row,cwe_flux))
