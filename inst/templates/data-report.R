{{empty}}
#  ------------------------------------------------------------------------
#
# Title : Preparation data marges
#    By : Rte
#  Date : {{date_report}}
#
#  ------------------------------------------------------------------------





# Packages ----------------------------------------------------------------

library( antaresRead )
library( antaresWeeklyMargin )




# Simulation path ---------------------------------------------------------

opts <- setSimulationPath(path = "{{simPath}}")





# Marges ------------------------------------------------------------------

marges_up <- compute_margins(date = "{{date_start}}", area = "fr", margin = "upward")

saveRDS(object = marges_up, file = "{{path}}/datas/marges_up.rds")


marges_down  <- compute_margins(date =  "{{date_start}}", area = "fr", margin = "downward", formula_cnes = TRUE)

saveRDS(object = marges_down, file = "{{path}}/datas/marges_down.rds")



# Marges data all ---------------------------------------------------------

marges_all <- compute_all_margins(date = "{{date_start}}", mcYear = {{year_mc}})

saveRDS(object = marges_all, file = "{{path}}/datas/marges_all.rds")



# Monotones ---------------------------------------------------------------

mono <- compute_mono(start = "{{date_start}}", date = "{{date_study}}")

saveRDS(object = mono, file = "{{path}}/datas/mono.rds")




# Creation rapport --------------------------------------------------------

rmarkdown::render("{{path}}/index.Rmd", params = list(
  week = {{week}}, # Week for upward margins
  n_scenario = {{n_scenario}}, # Number of scenario
  year_mc = {{year_mc}}, # MC year studied
  date_study = "{{date_study}}",
  date_debut = "{{date_start}}"
))








