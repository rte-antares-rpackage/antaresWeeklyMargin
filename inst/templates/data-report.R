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

marges <- compute_margins(date = "{{date_margins}}", area = "fr", margin = "upward")

saveRDS(object = marges, file = "{{path}}/datas/marges.rds")




# Marges data all ---------------------------------------------------------

marges_all <- compute_all_margins(date = "{{date_margins}}", mcYear = 50)

saveRDS(object = marges_all, file = "{{path}}/datas/marges_all.rds")



# Monotones ---------------------------------------------------------------

mono <- compute_mono(start = "{{date_start}}", date = "{{date_mono}}")

saveRDS(object = mono, file = "{{path}}/datas/mono.rds")













