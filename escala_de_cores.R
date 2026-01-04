c("#6e3500", "#9a4a00", "#b07100", "#c69700", "#d7b500", "#e7d100", "#f9de00", "#fff500", "#f0f600", "#cde200", "#9dc800", "#77b400", "#4f9f00", "#1d8500", "#156200")
allobates_maxent
obj <- sdm::getVarImp(allobates_maxent,
               id = "ensemble",
               setting = list(method = 'weighted',
                              stat = 'auc')) %>%
  plot()


