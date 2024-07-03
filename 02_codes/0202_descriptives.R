#-------------------------------------------------------------------
# Project: 
# Organization: 
# Objective: Computing descriptives
# Author: Valeria Egorova
# Date: 03 July 2024
#-------------------------------------------------------------------

var <- c("age", "exp_y", "task_CISS",	"em_CISS", "av_CISS",	"distr_CISS",
         "sdistr_CISS",	"stress",	"abil",	"vpyl",	"nast",	"obid",	"neust",
         "bescom",	"mstit",	"neter",	"podoz",	"poza",	"nega",	"confl",	"sit_tr",	"lich_t", "exp_m")

desc <- bind_rows(lapply(data[var], function(x) psych::describe(x))) %>%
  select(min, mean, median, sd, max, skew, kurtosis) %>%
  mutate(across(everything(), ~round(.x, digits = 2))) 

summary(data$sex)

write_xlsx(desc, file.path(outTables,"descriptives.xlsx"))




