#-------------------------------------------------------------------
# Project: 
# Organization: 
# Objective: Prepare a data set for the analysis
# Author:  Валерия Егорова
# Date: 03 Июля 2024
#-------------------------------------------------------------------

# Read the script to upload necessary libraries
source(file.path(rcodes, "0200_load_packages.R"))

data <- read_excel("01_input_data/data.xlsx") %>%
  mutate(exp_y = as.numeric(exp_y),
         exp_m = exp_y*12,
         sex = as.factor(sex),
         type = case_when(stress <= 10 ~ "1. type B",
                          stress %in% c(11:20) ~ "2. close type B",
                          stress  %in% c(21:30) ~ "3. colse to type A",
                          stress %in% c(31:40) ~ "4.  type A"),
         int_coping = task_CISS + em_CISS + av_CISS + distr_CISS + sdistr_CISS) %>%
  drop_na() %>%
  mutate_at(c("task_CISS",	"em_CISS", "av_CISS",	"distr_CISS", "int_coping",
              "sdistr_CISS",	"stress",	"abil",	"vpyl",	"nast",	"obid",	"neust",
              "bescom",	"mstit",	"neter",	"podoz",	"poza",	"nega",	"confl",	"sit_tr",	"lich_t"),
            ~ scale(., center = T, scale = T)) %>%
  filter(age > 20)

write_dta(data, file.path(outData,"data.dta"))

