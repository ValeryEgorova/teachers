#-------------------------------------------------------------------
# Project: 
# Organization: 
# Objective: Profiling 
# Author: Valeria Egorova
# Date: 03 July 2024
#-------------------------------------------------------------------

profile_sex <- 
  data_with_cl %>%
  group_by(sex) %>%
  summarise(exp_y = round(mean(exp_y, na.rm = T), 2),
            age = round(mean(age, na.rm = T), 2), 
            task_CISS = round(mean(task_CISS, na.rm = T), 2), 
            em_CISS = round(mean(em_CISS, na.rm = T), 2), 
            av_CISS = round(mean(av_CISS, na.rm = T), 2), 
            distr_CISS = round(mean(distr_CISS, na.rm = T), 2), 
            sdistr_CISS = round(mean(sdistr_CISS, na.rm = T), 2), 
            stress = round(mean(stress, na.rm = T), 2), 
            abil = round(mean(abil, na.rm = T), 2), 
            vpyl = round(mean(vpyl, na.rm = T), 2), 
            nast = round(mean(nast, na.rm = T), 2), 
            obid = round(mean(obid, na.rm = T), 2), 
            neust = round(mean(neust, na.rm = T), 2), 
            bescom = round(mean(bescom, na.rm = T), 2), 
            mstit = round(mean(mstit, na.rm = T), 2), 
            neter = round(mean(neter, na.rm = T), 2), 
            podoz = round(mean(podoz, na.rm = T), 2), 
            poza = round(mean(poza, na.rm = T), 2), 
            nega = round(mean(nega, na.rm = T), 2), 
            confl = round(mean(confl, na.rm = T), 2), 
            sit_tr = round(mean(sit_tr, na.rm = T), 2), 
            lich_t = round(mean(lich_t, na.rm = T), 2)) %>%
  as.data.frame() %>%
  write_xlsx(file.path(outTables,"profile_sex.xlsx"))

profile_clust <- 
  data_with_cl %>%
  group_by(cluster) %>%
  summarise(age = round(mean(age, na.rm = T), 2), 
            task_CISS = round(mean(task_CISS, na.rm = T), 2), 
            em_CISS = round(mean(em_CISS, na.rm = T), 2), 
            av_CISS = round(mean(av_CISS, na.rm = T), 2), 
            distr_CISS = round(mean(distr_CISS, na.rm = T), 2), 
            sdistr_CISS = round(mean(sdistr_CISS, na.rm = T), 2), 
            stress = round(mean(stress, na.rm = T), 2), 
            abil = round(mean(abil, na.rm = T), 2), 
            vpyl = round(mean(vpyl, na.rm = T), 2), 
            nast = round(mean(nast, na.rm = T), 2), 
            obid = round(mean(obid, na.rm = T), 2), 
            neust = round(mean(neust, na.rm = T), 2), 
            bescom = round(mean(bescom, na.rm = T), 2), 
            mstit = round(mean(mstit, na.rm = T), 2), 
            neter = round(mean(neter, na.rm = T), 2), 
            podoz = round(mean(podoz, na.rm = T), 2), 
            poza = round(mean(poza, na.rm = T), 2), 
            nega = round(mean(nega, na.rm = T), 2), 
            confl = round(mean(confl, na.rm = T), 2), 
            sit_tr = round(mean(sit_tr, na.rm = T), 2), 
            lich_t = round(mean(lich_t, na.rm = T), 2)) %>%
  as.data.frame() %>%
  write_xlsx(file.path(outTables,"profile_clust.xlsx"))

var2 <- c("exp_y", "task_CISS",	"em_CISS", "av_CISS",	"distr_CISS",
         "sdistr_CISS",	"stress",	"abil",	"vpyl",	"nast",	"obid",	"neust",
         "bescom",	"mstit",	"neter",	"podoz",	"poza",	"nega",	"confl",	"sit_tr",	"lich_t")

lapply(data_with_cl[var2], function(x) t.test(x ~ data_with_cl$sex))


var3 <- c("task_CISS",	"em_CISS", "av_CISS",	"distr_CISS",
          "sdistr_CISS",	"stress",	"abil",	"vpyl",	"nast",	"obid",	"neust",
          "bescom",	"mstit",	"neter",	"podoz",	"poza",	"nega",	"confl",	"sit_tr",	"lich_t")

lapply(data_with_cl[var3], function(x) t.test(x ~ data_with_cl$cluster))