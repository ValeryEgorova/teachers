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

#####################################################################

CISS <- c( "task_CISS",	"em_CISS", "av_CISS",	"distr_CISS",
          "sdistr_CISS")

names(data_with_cl[, CISS]) <- CISS

ridge_1 = 
  data_with_cl %>%
  select(all_of(CISS)) %>%
  rename_at(vars(CISS), ~ CISS) %>%
  gather(NCS, Value, CISS)%>%
  drop_na()

ggplot(ridge_1, aes(x = Value, y = NCS,  fill = 0.5 - abs(0.5 - stat(ecdf)))) +
  #geom_density(trim = TRUE)+
  stat_density_ridges(bandwidth=0.28, calc_ecdf = T, geom = "density_ridges_gradient", rel_min_height = 0.05)+
  scale_fill_viridis_c(name = "ECDF", direction = -1)+
  theme_ridges() + 
  theme(legend.position = "none")+
  xlab("")+
  ylab("")+
  scale_x_continuous(breaks = c(-3,-2,-1,0,1,2,3))

agr <- c(	"vpyl",	"nast",	"obid",	"neust",
         "bescom",	"mstit",	"neter",	"podoz",	"poza",	"nega",	"confl")

names(data_with_cl[, agr]) <- agr

ridge_2 = 
  data_with_cl %>%
  select(all_of(agr)) %>%
  rename_at(vars(agr), ~ agr) %>%
  gather(NCS, Value, agr)%>%
  drop_na()

ggplot(ridge_2, aes(x = Value, y = NCS,  fill = 0.5 - abs(0.5 - stat(ecdf)))) +
  #geom_density(trim = TRUE)+
  stat_density_ridges(bandwidth=0.28, calc_ecdf = T, geom = "density_ridges_gradient", rel_min_height = 0.05)+
  scale_fill_viridis_c(name = "ECDF", direction = -1)+
  theme_ridges() + 
  theme(legend.position = "none")+
  xlab("")+
  ylab("")+
  scale_x_continuous(breaks = c(-3,-2,-1,0,1,2,3))

tr <- c("sit_tr",	"lich_t")

names(data_with_cl[, tr]) <- tr

ridge_3 = 
  data_with_cl %>%
  select(all_of(tr)) %>%
  rename_at(vars(tr), ~ tr) %>%
  gather(NCS, Value, tr)%>%
  drop_na()

ggplot(ridge_3, aes(x = Value, y = NCS,  fill = 0.5 - abs(0.5 - stat(ecdf)))) +
  #geom_density(trim = TRUE)+
  stat_density_ridges(bandwidth=0.28, calc_ecdf = T, geom = "density_ridges_gradient", rel_min_height = 0.05)+
  scale_fill_viridis_c(name = "ECDF", direction = -1)+
  theme_ridges() + 
  theme(legend.position = "none")+
  xlab("")+
  ylab("")+
  scale_x_continuous(breaks = c(-3,-2,-1,0,1,2,3))


st <- c("stress","abil")

names(data_with_cl[, st]) <- st

ridge_4 = 
  data_with_cl %>%
  select(all_of(st)) %>%
  rename_at(vars(st), ~ st) %>%
  gather(NCS, Value, st)%>%
  drop_na()

ggplot(ridge_4, aes(x = Value, y = NCS,  fill = 0.5 - abs(0.5 - stat(ecdf)))) +
  #geom_density(trim = TRUE)+
  stat_density_ridges(bandwidth=0.28, calc_ecdf = T, geom = "density_ridges_gradient", rel_min_height = 0.05)+
  scale_fill_viridis_c(name = "ECDF", direction = -1)+
  theme_ridges() + 
  theme(legend.position = "none")+
  xlab("")+
  ylab("")+
  scale_x_continuous(breaks = c(-3,-2,-1,0,1,2,3))


