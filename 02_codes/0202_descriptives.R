#-------------------------------------------------------------------
# Project: 
# Organization: 
# Objective: Computing descriptives
# Author: Valeria Egorova
# Date: 03 July 2024
#-------------------------------------------------------------------


#-------------------------Computing descriptive statistics------------------------------#

var <- c("age", "exp_y", "task_CISS",	"em_CISS", "av_CISS",	"distr_CISS", "int_coping", 
         "sdistr_CISS",	"stress",	"abil",	"vpyl",	"nast",	"obid",	"neust",
         "bescom",	"mstit",	"neter",	"podoz",	"poza",	"nega",	"confl",	
         "sit_tr",	"lich_t", "exp_m")

desc <- bind_rows(lapply(data[var], function(x) psych::describe(x))) %>%
  select(min, mean, median, sd, max, skew, kurtosis) %>%
  mutate(across(everything(), ~round(.x, digits = 2))) 

summary(data$sex)

write_xlsx(desc, file.path(outTables,"descriptives.xlsx"))

#-------------------------Visualizing descriptive statistics------------------------------#

CISS <- c( "task_CISS",	"em_CISS", "av_CISS",	"distr_CISS", "int_coping", 
          "sdistr_CISS")

names(data_with_cl[, CISS]) <- CISS

ridge_1 = 
  data_with_cl %>%
  select(all_of(CISS)) %>%
  rename_at(vars(CISS), ~ CISS) %>%
  gather(NCS, Value, CISS)%>%
  drop_na()

CISS_P <- ggplot(ridge_1, aes(x = Value, y = NCS,  fill = 0.5 - abs(0.5 - stat(ecdf)))) +
  stat_density_ridges(bandwidth=0.28, calc_ecdf = T, geom = "density_ridges_gradient", 
                      rel_min_height = 0.05)+
  theme_bw() + 
  theme(legend.position = "none")+
  xlab("")+
  ylab("")+
  scale_x_continuous(breaks = c(-3,-2,-1,0,1,2,3), limits = c(-3.5,3.5)) +
  theme(axis.text = element_text(color = "black")) +
  scale_y_discrete(labels=c( 'Избегание', 'Отвлечение', 'ЭОК', 'ОПК',
                             'Социальное
                            отвлечение','ПОК'))


agr <- c(	"vpyl",	"nast",	"obid",	"neust",
         "bescom",	"mstit",	"neter",	"podoz",	"poza",	"nega",	"confl")

names(data_with_cl[, agr]) <- agr

ridge_2 = 
  data_with_cl %>%
  select(all_of(agr)) %>%
  rename_at(vars(agr), ~ agr) %>%
  gather(NCS, Value, agr)%>%
  drop_na()

AGR_P <- ggplot(ridge_2, aes(x = Value, y = NCS,  fill = 0.5 - abs(0.5 - stat(ecdf)))) +
  stat_density_ridges(bandwidth=0.28, calc_ecdf = T, geom = "density_ridges_gradient", 
                      rel_min_height = 0.05)+
  theme_bw() + 
  theme(legend.position = "none")+
  xlab("")+
  ylab("")+
  scale_x_continuous(breaks = c(-3,-2,-1,0,1,2,3), limits = c(-3.5,3.5)) +
  theme(axis.text = element_text(color = "black")) + 
  scale_y_discrete(labels=c('Бескомпромиссность', 'Конфликтность', 'Мстительность', 
                            'Наступательность', 'НА', 'Нетерпимость 
                            к мнению других', 'Неуступчивость', 'Обидчивость',
                            'Подозрительность', 'ПА','Вспыльчивость'))

tr <- c("sit_tr",	"lich_t")

names(data_with_cl[, tr]) <- tr

ridge_3 = 
  data_with_cl %>%
  select(all_of(tr)) %>%
  rename_at(vars(tr), ~ tr) %>%
  gather(NCS, Value, tr)%>%
  drop_na()

TR_P <- ggplot(ridge_3, aes(x = Value, y = NCS,  fill = 0.5 - abs(0.5 - stat(ecdf)))) +
  stat_density_ridges(bandwidth=0.28, calc_ecdf = T, geom = "density_ridges_gradient", 
                      rel_min_height = 0.05)+
  theme_bw() + 
  theme(legend.position = "none")+
  xlab("")+
  ylab("")+
  scale_x_continuous(breaks = c(-3,-2,-1,0,1,2,3), limits = c(-3.5,3.5)) +
  theme(axis.text = element_text(color = "black")) +
  scale_y_discrete(labels=c('Личностная
                            тревожность',
                            'Ситуативная 
                            тревожность'))


st <- c("stress","abil")

names(data_with_cl[, st]) <- st

ridge_4 = 
  data_with_cl %>%
  select(all_of(st)) %>%
  rename_at(vars(st), ~ st) %>%
  gather(NCS, Value, st)%>%
  drop_na()

ST_P <- ggplot(ridge_4, aes(x = Value, y = NCS,  fill = 0.5 - abs(0.5 - stat(ecdf)))) +
  stat_density_ridges(bandwidth=0.28, calc_ecdf = T, geom = "density_ridges_gradient", 
                      rel_min_height = 0.05)+
  theme_bw() + 
  theme(legend.position = "none")+
  xlab("")+
  ylab("")+
  scale_x_continuous(breaks = c(-3,-2,-1,0,1,2,3), limits = c(-3.5,3.5)) +
  theme(axis.text = element_text(color = "black")) +
  scale_y_discrete(labels=c('Способность 
                            к социальному 
                            самоконтролю',
                            'Стрессоустойчивость'))

#-------------------------Merging all figures ------------------------------#

fig1 <- ggarrange(CISS_P,  TR_P,
                     labels = c("", "", ""),
                     ncol = 1, nrow = 2)


fig2 <- ggarrange(AGR_P, ST_P,
                  labels = c("", "", ""),
                  ncol = 1, nrow = 2)


