#-------------------------------------------------------------------
# Project: 
# Organization: 
# Objective: logistic regression
# Author:  Valeria Egorova
# Date: 03 July 2024
#-------------------------------------------------------------------

data_with_cl <- read_excel("03_outputs/0301_data/data_with_cl.xlsx")

#-----------------Модель 1-----------------------#
mod3 <- lm(exp_y ~	em_CISS + 	distr_CISS +
            	abil +	bescom +	mstit +	neter, 
          data = data_with_cl)
summary(mod3)

durbinWatsonTest(mod3)

autoplot(mod3) +
  theme_bw()

vif(mod3)

data_for_lolli1 <- read_excel("03_outputs/0302_tables/data_for_lolli1.xlsx")

ggplot(data_for_lolli1, aes(x=name, y=value)) +
  geom_segment( aes(x=name, xend=name, y=0, yend=value), color="black") +
  geom_point( color="#152e48", size=4, alpha=1) +
  geom_label(aes(name, value+0,5, label = signif(value,2)), 
             colour = "black", nudge_x = 0.35, size = 4) +
  coord_flip() + 
  theme_bw() +
  labs(y= "Коэффициенты", x="") +
  ylim(-3.5,3.5) +
  theme(axis.text.y = element_text(color = "black"),
        axis.text.x = element_text(color = "black")) +
  scale_x_discrete(labels=c("Бескомпромиссность", "Мстительность",
                            "Нетерпимость
                            к мнению
                            других",
                            "Отвлечение",
                            "Способность 
                            к социальному 
                            самоконтролю",
                            "ЭОК"))

#-------------------------------Модель 2--------------------------------------#

data2 <- 
  data_with_cl %>%
  mutate(int_coping = task_CISS + em_CISS + av_CISS + distr_CISS + sdistr_CISS)

mod4 <- lm(int_coping ~		abil   +	lich_t + type + exp_y + sex, 
           data = data2)
summary(mod4)

durbinWatsonTest(mod4)
autoplot(mod4)
vif(mod4)

data_for_lolli2 <- read_excel("03_outputs/0302_tables/data_for_lolli2.xlsx")

ggplot(data_for_lolli2, aes(x=name, y=value)) +
  geom_segment( aes(x=name, xend=name, y=0, yend=value), color="black") +
  geom_point( color="#152e48", size=4, alpha=1) +
  geom_label(aes(name, value+0,5, label = signif(value,2)), 
             colour = "black", nudge_x = 0.35, size = 4) +
  coord_flip() + 
  theme_bw() +
  labs(y= "Коэффициенты", x="") +
  ylim(-1,4) +
  theme(axis.text.y = element_text(color = "black"),
        axis.text.x = element_text(color = "black")) +
  scale_x_discrete(labels=c( "Личностная
                             тревожность", "Способность 
                            к социальному 
                            самоконтролю",
                             "Тип стрессоустойчивости:
                             Склонность к типу А",
                             "Тип стрессоустойчивости:
                             Склонность к типу Б",
                             "Тип стрессоустойчивости:
                             Тип А"))
    