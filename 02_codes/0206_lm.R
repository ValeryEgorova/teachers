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


#---------------Модель 2---------------------------------------------------------#

data2 <- 
  data_with_cl %>%
  mutate(int_coping = task_CISS + em_CISS + av_CISS + distr_CISS + sdistr_CISS)

mod4 <- lm(int_coping ~		abil   +	lich_t + type + exp_y + sex, 
           data = data2)
summary(mod4)

durbinWatsonTest(mod4)
autoplot(mod4)
vif(mod4)

