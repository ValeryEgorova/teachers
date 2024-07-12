#-------------------------------------------------------------------
# Project: 
# Organization: 
# Objective: logistic regression
# Author:  Valeria Egorova
# Date: 03 July 2024
#-------------------------------------------------------------------
data_with_cl <- read_excel("03_outputs/0301_data/data_with_cl.xlsx")



#############
mod <- lm(exp_y ~  sex + 	em_CISS + 	distr_CISS +	abil +	 bescom +	mstit +	neter, 
           data = data_with_cl)
summary(mod)

durbinWatsonTest(mod)
autoplot(mod)
vif(mod)

ggplot(data_with_cl, aes(x=exp_m, y=mstit)) + 
  geom_point()

############
mod2 <- lm(exp_y ~ 	em_CISS + distr_CISS +	abil  + type, 
          data = data_with_cl)
summary(mod2)

durbinWatsonTest(mod2)
autoplot(mod2)
vif(mod2)


###########
mod3 <- lm(exp_y ~	em_CISS + 	distr_CISS +
            	abil +	bescom +	mstit +	neter, 
          data = data_with_cl)
summary(mod3)

durbinWatsonTest(mod3)
autoplot(mod3)
vif(mod3)

######################
data_for_glm <- 
  data_with_cl %>%
  mutate(cluster = ifelse(cluster == 1, 1, 0))


gmod <- glm(cluster ~  	em_CISS + av_CISS  +  bescom +	mstit +	neter, 
           data = data_for_glm, 
           family = binomial(link = "logit"), x = TRUE)
summary(gmod)
maBina(gmod)
vif(gmod)
PseudoR2(gmod, which = "all")

dat <- 
  data_for_glm %>%
  select(cluster, em_CISS, av_CISS,  bescom,	mstit,	neter) %>%
  drop_na()

predicts <- as.numeric(gmod$fitted.values >= 0.5)
confusionMatrix(as.factor(predicts), as.factor(dat$cluster))

to_check <- 
  data_for_glm %>%
  select(cluster, em_CISS, av_CISS,  bescom,	mstit,	neter)

corr_matrix1 <- cor(to_check)
p.mat1 = cor_pmat(to_check)
ggcorrplot(corr_matrix1,  type = "lower",
           lab = TRUE, p.mat = p.mat1, tl.cex = 10, lab_size = 2.5)

#####################################################
gmod1 <- glm(cluster ~	em_CISS  +	distr_CISS, 
            data = data_for_glm, 
            family = binomial(link = "logit"), x = TRUE)
summary(gmod1)
maBina(gmod1)
vif(gmod1)
PseudoR2(gmod1, which = "all")

dat <- 
  data_for_glm %>%
  select(cluster, em_CISS, distr_CISS) %>%
  drop_na()

predicts <- as.numeric(gmod1$fitted.values >= 0.5)
confusionMatrix(as.factor(predicts), as.factor(dat$cluster))

to_check2 <- 
  data_for_glm %>%
  select(cluster, em_CISS, distr_CISS)

corr_matrix2 <- cor(to_check2)
p.mat2 = cor_pmat(to_check2)
ggcorrplot(corr_matrix2,  type = "lower",
           lab = TRUE, p.mat = p.mat2, tl.cex = 10, lab_size = 2.5)


######################

data2 <- 
  data_with_cl %>%
  mutate(int_coping = task_CISS + em_CISS + av_CISS + distr_CISS + sdistr_CISS)

mod4 <- lm(int_coping ~		abil   +	lich_t + type + exp_y + sex, 
           data = data2)
summary(mod4)

durbinWatsonTest(mod4)
autoplot(mod4)
vif(mod4)









#task_CISS +	em_CISS + av_CISS +	distr_CISS +
#  sdistr_CISS +	stress +	abil +	vpyl +	nast +	obid +	neust +
#  bescom +	mstit +	neter +	podoz +	poza +	nega +	confl +	sit_tr +	lich_t + type