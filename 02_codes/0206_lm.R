#-------------------------------------------------------------------
# Project: 
# Organization: 
# Objective: logistic regression
# Author:  Valeria Egorova
# Date: 03 July 2024
#-------------------------------------------------------------------

#############
mod <- lm(exp_y ~  sex + 	em_CISS + 	distr_CISS +	abil +	 bescom +	mstit +	neter, 
           data = data_with_cl)
summary(mod)

############
mod2 <- lm(exp_y ~ 	em_CISS + distr_CISS +	abil  + type, 
          data = data_with_cl)
summary(mod2)

###########
mod3 <- lm(exp_y ~	em_CISS + 	distr_CISS +
            	abil +	bescom +	mstit +	neter, 
          data = data_with_cl)
summary(mod3)

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


#task_CISS +	em_CISS + av_CISS +	distr_CISS +
#  sdistr_CISS +	stress +	abil +	vpyl +	nast +	obid +	neust +
#  bescom +	mstit +	neter +	podoz +	poza +	nega +	confl +	sit_tr +	lich_t + type