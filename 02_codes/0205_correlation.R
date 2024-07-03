#-------------------------------------------------------------------
# Project: 
# Organization: 
# Objective: Correlation
# Author: Valeria Egorova
# Date: 03 July 2024
#-------------------------------------------------------------------

cor_m <- 
  data_with_cl %>%
  select(age, exp_y, task_CISS,	em_CISS, av_CISS,	distr_CISS,
         sdistr_CISS,	stress,	abil,	vpyl,	nast,	obid,	neust,
         bescom,	mstit,	neter,	podoz,	poza,	nega,	confl,	sit_tr,	lich_t, cluster)

corr_matrix2 <- cor(cor_m)
p.mat2 = cor_pmat(cor_m)
ggcorrplot(corr_matrix2,  type = "lower",
           lab = TRUE, p.mat = p.mat2, tl.cex = 10, lab_size = 2.5)
