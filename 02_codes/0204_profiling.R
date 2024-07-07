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

###################

sex1 <- read_excel("03_outputs/0302_tables/sex1.xlsx")

vis_er <- 
  sex1%>%
  mutate(name = as.factor(name),
         group = as.factor(group))

var <- c("dodgerblue4", "skyblue3")

a1 <- ggplot(vis_er, aes(x= name, y = value, fill=group)) + 
  geom_bar(stat="identity",
           position = "dodge")+
  coord_flip()+
  stat_summary(fun = mean, geom = "text", col = "black",     # Add text to plot
               vjust = 0.5, position = position_dodge(width= 0.45), 
               aes(label = paste("",round(..y.., digits = 2))))+
  scale_fill_manual(values = var)+
  theme_bw()+
  xlab("")+
  ylab("Средний балл по методике")+
  theme(legend.title = element_blank())

sex2 <- read_excel("03_outputs/0302_tables/sex2.xlsx")

vis_er2 <- 
  sex2%>%
  mutate(name = as.factor(name),
         group = as.factor(group))

a2 <- ggplot(vis_er2, aes(x= name, y = value, fill=group)) + 
  geom_bar(stat="identity",
           position = "dodge")+
  coord_flip()+
  stat_summary(fun = mean, geom = "text", col = "black",     # Add text to plot
               vjust = 0.5, position = position_dodge(width= 0.45), 
               aes(label = paste("",round(..y.., digits = 2))))+
  scale_fill_manual(values = var)+
  theme_bw()+
  xlab("")+
  ylab("Средний балл по методике")+
  theme(legend.title = element_blank())

sex3 <- read_excel("03_outputs/0302_tables/sex3.xlsx")

vis_er3 <- 
  sex3%>%
  mutate(name = as.factor(name),
         group = as.factor(group))

a3 <- ggplot(vis_er3, aes(x= name, y = value, fill=group)) + 
  geom_bar(stat="identity",
           position = "dodge")+
  coord_flip()+
  stat_summary(fun = mean, geom = "text", col = "black",     # Add text to plot
               vjust = 0.5, position = position_dodge(width= 0.45), 
               aes(label = paste("",round(..y.., digits = 2))))+
  scale_fill_manual(values = var)+
  theme_bw()+
  xlab("")+
  ylab("Средний балл по методике")+
  theme(legend.title = element_blank())

sex4 <- read_excel("03_outputs/0302_tables/sex4.xlsx")

vis_er4 <- 
  sex4%>%
  mutate(name = as.factor(name),
         group = as.factor(group))

a4 <- ggplot(vis_er4, aes(x= name, y = value, fill=group)) + 
  geom_bar(stat="identity",
           position = "dodge")+
  coord_flip()+
  stat_summary(fun = mean, geom = "text", col = "black",     # Add text to plot
               vjust = 0.5, position = position_dodge(width= 0.45), 
               aes(label = paste("",round(..y.., digits = 2))))+
  scale_fill_manual(values = var)+
  theme_bw()+
  xlab("")+
  ylab("Средний балл по методике")+
  theme(legend.title = element_blank())

sex5 <- read_excel("03_outputs/0302_tables/sex5.xlsx")

vis_er5 <- 
  sex5%>%
  mutate(name = as.factor(name),
         group = as.factor(group))

a5 <- ggplot(vis_er5, aes(x= name, y = value, fill=group)) + 
  geom_bar(stat="identity",
           position = "dodge")+
  coord_flip()+
  stat_summary(fun = mean, geom = "text", col = "black",     # Add text to plot
               vjust = 0.5, position = position_dodge(width= 0.45), 
               aes(label = paste("",round(..y.., digits = 2))))+
  scale_fill_manual(values = var)+
  theme_bw()+
  xlab("")+
  ylab("Средний балл по методике")+
  theme(legend.title = element_blank())


figure1 <- ggarrange(a1, a2,  
                     labels = c("", "", ""),
                     ncol = 1, nrow = 2)

figure2 <- ggarrange( a4, a5, 
                     labels = c("", "", ""),
                     ncol = 1, nrow = 2)
########

female <- 
  data_with_cl %>%
  filter(sex == 0) %>%
  mutate(type = as.factor(type))

summary(female$type)

male <- 
  data_with_cl %>%
  filter(sex == 1) %>%
  mutate(type = as.factor(type))

summary(male$type)

#############
cl1 <- 
  data_with_cl %>%
  filter(cluster == 1) %>%
  mutate(type = as.factor(type))

summary(cl1$type)
summary(cl1$sex)

cl2 <- 
  data_with_cl %>%
  filter(cluster == 2) %>%
  mutate(type = as.factor(type))

summary(cl2$type)
summary(cl2$sex)

############################################

cl1 <- read_excel("03_outputs/0302_tables/cl1.xlsx")

cl_er <- 
  cl1%>%
  mutate(name = as.factor(name),
         group = as.factor(group))

var <- c("dodgerblue4", "skyblue3")

ggplot(cl_er, aes(x= name, y = value, fill=group)) + 
  geom_bar(stat="identity",
           position = "dodge")+
  coord_flip()+
  stat_summary(fun = mean, geom = "text", col = "black",     # Add text to plot
               vjust = 0.5, position = position_dodge(width= 0.45), 
               aes(label = paste("",round(..y.., digits = 2))))+
  scale_fill_manual(values = var)+
  theme_bw()+
  xlab("")+
  ylab("Средний балл по методике")+
  theme(legend.title = element_blank())


cl2 <- read_excel("03_outputs/0302_tables/cl2.xlsx")

cl_er2 <- 
  cl2%>%
  mutate(name = as.factor(name),
         group = as.factor(group))

ggplot(cl_er2, aes(x= name, y = value, fill=group)) + 
  geom_bar(stat="identity",
           position = "dodge")+
  coord_flip()+
  stat_summary(fun = mean, geom = "text", col = "black",     # Add text to plot
               vjust = 0.5, position = position_dodge(width= 0.45), 
               aes(label = paste("",round(..y.., digits = 2))))+
  scale_fill_manual(values = var)+
  theme_bw()+
  xlab("")+
  ylab("Средний балл по методике")+
  theme(legend.title = element_blank())


cl3 <- read_excel("03_outputs/0302_tables/cl3.xlsx")

cl_er3 <- 
  cl3%>%
  mutate(name = as.factor(name),
         group = as.factor(group))

ggplot(cl_er3, aes(x= name, y = value, fill=group)) + 
  geom_bar(stat="identity",
           position = "dodge")+
  coord_flip()+
  stat_summary(fun = mean, geom = "text", col = "black",     # Add text to plot
               vjust = 0.5, position = position_dodge(width= 0.45), 
               aes(label = paste("",round(..y.., digits = 2))))+
  scale_fill_manual(values = var)+
  theme_bw()+
  xlab("")+
  ylab("Средний балл по методике")+
  theme(legend.title = element_blank())

cl4 <- read_excel("03_outputs/0302_tables/cl4.xlsx")

cl_er4 <- 
  cl4%>%
  mutate(name = as.factor(name),
         group = as.factor(group))

ggplot(cl_er4, aes(x= name, y = value, fill=group)) + 
  geom_bar(stat="identity",
           position = "dodge")+
  coord_flip()+
  stat_summary(fun = mean, geom = "text", col = "black",     # Add text to plot
               vjust = 0.5, position = position_dodge(width= 0.45), 
               aes(label = paste("",round(..y.., digits = 2))))+
  scale_fill_manual(values = var)+
  theme_bw()+
  xlab("")+
  ylab("Средний балл по методике")+
  theme(legend.title = element_blank())
