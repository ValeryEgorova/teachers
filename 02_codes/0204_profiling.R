#-------------------------------------------------------------------
# Project: 
# Organization: кафедра психологии управления и юридической психологии, АПП ЮФУ
# Objective: Сравнение средних в разбивке по полу и кластерам
# Author: Валерия Егорова
# Date: 03 Июля 2024
#-------------------------------------------------------------------

#---------------------------Сравнение средних в разбивке по полу------------------#
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
            lich_t = round(mean(lich_t, na.rm = T), 2),
            int_coping = round(mean(int_coping, na.rm = T), 2)) %>%
  as.data.frame() %>%
  write_xlsx(file.path(outTables,"profile_sex.xlsx"))

var2 <- c("exp_y", "task_CISS",	"em_CISS", "av_CISS",	"distr_CISS",
          "sdistr_CISS",	"stress",	"abil",	"vpyl",	"nast",	"obid",	"neust",
          "bescom",	"mstit",	"neter",	"podoz",	"poza",	"nega",	"confl",	"sit_tr",	"lich_t",
          "int_coping")

lapply(data_with_cl[var2], function(x) t.test(x ~ data_with_cl$sex))

#--------------------------Визуализация--------------------------------#

sex1 <- read_excel("03_outputs/0302_tables/sex1.xlsx")

vis_er <- 
  sex1%>%
  mutate(name = as.factor(name),
         group = as.factor(group))

var <- c("#53acf0", "#3977a9")

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
  ylab("")+
  theme(legend.title = element_blank(),
        legend.position = c(0.8, 0.8),
        axis.text = element_text(color = "black")) +
  ylim(-0.25, 0.25)  +
  scale_x_discrete(labels=c('Способность 
                            к социальному 
                            самоконтролю',
                            'Стрессоустойчивость'))

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
  ylab("")+
  theme(legend.title = element_blank(),
        legend.position = "none",
        axis.text = element_text(color = "black")) +
  ylim(-0.40, 0.40)  +
  scale_x_discrete(labels=c('Личностная
                            тревожность',
                            'Ситуативная 
                            тревожность'))

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
  theme(legend.title = element_blank(),
        legend.position = "none",
        axis.text = element_text(color = "black")) +
  ylim(0, 45) +
  scale_x_discrete(labels=c('Возраст', 
                            'Стаж 
                            (в годах)'))

ggarrange(a1, a2, a5, 
          labels = c("", "", ""),
          ncol = 1, nrow = 3)

sex3 <- read_excel("03_outputs/0302_tables/sex3.xlsx")

vis_er3 <- 
  sex3%>%
  mutate(name = as.factor(name),
         group = as.factor(group))

a3 <- ggplot(vis_er3, aes(x= name, y = value, fill=group)) + 
  geom_bar(stat="identity",
           position = "dodge")+
  coord_flip()+
  stat_summary(fun = mean, geom = "text", col = "black",
               vjust = 0.5, position = position_dodge(width= 0.45), 
               aes(label = paste("",round(..y.., digits = 2))))+
  scale_fill_manual(values = var)+
  theme_bw()+
  xlab("")+
  ylab("Средний балл по методике")+
  theme(legend.title = element_blank(),
        legend.position = "none",
        axis.text = element_text(color = "black")) +
  scale_x_discrete(labels=c('Бескомпромиссность', 'Вспыльчивость', 'Конфликтность',
                            'Мстительность', 'Наступательность', 'НА', 'Нетерпимость 
                            к мнению других', 'Неуступчивость', 'Обидчивость', 
                            'Подозрительность', 'ПА'))

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
  ylab("")+
  theme(legend.title = element_blank(),
        legend.position = c(0.85, 0.85),
        axis.text = element_text(color = "black")) +
  scale_x_discrete(labels=c( 'Избегание', 'ОПК', 'Отвлечение', 'ПОК',
                             'Социальное
                            отвлечение', 'ЭОК')) +
  ylim(-0.40, 0.40)

ggarrange(a4, a3,
          labels = c("", "", ""),
          ncol = 1, nrow = 2)

#------------Частотный анализ в разбивке по полу-----------------------------------#


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


#---------------------------Сравнение средних в разбивке по кластерам------------------#
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
            lich_t = round(mean(lich_t, na.rm = T), 2),
            int_coping = round(mean(int_coping, na.rm = T), 2)) %>%
  as.data.frame() %>%
  write_xlsx(file.path(outTables,"profile_clust.xlsx"))

var3 <- c("task_CISS",	"em_CISS", "av_CISS",	"distr_CISS",
          "sdistr_CISS",	"stress",	"abil",	"vpyl",	"nast",	"obid",	"neust",
          "bescom",	"mstit",	"neter",	"podoz",	"poza",	"nega",	"confl",	"sit_tr",	"lich_t",
          "int_coping")

lapply(data_with_cl[var3], function(x) t.test(x ~ data_with_cl$cluster))

#--------------------------Визуализация--------------------------------#

cl1 <- read_excel("03_outputs/0302_tables/cl1.xlsx")

cl_er <- 
  cl1%>%
  mutate(name = as.factor(name),
         group = as.factor(group))


clp1 <- ggplot(cl_er, aes(x= name, y = value, fill=group)) +
  geom_bar(stat="identity", position = "dodge") +
  coord_flip() +
  stat_summary(fun = mean, geom = "text", col = "black",    
               vjust = 0.5, position = position_dodge(width= 0.45), 
               aes(label = paste("",round(..y.., digits = 2)))) +
  scale_fill_manual(values = var) +
  theme_bw() +
  xlab("") +
  ylab("") +
  theme(legend.title = element_blank(),
        legend.position = c(0.8, 0.8),
        axis.text = element_text(color = "black")) +
  ylim(-0.20, 0.20) +
  scale_x_discrete(labels=c('Способность 
                            к социальному 
                            самоконтролю',
                            'Стрессоустойчивость'))

cl2 <- read_excel("03_outputs/0302_tables/cl2.xlsx")

cl_er2 <- 
  cl2%>%
  mutate(name = as.factor(name),
         group = as.factor(group))

clp2 <- ggplot(cl_er2, aes(x= name, y = value, fill=group)) + 
  geom_bar(stat="identity", position = "dodge") +
  coord_flip() +
  stat_summary(fun = mean, geom = "text", col = "black",    
               vjust = 0.5, position = position_dodge(width= 0.45), 
               aes(label = paste("",round(..y.., digits = 2)))) +
  scale_fill_manual(values = var) +
  theme_bw() +
  xlab("") +
  ylab("") +
  theme(legend.title = element_blank(),
        legend.position = "none",
        axis.text = element_text(color = "black")) +
  ylim(-0.20, 0.20) +
  scale_x_discrete(labels=c('Личностная
                            тревожность',
                            'Ситуативная 
                            тревожность'))
cl4 <- read_excel("03_outputs/0302_tables/cl4.xlsx")

cl_er4 <- 
  cl4%>%
  mutate(name = as.factor(name),
         group = as.factor(group))

clp4 <- ggplot(cl_er4, aes(x= name, y = value, fill=group)) + 
  geom_bar(stat="identity", position = "dodge") +
  coord_flip() +
  stat_summary(fun = mean, geom = "text", col = "black",    
               vjust = 0.5, position = position_dodge(width= 0.45), 
               aes(label = paste("",round(..y.., digits = 2)))) +
  scale_fill_manual(values = var) +
  theme_bw() +
  xlab("") +
  ylab("Средний балл по методике") +
  theme(legend.title = element_blank(),
        legend.position = "none",
        axis.text = element_text(color = "black")) +
  ylim(-0.20, 0.20) + 
  scale_x_discrete(labels=c( 'Избегание', 'ОПК', 'Отвлечение', 'ПОК',
                             'Социальное
                            отвлечение', 'ЭОК'))

ggarrange(clp1, clp2, clp4,
          labels = c("", "", ""),
          ncol = 1, nrow = 3)


cl3 <- read_excel("03_outputs/0302_tables/cl3.xlsx")

cl_er3 <- 
  cl3%>%
  mutate(name = as.factor(name),
         group = as.factor(group))

clp3 <- ggplot(cl_er3, aes(x= name, y = value, fill=group)) + 
  geom_bar(stat="identity", position = "dodge") +
  coord_flip() +
  stat_summary(fun = mean, geom = "text", col = "black",    
               vjust = 0.5, position = position_dodge(width= 0.45), 
               aes(label = paste("",round(..y.., digits = 2)))) +
  scale_fill_manual(values = var) +
  theme_bw() +
  xlab("") +
  ylab("") +
  theme(legend.title = element_blank(),
        legend.position = c(0.85, 0.85),
        axis.text = element_text(color = "black")) +
  ylim(-0.20, 0.20) +
  scale_x_discrete(labels=c('Бескомпромиссность', 'Вспыльчивость', 'Конфликтность',
                            'Мстительность', 'Наступательность', 'НА', 'Нетерпимость 
                            к мнению других', 'Неуступчивость', 'Обидчивость', 
                            'Подозрительность', 'ПА'))

#------------Частотный анализ в разбивке по кластерам-----------------------------------#

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