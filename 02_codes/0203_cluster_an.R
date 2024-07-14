#-------------------------------------------------------------------
# Project: 
# Organization: 
# Objective: Computing cluster analysis
# Author: Valeria Egorova
# Date: 03 July 2024
#-------------------------------------------------------------------

data_clust <- 
  data %>%
  select(exp_y) %>%
  filter(exp_y < 53)

exp_p <- ggplot(data_clust, aes(y = exp_y)) +
  geom_density(alpha = 0.60, fill = "#3c7cb0") + 
  coord_flip() +
  theme_bw() +
  xlab("")+
  ylab("Стаж педагогической деятельности (в годах)")+
  theme(axis.text = element_text(color = "black"),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        plot.title = element_text(color="black", size=12, face="plain")) +
  ggtitle("График распределения переменной стажа педагогической деятельности") 
  

fviz_nbclust(data_clust, kmeans, method = "wss") 

gap_stat <- clusGap(data_clust,
                    FUN = kmeans,
                    nstart = 25,
                    K.max = 10,
                    B = 50)

exp_gap <- fviz_gap_stat(gap_stat, linecolor = "#3c7cb0") +
  theme_bw() + 
  theme(axis.text = element_text(color = "black"),
        plot.title = element_text(color="black", size=12, face="plain")) +
  ylab("Статистика разрыва") +
  xlab("Количество кластеров") +
  ggtitle("График зависимости кластеров от статистики разрыва") 

fig_k <- ggarrange(exp_p, exp_gap,
                  labels = c("", "", ""),
                  ncol = 1, nrow = 2)

km <- kmeans(data_clust, centers = 2, nstart = 25)
km$size

data_with_cl <- 
  data %>%
  filter(exp_y < 53) %>%
  bind_cols(km$cluster) %>%
  mutate(cluster = `...28`) %>%
  select(-`...28`)

saveRDS(data_with_cl, file.path(outData,"data_with_cl.rds"))

#----------------------Computing descriptive statistics for clusters ----------------------------#


clust1 <- 
  data_with_cl %>%
  filter(cluster == 1) %>%
  mutate(exp_y = as.numeric(exp_y))

cl1 <- psych::describe(clust1$exp_y)

clust2 <- 
  data_with_cl %>%
  filter(cluster == 2) %>%
  mutate(exp_y = as.numeric(exp_y))

cl2 <-   psych::describe(clust2$exp_y)

descr_cl <- 
  cl1 %>%
  bind_rows(cl2) %>%
  select(min, mean, median, sd, max, skew, kurtosis) %>%
  mutate(across(everything(), ~round(.x, digits = 2)))

write_xlsx(descr_cl, file.path(outTables,"descriptives_by_clusters.xlsx"))
