#-------------------------------------------------------------------
# Project: 
# Organization: 
# Objective: Computing cluster analysis
# Author: Valeria Egorova
# Date: 03 July 2024
#-------------------------------------------------------------------

library (factoextra)
library (cluster)

data_clust <- 
  data %>%
  select(exp_y) %>%
  filter(exp_y < 53)

exp <- density(data_clust$exp_y)
plot(exp)


fviz_nbclust(data_clust, kmeans, method = "wss") 
#calculate gap statistic based on number of clusters
gap_stat <- clusGap(data_clust,
                    FUN = kmeans,
                    nstart = 25,
                    K.max = 10,
                    B = 50)

#plot number of clusters vs. gap statistic
fviz_gap_stat(gap_stat) 


km <- kmeans(data_clust, centers = 2, nstart = 25)
km$size

data_with_cl <- 
  data %>%
  filter(exp_y < 53) %>%
  bind_cols(km$cluster) %>%
  mutate(cluster = `...27`) %>%
  select(-`...27`)

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
