library(dplyr)
library(tidyverse)
library(ggplot2)
library(stats)
library(stylo)
library(qgraph)
library(factoextra)


set.seed(123)
######## Prepare Data ########
df <- read.csv('../audio_features_w_genre_clean.csv') %>%
  select(c('hit','danceability', 'energy', 'key', 'loudness','mode',
           'speechiness', 'acousticness', 'instrumentalness',
           'liveness', 'valence', 'tempo', 'duration_ms', 'time_signature')) %>%
  drop_na()
  

hit <- df$hit
df <- select(df, -c('hit')) %>%
  scale() %>%
  as_tibble()
head(df)
str(df)

########  K-means ########

#### Optimal K ####
fviz_nbclust(
  df, 
  kmeans, 
  k.max = 6,
  method = 'wss',
  diss = get_dist(df, method = 'euclidean')
  ) + geom_vline(xintercept = 4, linetype = 2)

fviz_nbclust(df, kmeans, method='silhouette', k.max=6)

# WSS returns k = 4, Silhouette returns k = 2.
# k = 2 is reasonable because with pre-knowledge of the data, we know there are 2 groups - hit and non-hit songs.
# k = 4 also makes sense, since the clustering might be based on the type of songs with common audio features.
# k = 3 also seems to be an elbow, as the slope becomes slightly flatter.

#### k = 2 ####
km2 <- kmeans(df, 2, nstart = 24)
d2 <- cbind(label = hit, cluster = km2$cluster)
head(d2)

# number of points in each cluster
table(km2$cluster)
##     1     2 
##    5797 13429 
## this is aligned with the data which contains around 3300 hit songs.

# accuracy (only applies when k = 2)
mean((km2$cluster) == -hit+2)
## 0.6180173

# visualization
options(ggrepel.max.overlaps = Inf)
fviz_cluster(km2, df,
             ellipse.type = 'euclid',
             star.plot = TRUE,
             repel = TRUE,
             geom = 'point',
             ggtheme = theme_minimal())


#### k = 3 ####
km3 <- kmeans(df, 3, nstart = 24)

# number of points in each cluster
table(km3$cluster)
##    1     2     3 
##   5416 12951   859 

# visualization
fviz_cluster(km3, df,
             ellipse.type = 'euclid',
             star.plot = TRUE,
             repel = TRUE,
             geom = 'point',
             ggtheme = theme_minimal())

#### k = 4 ####
km4 <- kmeans(df, 4, nstart = 24)

# number of points in each cluster
table(km4$cluster)

# visualization
fviz_cluster(km4, df,
             ellipse.type = 'euclid',
             star.plot = TRUE,
             repel = TRUE,
             geom = 'point',
             ggtheme = theme_minimal())

#### Prediction ####
pred_data <- read.csv('../Cluster_audio_features_pred_data.csv') %>%
  select(c('danceability', 'energy', 'key', 'loudness','mode',
           'speechiness', 'acousticness', 'instrumentalness',
           'liveness', 'valence', 'tempo', 'duration_ms', 'time_signature')) %>%
  scale() %>%
  as_tibble() %>%
  mutate(time_signature = as.numeric(c(0.172655957076637,0.172655957076637,0.172655957076637)))
head(pred_data)
pred_name <- c('Intro (Hate On Me)',
               'Flocky Flocky (feat. Travis Scott)',
               'Way 2 Sexy (witih Future & Young Thug)')
# k = 2
test_pred2 <- predict(km2, pred_data)
table(test_pred2, pred_name)
# k = 3
test_pred3 <- predict(km3, pred_data)
table(test_pred3, pred_name)


########  Hierarchical Clustering ########  
#### distance matrices ####
df_sample <- sample_n(df, 50)
euc <- dist(df_sample, method = 'euclidean')
man <- dist(df_sample, method = 'manhattan')

m <- as.matrix(df_sample)
cos_sim <- m / sqrt(rowSums(m * m))
cos_sim <- cos_sim %*% t(cos_sim)
cos_dist <- as.dist(1 - cos_sim)

# visualization
qgraph(1/as.matrix(euc), layout='spring', vsize=3)
qgraph(1/as.matrix(man), layout='spring', vsize=3)
qgraph(1/as.matrix(cos_dist), layout='spring', vsize=3)

#### hierarchical clustering with Euclidean distance #### 
hc_euc <- hclust(d = euc, method = 'ward.D2')

fviz_dend(hc_euc,
          cex = 0.6)

fviz_dend(hc_euc, k = 2,
          cex = 0.5,
          k_colors = c('#2E9FDF','#FC4E07'),#'00AFBB',
          color_labels_by_k = TRUE,
          rect = TRUE)

fviz_dend(hc_euc, k = 3,
          cex = 0.5,
          k_colors = c('#2E9FDF','#FC4E07','#E7B800'),
          color_labels_by_k = TRUE,
          rect = TRUE)

fviz_dend(hc_euc, k = 4,
          cex = 0.5,
          k_colors = c('#2E9FDF','#00AFBB','#E7B800','#FC4E07'),
          color_labels_by_k = TRUE,
          rect = TRUE)

fviz_dend(hc_euc, k = 5,
          cex = 0.5,
          k_colors = c('#2E9FDF','#00AFBB','#E7B800','#FC4E07','#8877bb'),
          color_labels_by_k = TRUE,
          rect = TRUE)

#### hierarchical clustering with Manhattan distance ####
hc_man <- hclust(d = man, method = 'ward.D2')

fviz_dend(hc_man,
          cex = 0.6)

fviz_dend(hc_man, k = 2,
          cex = 0.5,
          k_colors = c('#2E9FDF','#FC4E07'),#'00AFBB',
          color_labels_by_k = TRUE,
          rect = TRUE)

fviz_dend(hc_man, k = 3,
          cex = 0.5,
          k_colors = c('#2E9FDF','#FC4E07','#E7B800'),
          color_labels_by_k = TRUE,
          rect = TRUE)

fviz_dend(hc_man, k = 4,
          cex = 0.5,
          k_colors = c('#2E9FDF','#00AFBB','#E7B800','#FC4E07'),
          color_labels_by_k = TRUE,
          rect = TRUE)

fviz_dend(hc_man, k = 5,
          cex = 0.5,
          k_colors = c('#2E9FDF','#00AFBB','#E7B800','#FC4E07','#8877bb'),
          color_labels_by_k = TRUE,
          rect = TRUE)

#### hierarchical clustering with Cosine Similarity ####
hc_cos <- hclust(d = cos_dist, method = 'ward.D2')

fviz_dend(hc_cos,
          cex = 0.6)

fviz_dend(hc_cos, k = 2,
          cex = 0.5,
          k_colors = c('#2E9FDF','#FC4E07'),#'00AFBB',
          color_labels_by_k = TRUE,
          rect = TRUE)

fviz_dend(hc_cos, k = 3,
          cex = 0.5,
          k_colors = c('#2E9FDF','#FC4E07','#E7B800'),
          color_labels_by_k = TRUE,
          rect = TRUE)

fviz_dend(hc_cos, k = 4,
          cex = 0.5,
          k_colors = c('#2E9FDF','#00AFBB','#E7B800','#FC4E07'),
          color_labels_by_k = TRUE,
          rect = TRUE)

fviz_dend(hc_cos, k = 5,
          cex = 0.5,
          k_colors = c('#2E9FDF','#00AFBB','#E7B800','#FC4E07','#8877bb'),
          color_labels_by_k = TRUE,
          rect = TRUE)
