library(factoextra)
library(ggplot2)
library(stats)
library(stylo)
library(qgraph)

setwd('/Users/kristallqiu/Desktop/501/portfolio')
df <- read.csv('spotify_hitsong_full_1009.csv')#[16000:17000,]
df <- df[!(rowSums(is.na(df))),]
head(df)

audio_features <- df[, c('danceability', 
                         'energy',
                         'key',
                         'loudness',
                         'speechiness',
                         'acousticness',
                         'instrumentalness',
                         'liveness',
                         'valence',
                         'tempo')]

audio_features <- scale(audio_features)
head(audio_features)

# K-means
## optimal k
fviz_nbclust(
  audio_features, 
  kmeans, 
  k.max = 6,
  method = "wss",
  diss = get_dist(audio_features, method = "euclidean")
  ) + geom_vline(xintercept = 4, linetype = 2)

fviz_nbclust(audio_features, kmeans, method='silhouette', k.max=6)

# WSS returns k = 4, Silhouette returns k = 2.
# k = 2 is reasonable because with pre-knowledge of the data, we know there are 2 groups - hit and non-hit songs.
# k = 4 also makes sense, since the clustering might be based on the type of songs with common audio features.
# However, in the elbow method of determining optimal k,
# k = 3 also seems to be an elbow, as the slope becomes flatter.

set.seed(123)

## k = 2
km_2 <- kmeans(audio_features, 2, nstart = 24)
d_2 <- cbind(df, cluster = km_2$cluster)
head(d_2)

### number of points in each cluster
table(km_2$cluster)
# 1-13399 2-4233 this is aligned with the data which contains around 3300 hit songs.

### Visualization
options(ggrepel.max.overlaps = Inf)

fviz_cluster(km_2, audio_features,
             ellipse.type = 'euclid',
             star.plot = TRUE,
             repel = TRUE,
             geom = 'point',
             ggtheme = theme_minimal())

### Accuracy (only applies when k = 2)
mean((km_2$cluster - 1) == df$hit)

## k = 3
km_3 <- kmeans(audio_features, 3, nstart = 24)
d_3 <- cbind(df, cluster = km_3$cluster)
head(d_3)

### number of points in each cluster
table(km_3$cluster)

### Visualization
fviz_cluster(km_3, audio_features,
             ellipse.type = 'euclid',
             star.plot = TRUE,
             repel = TRUE,
             geom = 'point',
             ggtheme = theme_minimal())

## k = 4
km_4 <- kmeans(audio_features, 4, nstart = 24)
d_4 <- cbind(df, cluster = km_4$cluster)
head(d_4)

### number of points in each cluster
table(km_4$cluster)

### Visualization
fviz_cluster(km_4, audio_features,
             ellipse.type = 'euclid',
             star.plot = TRUE,
             repel = TRUE,
             geom = 'point',
             ggtheme = theme_minimal())

## Predict on new data
predict_data <- read.csv('assign3_predict_data.csv')
head(predict_data)
predict_audio_features <- scale(predict_data[, c('danceability', 
                                           'energy',
                                           'key',
                                           'loudness',
                                           'speechiness',
                                           'acousticness',
                                           'instrumentalness',
                                           'liveness',
                                           'valence',
                                           'tempo')])

### k = 2
test_preds_2 <- predict(km_2, predict_audio_features)
table(test_preds_2, predict_data$name)

### k = 3
test_preds_3 <- predict(km_3, predict_audio_features)
table(test_preds_3, predict_data$name)

# Hierarchical clustering
## Distance matrices
euc <- dist(audio_features, method = 'euclidean')
man <- dist(audio_features, method = 'manhattan')

m <- as.matrix(audio_features)
cos_sim <- m / sqrt(rowSums(m * m))
cos_sim <- cos_sim %*% t(cos_sim)
cos_dist <- as.dist(1 - cos_sim)

### Visualization
#qgraph(1/as.matrix(euc), layout='spring', vsize=3)
#qgraph(1/as.matrix(man), layout='spring', vsize=3)
#qgraph(1/as.matrix(cos_sim), layout='spring', vsize=3)

## Hierarchical clustering with Euclidean distance
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

## Hierarchical clustering with Manhattan distance
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

## Hierarchical clustering with Cosine Similarity
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
