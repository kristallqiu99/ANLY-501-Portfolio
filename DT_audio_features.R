getwd()
setwd('/Users/kristallqiu/Desktop/501/portfolio')

#install.packages('rpart')
library(rpart)
library(rpart.plot)
#install.packages('rattle')
library(rattle)
library(dplyr)
library(tidyverse)
library(ggplot2)
#install.packages('caret')
library(caret)
#install.packages('cvms')
library(cvms)

######## Prepare data######## 
audio.features <- read.csv('audio_features_w_genre.csv', stringsAsFactors = T) %>% 
  select(-c('name','artists','id','release_date', 'artist_genres')) %>%
  mutate(hit = factor(hit, levels = c(0, 1), labels = c('Non-hit', 'Hit')))
str(audio.features)
head(audio.features)

######## Split the data######## 
set.seed(1009)

train_test_split <- function(data, size = 0.8, train = TRUE) {
  total_row <- size * nrow(data)
  train_sample <- sample(nrow(data),
                          total_row, replace = F)
  if (train == TRUE) {
    return (data[train_sample, ])
  } else {
    return (data[-train_sample, ])
  }
}

train.data <- train_test_split(audio.features, size = 0.8, train = T)
table(train.data$hit)
str(train.data)
test.data <- train_test_split(audio.features, size = 0.8, train = F)
table(test.data$hit)

######## Decision tree######## 
#### all properties ####
# grow tree
dt1 <- rpart(hit ~ ., data = train.data,
             method = 'class', 
             control = rpart.control(minsplit=2, minbucket=1, cp=0.002))
plotcp(dt1)
png(file='DTPlot/AudioFeaturesDT1.png', width = 1500, height = 1200)
fancyRpartPlot(dt1, sub = NULL)
dev.off()
# feature importance
feature_importance <- function(model){
  feature.importance <- data.frame(importance = model$variable.importance) %>% 
    tibble::rownames_to_column() %>% 
    dplyr::rename('variable' = rowname) %>% 
    dplyr::arrange(importance) %>%
    dplyr::mutate(variable = forcats::fct_inorder(variable))
  
  return (ggplot2::ggplot(feature.importance) +
    geom_col(aes(x = variable, y = importance),
             col = '#1DB954', show.legend = F, fill = '#1DB954') +
    coord_flip() +
    theme_bw())
}
feature_importance(dt1)
# prediction
pred <- predict(dt1, select(test.data, -hit), type='class')
caret:: confusionMatrix(pred, test.data$hit, positive = 'Hit')

conf_matrix_vis <- function(cm){
  return(plot_confusion_matrix(
    as.tibble(cm$table),
    target_col = 'Prediction', 
    prediction_col = "Reference",
    counts_col = 'n',
    palette = 'Greens')
    )
}
conf_matrix_vis(confusionMatrix(pred, test.data$hit, positive = 'Hit'))

#### key musical properties ####
# split data
train.data2 <- select(train.data, 
                      c('hit','danceability',
                        'energy','key','loudness', 'speechiness','acousticness',
                        'instrumentalness','liveness','valence','tempo'))
test.data2 <- select(test.data, 
                     c('hit','danceability',
                       'energy','key','loudness', 'speechiness','acousticness',
                       'instrumentalness','liveness','valence','tempo'))
# grow tree
dt2 <- rpart(hit ~ ., 
             data = train.data2,
             method = 'class', 
             control = rpart.control(minsplit=2, minbucket=1, cp=0.002))
plotcp(dt2)
png(file='DTPlot/AudioFeaturesDT2.png', width = 1500, height = 1200)
fancyRpartPlot(dt2, sub = NULL)
dev.off()
# feature importance
feature_importance(dt2)
# prediction
pred2 <- predict(dt2, select(test.data2, -hit), type='class')
confusionMatrix(pred2, test.data2$hit, positive = 'Hit')
conf_matrix_vis(confusionMatrix(pred2, test.data2$hit, positive = 'Hit'))

#### simpler tree ####
# grow tree
dt3 <- rpart(hit ~ ., 
             data = train.data2,
             method = 'class', 
             control = rpart.control(minsplit=2, minbucket=1, cp=0.005))
plotcp(dt3)
png(file='DTPlot/AudioFeaturesDT3.png', width = 1200, height = 700)
fancyRpartPlot(dt3, sub = NULL)
dev.off()
# prediction
pred3 <- predict(dt3, select(test.data2, -hit), type='class')
confusionMatrix(pred3, test.data2$hit, positive = 'Hit')
conf_matrix_vis(confusionMatrix(pred3, test.data2$hit, positive = 'Hit'))

#### covid era 2020 - now ####
# prepare data
covid.era <- read.csv('audio_features_w_genre.csv') %>%
  subset(as.Date.character(release_date)>='2020-01-01') %>%
  select(c('hit','danceability','energy','key','loudness','speechiness',
           'acousticness', 'instrumentalness','liveness','valence','tempo')) %>%
  mutate(hit = factor(hit, levels = c(0, 1), labels = c('Non-hit', 'Hit')))
str(covid.era)
# split data
train.data4 <- train_test_split(covid.era, size = 0.8, train = T)
table(train.data4$hit)

test.data4 <- train_test_split(covid.era, size = 0.8, train = F)
table(test.data4$hit)
# grow tree
dt4 <- rpart(hit ~ ., 
             data = train.data4,
             method = 'class', 
             control = rpart.control(minsplit=2, minbucket=1, cp=0.01))
plotcp(dt4)
png(file='DTPlot/AudioFeaturesDT4covid.png', width = 1200, height = 700)
fancyRpartPlot(dt4, sub = NULL)
dev.off()
# feature importance
feature_importance(dt4)
# prediction
pred4 <- predict(dt4, select(test.data4, -hit), type='class')
confusionMatrix(pred4, test.data4$hit, positive = 'Hit')
conf_matrix_vis(confusionMatrix(pred4, test.data4$hit, positive = 'Hit'))