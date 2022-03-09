library(dplyr)
library(tidyverse)
library(ggplot2)
library(reshape2)
library(plyr)
library(e1071)
library(caret)
library(cvms)
library(klaR)

options(warn=-1)
set.seed(1009)

df <- read.csv('../audio_features_w_genre_clean.csv', stringsAsFactors = T) %>% 
  dplyr::select(-c('name','artists','id','release_date', 'artist_genres')) %>%
  mutate(hit = factor(hit, levels = c(0, 1), label=c('Non-hit', 'Hit')),
         mode=as.factor(mode),
         time_signature=as.factor(time_signature),
         season = as.factor(as.numeric(season))) # 1-holiday, 2-regular, 3-summer
str(df)
head(df)

######## Split the data######## 
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

df_train <- train_test_split(df)
df_test <- train_test_split(df, train = F)
test_label <- df_test$hit
df_test <- dplyr::select(df_test, -c('hit'))


######## Naive Bayes ########

## all variables
nb_grid <- expand.grid(usekernel=c(TRUE,FALSE),
                       fL=c(0, 0.5, 1), 
                       adjust=c(0, 0.5, 1))
nb1 <- caret::train(hit ~., data=df_train,
                    'nb', tuneGrid=nb_grid,
                    trControl=trainControl(method='cv', number=10))
nb1$finalModel$tuneValue
plot(nb1)
plot(NaiveBayes(hit~., data=df_train), legendplot = T)

### feature importance
plot(varImp(nb1), col='#1DB954')

### density plot
ggplot(data=df_train, aes(x=danceability, group=hit, fill=hit)) +
    geom_density(adjust=1.5, alpha=0.6) + 
    geom_vline(data=ddply(df_train, "hit", summarise, grp.mean=mean(danceability)), aes(xintercept=grp.mean, color=hit),
               linetype="dashed")

ggplot(data=df_train, aes(x=speechiness, group=hit, fill=hit)) +
  geom_density(adjust=1.5, alpha=0.6) + 
  geom_vline(data=ddply(df_train, "hit", summarise, grp.mean=mean(speechiness)), aes(xintercept=grp.mean, color=hit),
             linetype="dashed")

ggplot(data=df_train, aes(x=energy, group=hit, fill=hit)) +
  geom_density(adjust=1.5, alpha=0.6) + 
  geom_vline(data=ddply(df_train, "hit", summarise, grp.mean=mean(energy)), aes(xintercept=grp.mean, color=hit),
             linetype="dashed")

### prediction
pred1 <- predict(nb1, df_test)
conf_matrix_vis <- function(pred, true_label){
  return(plot_confusion_matrix(
    as.tibble(confusionMatrix(pred, true_label, positive = 'Hit')$table),
    target_col = 'Reference', 
    prediction_col = "Prediction",
    counts_col = 'n',
    palette = 'Greens')
  )
}
caret:: confusionMatrix(pred1, test_label, positive = 'Hit')
conf_matrix_vis(pred1, test_label)

## key variables
nb2 <- train(hit ~danceability+instrumentalness+speechiness+duration_ms+energy,
             data=df_train,'nb', tuneGrid=nb_grid,
             trControl=trainControl(method='cv', number=10))
nb2$finalModel$tuneValue
plot(nb2)
### feature importance
plot(varImp(nb2), col='#1DB954')
### prediction
pred2 <- predict(nb2, df_test)
confusionMatrix(pred2, test_label, positive = 'Hit')
conf_matrix_vis(pred2, test_label)                
