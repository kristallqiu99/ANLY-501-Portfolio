library(dplyr)
library(tidyverse)
library(ggplot2)
library(e1071)
library(caret)
library(cvms)

setwd('Desktop/501/portfolio')
set.seed(1009)
df <- read.csv('audio_features_w_genre_clean.csv', stringsAsFactors = F) %>% 
  dplyr::select(-c('name','artists','id','release_date', 'artist_genres')) %>% 
  mutate(regular=case_when(season == 'regular' ~1, TRUE ~0),
         summer=case_when(season == 'summer' ~1, TRUE ~0),
         holiday=case_when(season == 'holiday' ~1, TRUE ~0),
         hit=as.factor(hit)) %>%
  dplyr::select(-c('season'))
str(df)
head(df)

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

df_train <- train_test_split(df)
df_test <- train_test_split(df, train = F)
test_label <- df_test$hit
df_test <- dplyr::select(df_test, -c('hit'))

# smaller set for tuning
df_sample <- sample_n(df, 8000)

######## SVM ########
## polynomial
poly_tune <-  tune(svm, hit~., data=df_sample, kernel='polynomial',
                     range=list(degree=c(3,4,5),cost=c(0.01, 0.1, 1, 10, 100)))
summary(poly_tune)

best_poly <- e1071::svm(hit~., data=df_train, kernel='polynomial',
                         degree=4, cost=10)
pred_poly <- predict(best_poly, df_test)
conf_matrix_vis <- function(pred, true_label){
  return(plot_confusion_matrix(
    as_tibble(confusionMatrix(pred, true_label, positive = '1')$table),
    target_col = 'Reference', 
    prediction_col = "Prediction",
    counts_col = 'n',
    palette = 'Greens')
  )
}
caret::confusionMatrix(pred_poly, test_label, positive = '1')
conf_matrix_vis(pred_poly, test_label)
plot(best_poly, df_train, danceability~energy)

## radial
rbf_tune <- tune(svm, hit~., data=df_sample, kernel='radial',
                 range=list(gamma=c(0.1,0.5,1,2,3,4),cost=c(0.01, 0.1, 1, 10, 100)))
summary(rbf_tune)

best_rbf <- svm(hit~., data=df_train, kernel='radial',
                 gamma=0.1,cost=10)
pred_rbf <- predict(best_rbf, df_test)
caret::confusionMatrix(pred_rbf, test_label, positive = '1')
conf_matrix_vis(pred_rbf, test_label)
plot(best_rbf, df_train, danceability~energy)
## sigmoid
sigm_tune <- tune(svm, hit~., data=df_sample, kernel='sigmoid',
                 range=list(gamma=c(0.1,0.5,1,2,3,4),cost=c(0.01, 0.1, 1, 10, 100)))
summary(sigm_tune)

best_sigm <- svm(hit~., data=df_train, kernel='sigmoid',
                gamma=0.1,cost=0.01)
pred_sigm <- predict(best_sigm, df_test)
caret::confusionMatrix(pred_sigm, test_label, positive = '1')
conf_matrix_vis(pred_sigm, test_label)
plot(best_sigm, df_train, danceability~energy)