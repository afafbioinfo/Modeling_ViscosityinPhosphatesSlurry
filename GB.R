
library(dplyr)    # for general data wrangling needs
library(gbm)      # for original implementation of regular and stochastic GBMs
library(lares)
library(caret)
library(ggplot2)

All <- read.csv(file = 'threevarforV.csv')
Allfitred<-All[All$X.SR.>26.6,]#12 observations are left out!
All<-Allfitred
set.seed(64)
sample_size = floor(0.75*nrow(All))
picked = sample(seq_len(nrow(All)),size = sample_size)
train =All[picked,]
test =All[-picked,]


plot(All$X.V)
plot(All$X.TS.)
plot(All$X.GC.)
hist(All$X.GC.)
boxplot(All$X.GC.)
hist(Allfitred$X.GC.)
plot(All$X.T.) 

plot(train$X.V)
plot(train$X.TS.)
plot(train$X.T.)
plot(train$X.GC.)

plot(test$X.V)
plot(test$X.TS.)
plot(test$X.T.)
plot(test$X.GC.)
set.seed(64)




###################### find the optimal learning rate
# create grid search
hyper_grid <- expand.grid(
  learning_rate = c(0.3, 0.1, 0.05, 0.01, 0.005),
  RMSE = NA,
  trees = NA,
  time = NA
)

# execute grid search
for(i in seq_len(nrow(hyper_grid))) {
  
  # fit gbm
  set.seed(64)  # for reproducibility
  train_time <- system.time({
    m <- gbm(
      formula = X.V. ~ .,
      data = train,
      distribution = "gaussian",
      n.trees = 1000, 
      shrinkage = hyper_grid$learning_rate[i], 
      interaction.depth = 3, 
      n.minobsinnode = 10,
      cv.folds = 5
    )
  })
  
  # add SSE, trees, and training time to results
  hyper_grid$RMSE[i]  <- sqrt(min(m$cv.error))
  hyper_grid$trees[i] <- which.min(m$cv.error)
  hyper_grid$Time[i]  <- train_time[["elapsed"]]
  
}

# results
arrange(hyper_grid, RMSE)


# create model fit function
model_fit <- function(n.trees, shrinkage, interaction.depth, n.minobsinnode) {
  
  m <- gbm(
    formula = X.V. ~ .,
    data = train,
    distribution = "gaussian",
    n.trees = n.trees,
    shrinkage = shrinkage,
    interaction.depth = interaction.depth,
    n.minobsinnode = n.minobsinnode,
    cv.folds = 5
  )
  # compute RMSE
  sqrt(min(m$cv.error))
}

set.seed(64)
# search grid
hyper_grid <- expand.grid(
  n.trees = 345,#143,####### optimal
  shrinkage = 0.3,####### optimal
  interaction.depth = c(2,3,4, 5,6, 7,8,9,10,11),
  n.minobsinnode = c(2,3,4,5,7,9, 10,12, 15,20)
)

# perform search grid with functional programming
hyper_grid$rmse <- purrr::pmap_dbl(
  hyper_grid,
  ~ model_fit(
    n.trees = ..1,
    shrinkage = ..2,
    interaction.depth = ..3,
    n.minobsinnode = ..4
  )
)

# results
arrange(hyper_grid, rmse)

# for reproducibility
set.seed(64)

ames_gbm1 <- gbm(
  formula = X.V. ~ .,
  data = train,
  #distribution = "gaussian",  # SSE "loss function
  n.trees =920,#266,#143, #161 avec75%,
  shrinkage = 0.3, #0.3 avec75%,
  interaction.depth =7,#6,# 5, avec75%,
  n.minobsinnode = 8,#5,
  cv.folds = 5
)
ames_gbm1
best <- which.min(ames_gbm1$cv.error)
best

# get MSE and compute RMSE
sqrt(ames_gbm1$cv.error[best])
print(summary(ames_gbm1)) #Summary gives a table of Variable Importance and a plot of Variable Importance



print(i)

TrainingPredicted <-predict(ames_gbm1, newdata = train)
print(cor.test(train$X.V.,TrainingPredicted,
               method = "pearson"))

par(pty="s")
plot(train$X.V.,TrainingPredicted,xlim=c(0,35),ylim=c(0,35))
abline(coef = c(0,1))
### Puis sur le test
testPredicted <- predict(ames_gbm1, newdata = test)## cette fis-ci on fait appel au données du test

### On calcule désormais la correlation entre les données de test observées et celles prédites avec notre modèle RFmodel
print(cor.test(test$X.V.,testPredicted,
               method = "pearson"))

par(pty="s")
plot(test$X.V.,testPredicted,xlim=c(0,35),ylim=c(0,35))
abline(coef = c(0,1))

plot(ames_gbm1,i="X.TS.") 
plot(ames_gbm1,i="X.T.")
plot(ames_gbm1,i="X.GC.")
write.csv(data.frame(train$X.V.,TrainingPredicted),"99_99perf_train.csv", row.names = TRUE)

write.csv(data.frame(test$X.V.,testPredicted),"99_99perf_test.csv", row.names = TRUE)

rsq(train$X.V.,TrainingPredicted)
rsq(test$X.V.,testPredicted)

> rsq(train$X.V.,TrainingPredicted)
[1] 0.9965
> rsq(test$X.V.,testPredicted)
[1] 0.9913

75% training and 25% validation arbitrary assigned using sample fct in R.
468 observations

> rsq(train$X.V.,TrainingPredicted)
[1] 0.9708
> rsq(test$X.V.,testPredicted)
[1] 0.9557

456 observations

rsq(train$X.V.,TrainingPredicted)
[1] 0.9965
> rsq(test$X.V.,testPredicted)
[1] 0.9913