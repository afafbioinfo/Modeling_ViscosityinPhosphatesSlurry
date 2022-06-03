library(dplyr)    # for general data wrangling needs
library(gbm)      # for original implementation of regular and stochastic GBMs
library(caret)
library(ggplot2)a
library(randomForest)
library(rsq)

All <- read.csv(file = 'threevarforV.csv')
Allfitred<-All[All$X.SR.>26.6,]#12 observations are left out!
All<-Allfitred
set.seed(64)
sample_size = floor(0.75*nrow(All))
picked = sample(seq_len(nrow(All)),size = sample_size)
trainset =All[picked,]
testset =All[-picked,]



## Training number of trees while fixing the mtry value

set.seed(64)
trControl <- trainControl(method = 'repeatedcv',
                          number = 5,
                          repeats = 6,
                          search = 'grid')

#create tunegrid , mtry equals to the square root of the number of variables
mtry <- sqrt(ncol(trainset))

tunegrid <- expand.grid(.mtry =2)# optimal value
modellist <- list()
for (ntree in c(50,100,200,1000,2000,3000,4000,5000)) {
  
  fit <- train(X.V.~.,
               data = trainset,
               method = "rf",
               tuneGrid = tunegrid,
               trControl = trControl,
               importance = TRUE,
               ntree = ntree)
  
  key <- toString(ntree)
  modellist[[key]] <- fit
}

#Compare results
results <- resamples(modellist)
summary(results)     
dotplot(results)

########200 is the optimal number
set.seed(64)
#RFmodel<-randomForest(S2 ~ ., data=train, importance=TRUE,ntree=5000,mtry=1,
#                      proximity=TRUE, do.trace=FALSE)

RFmodel<-randomForest(X.V. ~ ., data=trainset, importance=TRUE,ntree=3000,mtry=2,
                      proximity=TRUE, do.trace=FALSE)
TrainingPredicted <-predict(RFmodel, newdata = trainset)

#facteur<-( max(trainset$S2) +min(trainset$S2))/2
#print(cor.test((train$S2 +1)*facteur,(TrainingPredicted +1)*facteur,
#        method = "pearson")$estimate )

print(cor.test(trainset$X.V. ,TrainingPredicted ,
               method = "pearson") )

#data.frame(train$S2.,TrainingPredicted)
#plot(train$S2.,TrainingPredicted)

### Puis sur le test
testPredicted <- predict(RFmodel, newdata = testset)## cette fis-ci on fait appel au données du test

### On calcule désormais la correlation entre les données de test observées et celles prédites avec notre modèle RFmodel
#facteur<-( max(testset$S2) +min(testset$S2))/2
#print(cor.test((test$S2 +1)*1000*facteur,(testPredicted +1)*facteur,
#             method = "pearson")$estimate )
#print(cor.test((test$S2 +1)*facteur,(testPredicted +1)*facteur,
#              method = "pearson")$estimate )
print(cor.test(testset$X.V. ,testPredicted,
               method = "pearson") )


rsq(trainset$X.V.,TrainingPredicted)
rsq(testset$X.V.,testPredicted)

#data.frame(test$S2.,testPredicted)
#plot(test$S2,testPredicted)
write.csv(data.frame(trainset$X.V,TrainingPredicted),"Train_3V.csv", row.names = TRUE)

write.csv(data.frame(testset$X.V,testPredicted),"Validation_3V.csv", row.names = TRUE)

caret::varImp(RFmodel, scale=T)
