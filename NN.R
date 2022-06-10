All <- read.csv(file = 'threevarforV.csv')
Allfitred<-All[All$X.SR.>26.6,]#12 observations are left out!
All<-Allfitred
set.seed(64)
sample_size = floor(0.75*nrow(All))
picked = sample(seq_len(nrow(All)),size = sample_size)
train =All[picked,]
test =All[-picked,]


## Scale data for neural network
max = apply(All , 2 , max)
min = apply(All, 2 , min)
scaled = as.data.frame(scale(All, center = min, scale = max - min))
set.seed(64)
library(neuralnet)

# creating training and test set
trainNN = scaled[picked , ]
testNN = scaled[-picked , ]
n <- names(trainNN)
f <- as.formula(paste("X.V.~", paste(n[!n %in% "X.V."], collapse = " + ")))
set.seed(64)
#traçage
NN <- neuralnet(f, data=trainNN, hidden= 6,linear.output=T)
plot(NN)
library(NeuralNetTools )
olden(NN)$data
#traçage du l'erreur en fonction de N 
#x<-c(0.668342,0.412878,0.605979,0.391458,0.3826,0.442533)
y<-c(2:7)
x<-c(0.557087,0.535342,0.421296,0.377057,0.283967,0.310473)
df2<-data.frame(y,x)
p<-ggplot(df2, aes(x=y, y=x))+
  geom_line(aes(color="#E69F00"))+
  geom_point(aes(color="#E69F00")) 
p <- p + scale_color_brewer(palette="Dark2")+
  theme_minimal()

NN.matrix
plot(y,x,col="blue")

predict_testNN = neuralnet::compute(NN, testNN[,c(1:3)])
predict_testNN = (predict_testNN$net.result * (max(All$X.V.) - min(All$X.V.))) + min(All$X.V.)
plot(test$X.V., predict_testNN, col='red', pch=16, ylab = "Predicted V ", xlab = "Real V")
abline(0,1)

write.csv(data.frame(test$X.V.,predict_testNN),"NN.csv", row.names = TRUE)

predict_trainNN = neuralnet::compute(NN, trainNN[,c(1:3)])
predict_trainNN = (predict_trainNN$net.result * (max(All$X.V.) - min(All$X.V.))) + min(All$X.V.)
plot(train$X.V., predict_trainNN, col='red', pch=16, ylab = "Predicted V ", xlab = "Real V")
abline(0,1)

write.csv(data.frame(train$X.V.,predict_trainNN),"train_NN.csv", row.names = TRUE)

#Error
err.moy <- (sum(predict_testNN)-sum(test$X.V.))/456
print(err.moy)
#precision
precision <- sum(test$X.V.)/sum(predict_testNN)
print(precision)
#Correlation
cor(test$X.V.,predict_testNN)


# Calculate Root Mean Square Error (RMSE)
RMSE.NN = (sum((test$V - predict_testNN)^2) / nrow(test)) ^ 0.5
RMSE.NN



                   
ggplot(data = train, aes(x =predict_trainNN-train$X.V.)) +
  geom_histogram(fill = 'brown1', color = 'black') +
  labs(title = 'Histogram of Residuals in training', x = 'Residuals', y = 'Frequency')+
  theme_bw()+xlim(-7.5,3.5)


ggplot(data = train, aes(y =predict_trainNN,x=train$X.V.)) +
  geom_abline(color='grey') +
  geom_point( color = 'steelblue') +
  labs(title ="training", x = 'observed value', y = 'predicted value')+
  theme_bw()+xlim(5,40)+ylim(5,40)+ coord_fixed()


ggplot(data = test, aes(x =predict_testNN-test$X.V.)) +
  geom_histogram(fill = 'brown1', color = 'black') +
  labs(title = 'Histogram of Residuals in test', x = 'Residuals', y = 'Frequency')+
  theme_bw()+xlim(-7.5,3.5)

ggplot(data = test, aes(y =predict_testNN,x=test$X.V.)) +
  geom_abline(color='grey') +
  geom_point( color = 'steelblue') +
  labs(title ="test", x = 'observed value', y = 'predicted value')+
  theme_bw()+xlim(5,40)+ylim(5,40)+ coord_fixed()
