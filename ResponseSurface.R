library(rsm)
##A response surface is fitted using the R function "rsm" from the package rsm
##FO (first-order), TWI(two-way interaction), PQ (pure quadratic), SO( second-order)


##The analysis-of-variance table shown includes a breakdown of lack of fit and pure error, and
#we are also given information about the direction of steepest ascen

#SO, which is shorthand for a model with FO, TWI, and PQ terms
All <- read.csv(file = 'threevarforV.csv') #Multiple R-squared:  0.8234,	Adjusted R-squared:  0.8199 
Allfitred<-All[All$X.SR.>26.6,]#12 observations are left out!
All<-Allfitred#Multiple R-squared:  0.8742,	Adjusted R-squared:  0.8717 

hist(All$X.SR.,breaks=100)
plot(All[All$X.SR.==26.6,]$X.SR.,All[All$X.SR.==26.6,]$X.V.)
summary(All)
head(All)
library(plot3D)
library(scatterplot3d)
summary(All$X.V.)

#Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#5.602  12.454  17.305  18.440  23.950  40.066 

#Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#5.068  12.378  17.211  18.459  23.868  59.629 

scatter3D(x=All$X.T., y=All$X.SC.,z= All$X.V.)
All[All$X.SR.==26.6,]
All[All$X.SR.==26.6,]
library(dplyr)
All<-All %>% 
  mutate(ExT = exp(1/X.T.))
All<-All %>% 
  mutate(StrSC = as.factor(X.SC.))
All<-All %>% 
  mutate(InvT = 1/X.T.)
All<-All %>% 
  mutate(LogSR = log(X.SR.))
All<-All %>% 
  mutate(LogV = log(X.V.))
All<-All %>% 
  mutate(SC2 = X.SC.*X.SC.)
All<-All %>% 
  mutate(LogSR2 = log(X.SR.)*log(X.SR.))
All<-All %>% 
  mutate(T2 = X.T.*X.T.)

All<-All %>% 
  mutate(SCT = X.SC.*X.T.)
All<-All %>% 
  mutate(LogSRT = log(X.SR.)*X.T.)
All<-All %>% 
  mutate(SCLogSR = X.SC.*log(X.SR.))

summary(All$LogSR)
summary(All$X.SC.)
exp(2.82)
log(400)
log(600)
#normalize <- function(x) {
#  return ((x-min(x))/(max(x)-min(x)))
#}
##########After normalization
#All<-as.data.frame(lapply(All, normalize))

library(ggpubr)
head(All)
# Utiliser le box plot comme graphiques marginaux
ggscatterhist(
  All[All$X.T.==85,], x = "X.SR.", y = "X.V.",
  color = "StrSC", size = 3, alpha = 0.6,
  palette = c("#00AFBB", "#E7B800", "#FC4E07","#0073C2FF"),
  margin.plot = "boxplot",ylim=c(0,40),
  ggtheme = theme_bw()
)

head(All)

# Calculer une matrice de corrélation
my_data <- All[, c(1,2,7,8,11,12,13,14,15,16)]
head(my_data)
corr <- round(cor(my_data), 4)
# Visualiser
library("ggcorrplot")
ggcorrplot(corr, p.mat = cor_pmat(my_data),
           hc.order = TRUE, type = "lower",
           color = c("#FC4E07", "white", "#00AFBB"),
           outline.col = "white", lab = TRUE,sig.level = 0.05, digits = 3)


RSmodel <- rsm(X.V. ~ SO(X.SC., X.SR.,X.T.), data = All)
summary(RSmodel )

rsm(FO(formula = All$X.V.~(All$X.SC.+ All$X.SR.+All$ExT)), data = All)
RSmodel <- rsm(LogV ~ SO(X.SC., LogSR,X.T.), data = All)
summary(RSmodel )

RSmodel <- rsm(LogV ~ PQ(X.SC., LogSR,X.T.), data = All)
summary(RSmodel )


library(tidyverse)


library(plotly)
plot_ly(All, 
        x=All$X.T., 
        y=All$X.SR., 
        z=All$X.SC., 
        type="scatter3d", 
        mode="markers", 
        color=All$X.V., size = 30)

RSmodel$Df
par(mar = rep(2, 4))
contour(RSmodel , ~ X.SC.+ X.SR., image = TRUE, main="SO model (2variables) x=SC, y=SR, z=V",
+ at =85)
contour(RSmodel , ~ X.T.+LogSR+ X.SC. , image = TRUE, main="SO model")
contour(RSmodel , ~ X.SR.+ X.SC.+X.T. , image = TRUE, main="SO model")
contour(RSmodel , ~ X.SC.+LogSR+ InvT , image = TRUE, main="SO model")
exp(2.33)
exp(2.898)
plot(All$LogV,All$InvT)

All85<-All[All$X.T.==85,]
fit = lm(All85$LogV~All85$InvT)
summary(fit)

  contour(RSmodel , ~ X.SR.+ X.T.+X.SC., image = TRUE, main="SO model (3variables) x=SR, y=T, z=V")
contour (RSmodel,~   X.SC.+X.SR.+ X.T., image = TRUE)
persp(RSmodel ,  ~ X.SR.+X.T.+X.SC., zlab = "y", main="first-order model")
persp (RSmodel, ~X.SR.+X.SC.+X.T., at = xs(RSmodel))
persp (RSmodel, ~X.SR.+X.SC.+X.T., at = list(block="1")
       , ticktype = "simple",col="lightblue"
     )


persp (RSmodel, ~X.SR.+X.SC.+X.T., at = xs(RSmodel),
       contours = "col", col = rainbow(40), zlab = "Viscosity",
       xlabs = c("SR", "SC", "T"))







