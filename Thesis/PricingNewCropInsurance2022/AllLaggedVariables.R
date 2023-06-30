rm(list = ls())
#### Weather and Packages ####
library(igraph)
library(rstanarm)
library(tidyverse)
library(bayestestR)
library(betareg)
# library(mmr)
# library(zoib)
library(broom.mixed)
library(ROCit)
# library(loo)
library(pROC)
library(ROCR)
set.seed(69420)
load(file='D:/CorntoSpecialty/Data/WeatherLag.rda')
WeatherLag<-Weather
load(file='D:/CorntoSpecialty/Data/Causey.rda')
load(file='D:/CorntoSpecialty/Data/Prev20Yields.rda')
load(file='D:/CorntoSpecialty/Data/LossRatiosx.rda')
load(file='D:/CorntoSpecialty/Data/PotatoePolicies.rda')
load(file='D:/CorntoSpecialty/Data/PotatoeCoverageLevels.rda')
#LossRatiosy<-subset(LossRatiosy,ZeroIndemnities!=1)
#### More Prep ####

LossRatios<-merge(dfy,LossRatiosx,by=c('FIPS','Year'))
LossRatios<-LossRatios %>% group_by(FIPS)%>% mutate(random = sample(100,1))
assignmentLR<-unique(LossRatios[,c('FIPS','random')])
LossRatios = subset(LossRatios, select = -c(random) )

LossRatios<-merge(LossRatios,WeatherLag,by=c('FIPS','Year'))
LossRatios<-merge(LossRatios,CoverageLevels,by=c('FIPS','Year'))
LossRatios<-merge(LossRatios,PotatoPolicies,by=c('FIPS','Year'))
LossRatios<-subset(LossRatios,PercentLoss<1)
LossRatios<-merge(LossRatios,assignmentLR,by=c('FIPS'),all.x=TRUE)
covariatenames<-colnames(LossRatios[,c(6:I(ncol(LossRatios)-1))])
covariatenumber<-length(covariatenames)
resultslogistic <-data.frame(matrix(ncol=1,nrow=I(length(covariatenames)+1)))
resultslogistic[,1]<-c('(Intercept)',covariatenames)
colnames(resultslogistic)<-'Parameter'
performancetrain<-data.frame(matrix(nrow=9,ncol=4))
performancetrain<-performancetrain%>%
  rename(
    Full=X1,
    Less1=X2,
    Less2=X3
  )
performancevalidate<-performancetrain
##### Logistic Regression Modern Loss Ratio Lag Weather####
# m = model.matrix(~ .^2 - . + 0, data = df)

LossRatios$NonZeroLoss<-factor(LossRatios$NonZeroLoss)
x<-paste("NonZeroLoss ~", paste(covariatenames,collapse = " + "))
linformula<-as.formula(x)
validate<-subset(LossRatios,I((1-1)*10)<=random & random<=I(1*10))
for (x in 1:9) {
  validate<-subset(LossRatios,I((x-1)*10)<=random & random<=I(x*10))
  excludelist<-unique(validate[c('FIPS')])
  train<-filter(LossRatios,!(FIPS %in% excludelist$FIPS) )
  train<-subset(train,random<91)
  model<-stan_glm(
    linformula ,
    family = binomial(link="logit"),
    data=train ,
    model = TRUE,
    x = TRUE,
    y = TRUE,
    contrasts = NULL,
    weights = Insured.Acres,
    QR=TRUE,
    refresh=0,
    prior= normal(0,2),
    prior_intercept = normal(0,2)
  )
  coef<-map_estimate(model, precision=2^10, method ='kernel')
  coef<-coef %>%
    rename(
      x=MAP_Estimate
    )
  #assign(paste0("train", x), train)
  #assign(paste0("validate", x), validate)
  t<-roc(train$NonZeroLoss,model$fitted.values,plot=FALSE, percent = TRUE, xlab="False Positive %", ylab="True Positive %")
  performancetrain[x,1]<-t$auc  
  #results[,1]<-coef[c(2:I(covariatenumber+1)),2]
  #resultslogistic<-merge(resultslogistic,coef,by=c('Parameter'),all.x=TRUE)
  #validate<-subset(LossRatios,random>79 & random<91)
  # a<-as.matrix(validate[,c(6:I(covariatenumber+5))])
  # b<-coef[c(2:I(covariatenumber+1)),2]
  # test<- a%*%b
  # validate$predict<-test
  q<-predict(model,newdata=validate, type='response')
  par(pty="s")
  t<-roc(validate$NonZeroLoss,q,plot=FALSE, percent = TRUE, xlab="False Positive %", ylab="True Positive %")
  performancevalidate[x,1]<-t$auc  
}


#less1#
load(file='D:/CorntoSpecialty/Data/LossRatiosx1.rda')
rm(LossRatios, LossRatiosx)
LossRatios<-merge(dfy,LossRatiosx1,by=c('FIPS','Year'))
LossRatios<-merge(LossRatios,Weather,by=c('FIPS','Year'))
LossRatios<-merge(LossRatios,CoverageLevels,by=c('FIPS','Year'))
LossRatios<-merge(LossRatios,PotatoPolicies,by=c('FIPS','Year'))
LossRatios<-subset(LossRatios,PercentLoss<1)
LossRatios<-merge(LossRatios,assignmentLR,by=c('FIPS'),all.x=TRUE)
covariatenames<-colnames(LossRatios[,c(6:I(ncol(LossRatios)-1))])
covariatenumber<-length(covariatenames)
LossRatios$NonZeroLoss<-factor(LossRatios$NonZeroLoss)
x<-paste("NonZeroLoss ~", paste(covariatenames,collapse = " + "))
linformula<-as.formula(x)
for (x in 1:9) {
  validate<-subset(LossRatios,I((x-1)*10)<=random & random<=I(x*10))
  excludelist<-unique(validate[c('FIPS')])
  train<-filter(LossRatios,!(FIPS %in% excludelist$FIPS) )
  train<-subset(train,random<91)
  model<-stan_glm(
    linformula ,
    family = binomial(link="logit"),
    data=train ,
    model = TRUE,
    x = TRUE,
    y = TRUE,
    contrasts = NULL,
    weights = Insured.Acres,
    QR=TRUE,
    refresh=0,
    prior= normal(0,2),
    prior_intercept = normal(0,2)
  )
  coef<-map_estimate(model, precision=2^10, method ='kernel')
  coef<-coef %>%
    rename(
      x=MAP_Estimate
    )
  t<-roc(train$NonZeroLoss,model$fitted.values,plot=FALSE, percent = TRUE, xlab="False Positive %", ylab="True Positive %")
  performancetrain[x,2]<-t$auc  
  #results[,1]<-coef[c(2:I(covariatenumber+1)),2]
  # resultslogistic<-merge(resultslogistic,coef,by=c('Parameter'),all.x=TRUE)
  # #validate<-subset(LossRatios,random>79 & random<91)
  # a<-as.matrix(validate[,c(6:I(covariatenumber+5))])
  # b<-coef[c(2:I(covariatenumber+1)),2]
  # test<- a%*%b
  # validate$predict<-test
  q<-predict(model,newdata=validate, type='response')
  par(pty="s")
  t<-roc(validate$NonZeroLoss,q,plot=FALSE, percent = TRUE, xlab="False Positive %", ylab="True Positive %")
  performancevalidate[x,2]<-t$auc  
}
#2#
load(file='D:/CorntoSpecialty/Data/LossRatiosx2.rda')
rm(LossRatios, LossRatiosx1)
LossRatios<-merge(dfy,LossRatiosx2,by=c('FIPS','Year'))
LossRatios<-merge(LossRatios,Weather,by=c('FIPS','Year'))
LossRatios<-merge(LossRatios,CoverageLevels,by=c('FIPS','Year'))
LossRatios<-merge(LossRatios,PotatoPolicies,by=c('FIPS','Year'))
LossRatios<-subset(LossRatios,PercentLoss<1)
LossRatios<-merge(LossRatios,assignmentLR,by=c('FIPS'),all.x=TRUE)
covariatenames<-colnames(LossRatios[,c(6:I(ncol(LossRatios)-1))])
covariatenumber<-length(covariatenames)
LossRatios$NonZeroLoss<-factor(LossRatios$NonZeroLoss)
x<-paste("NonZeroLoss ~", paste(covariatenames,collapse = " + "))
linformula<-as.formula(x)
for (x in 1:9) {
  validate<-subset(LossRatios,I((x-1)*10)<=random & random<=I(x*10))
  excludelist<-unique(validate[c('FIPS')])
  train<-filter(LossRatios,!(FIPS %in% excludelist$FIPS) )
  train<-subset(train,random<91)
  model<-stan_glm(
    linformula ,
    family = binomial(link="logit"),
    data=train ,
    model = TRUE,
    x = TRUE,
    y = TRUE,
    contrasts = NULL,
    weights = Insured.Acres,
    QR=TRUE,
    refresh=0,
    prior= normal(0,2),
    prior_intercept = normal(0,2)
  )
  coef<-map_estimate(model, precision=2^10, method ='kernel')
  coef<-coef %>%
    rename(
      x=MAP_Estimate
    )
  t<-roc(train$NonZeroLoss,model$fitted.values,plot=FALSE, percent = TRUE, xlab="False Positive %", ylab="True Positive %")
  performancetrain[x,3]<-t$auc  
  #results[,1]<-coef[c(2:I(covariatenumber+1)),2]
  # resultslogistic<-merge(resultslogistic,coef,by=c('Parameter'),all.x=TRUE)
  # #validate<-subset(LossRatios,random>79 & random<91)
  # a<-as.matrix(validate[,c(6:I(covariatenumber+5))])
  # b<-coef[c(2:I(covariatenumber+1)),2]
  # test<- a%*%b
  # validate$predict<-test
  q<-predict(model,newdata=validate, type='response')
  par(pty="s")
  t<-roc(validate$NonZeroLoss,q,plot=FALSE, percent = TRUE, xlab="False Positive %", ylab="True Positive %")
  performancevalidate[x,3]<-t$auc   
}
#3#
load(file='D:/CorntoSpecialty/Data/LossRatiosx3.rda')
rm(LossRatios, LossRatiosx2)
LossRatios<-merge(dfy,LossRatiosx3,by=c('FIPS','Year'))
LossRatios<-merge(LossRatios,Weather,by=c('FIPS','Year'))
LossRatios<-merge(LossRatios,CoverageLevels,by=c('FIPS','Year'))
LossRatios<-merge(LossRatios,PotatoPolicies,by=c('FIPS','Year'))
LossRatios<-subset(LossRatios,PercentLoss<1)
LossRatios<-merge(LossRatios,assignmentLR,by=c('FIPS'),all.x=TRUE)
covariatenames<-colnames(LossRatios[,c(6:I(ncol(LossRatios)-1))])
covariatenumber<-length(covariatenames)
LossRatios$NonZeroLoss<-factor(LossRatios$NonZeroLoss)
x<-paste("NonZeroLoss ~", paste(covariatenames,collapse = " + "))
linformula<-as.formula(x)
for (x in 1:9) {
  validate<-subset(LossRatios,I((x-1)*10)<=random & random<=I(x*10))
  excludelist<-unique(validate[c('FIPS')])
  train<-filter(LossRatios,!(FIPS %in% excludelist$FIPS) )
  train<-subset(train,random<91)
  model<-stan_glm(
    linformula ,
    family = binomial(link="logit"),
    data=train ,
    model = TRUE,
    x = TRUE,
    y = TRUE,
    contrasts = NULL,
    weights = Insured.Acres,
    QR=TRUE,
    refresh=0,
    prior= normal(0,2),
    prior_intercept = normal(0,2)
  )
  coef<-map_estimate(model, precision=2^10, method ='kernel')
  coef<-coef %>%
    rename(
      x=MAP_Estimate
    )
  
  t<-roc(train$NonZeroLoss,model$fitted.values,plot=FALSE, percent = TRUE, xlab="False Positive %", ylab="True Positive %")
  performancetrain[x,4]<-t$auc  
  #results[,1]<-coef[c(2:I(covariatenumber+1)),2]
  # resultslogistic<-merge(resultslogistic,coef,by=c('Parameter'),all.x=TRUE)
  # #validate<-subset(LossRatios,random>79 & random<91)
  # a<-as.matrix(validate[,c(6:I(covariatenumber+5))])
  # b<-coef[c(2:I(covariatenumber+1)),2]
  # test<- a%*%b
  # validate$predict<-test
  q<-predict(model,newdata=validate, type='response')
  par(pty="s")
  t<-roc(validate$NonZeroLoss,q,plot=FALSE, percent = TRUE, xlab="False Positive %", ylab="True Positive %")
  performancevalidate[x,4]<-t$auc   
}
# coef<-map_estimate(model, precision=2^10, method ='kernel')
# t<-tidy( model, conf.int = TRUE, conf.level=0.9)
# observed<-model$y
# predict<-model$fitted.values
# measure <- measureit(score = predict, class = observed,
#                      measure = c("ACC", "SENS","SPEC", "FSCR"))
# 
# plot(measure$SPEC~I(1-measure$SENS), type = "l")
#Diagnosis... meh
write.csv(performancetrain,file='D:/CorntoSpecialty/Data/lagRMAlogisticauctrain.csv',row.names = FALSE)
write.csv(performancevalidate,file='D:/CorntoSpecialty/Data/lagRMAlogisticaucvalidate.csv',row.names = FALSE)

#### Re Prep ########
rm(LossRatios, LossRatiosx3)
LossRatios<-merge(dfy,Yields,by=c('FIPS','Year'))
# LossRatios<-LossRatios %>% group_by(FIPS)%>% mutate(random = sample(100,1))
# assignmentLR<-unique(LossRatios[,c('FIPS','random')])
# LossRatios = subset(LossRatios, select = -c(random) )

LossRatios<-merge(LossRatios,WeatherLag,by=c('FIPS','Year'))
LossRatios<-merge(LossRatios,CoverageLevels,by=c('FIPS','Year'))
LossRatios<-merge(LossRatios,PotatoPolicies,by=c('FIPS','Year'))
LossRatios<-subset(LossRatios,PercentLoss<1)
LossRatios<-merge(LossRatios,assignmentLR,by=c('FIPS'),all.x=TRUE)
covariatenames<-colnames(LossRatios[,c(6:I(ncol(LossRatios)-1))])
covariatenumber<-length(covariatenames)
resultslogistic <-data.frame(matrix(ncol=1,nrow=I(length(covariatenames)+1)))
resultslogistic[,1]<-c('(Intercept)',covariatenames)
colnames(resultslogistic)<-'Parameter'
performancetrain<-data.frame(matrix(nrow=9,ncol=3))
performancetrain<-performancetrain%>%
  rename(
    Full=X1,
    Less1=X2,
    Less2=X3
  )
performancevalidate<-performancetrain
##### Logistic Regression with Nass ####
# m = model.matrix(~ .^2 - . + 0, data = df)

LossRatios$NonZeroLoss<-factor(LossRatios$NonZeroLoss)
x<-paste("NonZeroLoss ~", paste(covariatenames,collapse = " + "))
linformula<-as.formula(x)
validate<-subset(LossRatios,I((1-1)*10)<=random & random<=I(1*10))
for (x in 1:9) {
  validate<-subset(LossRatios,I((x-1)*10)<=random & random<=I(x*10))
  excludelist<-unique(validate[c('FIPS')])
  train<-filter(LossRatios,!(FIPS %in% excludelist$FIPS) )
  train<-subset(train,random<91)
  model<-stan_glm(
    linformula ,
    family = binomial(link="logit"),
    data=train ,
    model = TRUE,
    x = TRUE,
    y = TRUE,
    contrasts = NULL,
    weights = Insured.Acres,
    QR=TRUE,
    refresh=0,
    prior= normal(0,2),
    prior_intercept = normal(0,2)
  )
  coef<-map_estimate(model, precision=2^10, method ='kernel')
  coef<-coef %>%
    rename(
      x=MAP_Estimate
    )
  #assign(paste0("train", x), train)
  #assign(paste0("validate", x), validate)
  t<-roc(train$NonZeroLoss,model$fitted.values,plot=FALSE, percent = TRUE, xlab="False Positive %", ylab="True Positive %")
  performancetrain[x,1]<-t$auc  
  #results[,1]<-coef[c(2:I(covariatenumber+1)),2]
  #resultslogistic<-merge(resultslogistic,coef,by=c('Parameter'),all.x=TRUE)
  #validate<-subset(LossRatios,random>79 & random<91)
  # a<-as.matrix(validate[,c(6:I(covariatenumber+5))])
  # b<-coef[c(2:I(covariatenumber+1)),2]
  # test<- a%*%b
  # validate$predict<-test
  q<-predict(model,newdata=validate, type='response')
  par(pty="s")
  t<-roc(validate$NonZeroLoss,q,plot=FALSE, percent = TRUE, xlab="False Positive %", ylab="True Positive %")
  performancevalidate[x,1]<-t$auc  
}


#less1#
load(file='D:/CorntoSpecialty/Data/Prev20Yields1.rda')
rm(LossRatios, LossRatiosx)
LossRatios<-merge(dfy,Yields1,by=c('FIPS','Year'))
LossRatios<-merge(LossRatios,Weather,by=c('FIPS','Year'))
LossRatios<-merge(LossRatios,CoverageLevels,by=c('FIPS','Year'))
LossRatios<-merge(LossRatios,PotatoPolicies,by=c('FIPS','Year'))
LossRatios<-subset(LossRatios,PercentLoss<1)
LossRatios<-merge(LossRatios,assignmentLR,by=c('FIPS'),all.x=TRUE)
covariatenames<-colnames(LossRatios[,c(6:I(ncol(LossRatios)-1))])
covariatenumber<-length(covariatenames)
LossRatios$NonZeroLoss<-factor(LossRatios$NonZeroLoss)
x<-paste("NonZeroLoss ~", paste(covariatenames,collapse = " + "))
linformula<-as.formula(x)
for (x in 1:9) {
  validate<-subset(LossRatios,I((x-1)*10)<=random & random<=I(x*10))
  excludelist<-unique(validate[c('FIPS')])
  train<-filter(LossRatios,!(FIPS %in% excludelist$FIPS) )
  train<-subset(train,random<91)
  model<-stan_glm(
    linformula ,
    family = binomial(link="logit"),
    data=train ,
    model = TRUE,
    x = TRUE,
    y = TRUE,
    contrasts = NULL,
    weights = Insured.Acres,
    QR=TRUE,
    refresh=0,
    prior= normal(0,2),
    prior_intercept = normal(0,2)
  )
  coef<-map_estimate(model, precision=2^10, method ='kernel')
  coef<-coef %>%
    rename(
      x=MAP_Estimate
    )
  t<-roc(train$NonZeroLoss,model$fitted.values,plot=FALSE, percent = TRUE, xlab="False Positive %", ylab="True Positive %")
  performancetrain[x,2]<-t$auc  
  #results[,1]<-coef[c(2:I(covariatenumber+1)),2]
  # resultslogistic<-merge(resultslogistic,coef,by=c('Parameter'),all.x=TRUE)
  # #validate<-subset(LossRatios,random>79 & random<91)
  # a<-as.matrix(validate[,c(6:I(covariatenumber+5))])
  # b<-coef[c(2:I(covariatenumber+1)),2]
  # test<- a%*%b
  # validate$predict<-test
  q<-predict(model,newdata=validate, type='response')
  par(pty="s")
  t<-roc(validate$NonZeroLoss,q,plot=FALSE, percent = TRUE, xlab="False Positive %", ylab="True Positive %")
  performancevalidate[x,2]<-t$auc  
}
#2#
load(file='D:/CorntoSpecialty/Data/Prev20Yields2.rda')
rm(LossRatios, LossRatiosx1)
LossRatios<-merge(dfy,Yields2,by=c('FIPS','Year'))
LossRatios<-merge(LossRatios,Weather,by=c('FIPS','Year'))
LossRatios<-merge(LossRatios,CoverageLevels,by=c('FIPS','Year'))
LossRatios<-merge(LossRatios,PotatoPolicies,by=c('FIPS','Year'))
LossRatios<-subset(LossRatios,PercentLoss<1)
LossRatios<-merge(LossRatios,assignmentLR,by=c('FIPS'),all.x=TRUE)
covariatenames<-colnames(LossRatios[,c(6:I(ncol(LossRatios)-1))])
covariatenumber<-length(covariatenames)
LossRatios$NonZeroLoss<-factor(LossRatios$NonZeroLoss)
x<-paste("NonZeroLoss ~", paste(covariatenames,collapse = " + "))
linformula<-as.formula(x)
for (x in 1:9) {
  validate<-subset(LossRatios,I((x-1)*10)<=random & random<=I(x*10))
  excludelist<-unique(validate[c('FIPS')])
  train<-filter(LossRatios,!(FIPS %in% excludelist$FIPS) )
  train<-subset(train,random<91)
  model<-stan_glm(
    linformula ,
    family = binomial(link="logit"),
    data=train ,
    model = TRUE,
    x = TRUE,
    y = TRUE,
    contrasts = NULL,
    weights = Insured.Acres,
    QR=TRUE,
    refresh=0,
    prior= normal(0,2),
    prior_intercept = normal(0,2)
  )
  coef<-map_estimate(model, precision=2^10, method ='kernel')
  coef<-coef %>%
    rename(
      x=MAP_Estimate
    )
  t<-roc(train$NonZeroLoss,model$fitted.values,plot=FALSE, percent = TRUE, xlab="False Positive %", ylab="True Positive %")
  performancetrain[x,3]<-t$auc  
  #results[,1]<-coef[c(2:I(covariatenumber+1)),2]
  # resultslogistic<-merge(resultslogistic,coef,by=c('Parameter'),all.x=TRUE)
  # #validate<-subset(LossRatios,random>79 & random<91)
  # a<-as.matrix(validate[,c(6:I(covariatenumber+5))])
  # b<-coef[c(2:I(covariatenumber+1)),2]
  # test<- a%*%b
  # validate$predict<-test
  q<-predict(model,newdata=validate, type='response')
  par(pty="s")
  t<-roc(validate$NonZeroLoss,q,plot=FALSE, percent = TRUE, xlab="False Positive %", ylab="True Positive %")
  performancevalidate[x,3]<-t$auc   
}
#3#
# load(file='D:/CorntoSpecialty/Data/LossRatiosx3.rda')
# rm(LossRatios, LossRatiosx2)
# LossRatios<-merge(dfy,LossRatiosx3,by=c('FIPS','Year'))
# LossRatios<-merge(LossRatios,Weather,by=c('FIPS','Year'))
# LossRatios<-merge(LossRatios,CoverageLevels,by=c('FIPS','Year'))
# LossRatios<-merge(LossRatios,PotatoPolicies,by=c('FIPS','Year'))
# LossRatios<-subset(LossRatios,PercentLoss<1)
# LossRatios<-merge(LossRatios,assignmentLR,by=c('FIPS'),all.x=TRUE)
# covariatenames<-colnames(LossRatios[,c(6:I(ncol(LossRatios)-1))])
# covariatenumber<-length(covariatenames)
# LossRatios$NonZeroLoss<-factor(LossRatios$NonZeroLoss)
# x<-paste("NonZeroLoss ~", paste(covariatenames,collapse = " + "))
# linformula<-as.formula(x)
# for (x in 1:9) {
#   validate<-subset(LossRatios,I((x-1)*10)<=random & random<=I(x*10))
#   excludelist<-unique(validate[c('FIPS')])
#   train<-filter(LossRatios,!(FIPS %in% excludelist$FIPS) )
#   train<-subset(train,random<91)
#   model<-stan_glm(
#     linformula ,
#     family = binomial(link="logit"),
#     data=train ,
#     model = TRUE,
#     x = TRUE,
#     y = TRUE,
#     contrasts = NULL,
#     weights = Insured.Acres,
#     QR=TRUE,
#     refresh=0,
#     prior= normal(0,2),
#     prior_intercept = normal(0,2)
#   )
#   coef<-map_estimate(model, precision=2^10, method ='kernel')
#   coef<-coef %>%
#     rename(
#       x=MAP_Estimate
#     )
#   
#   t<-roc(train$NonZeroLoss,model$fitted.values,plot=FALSE, percent = TRUE, xlab="False Positive %", ylab="True Positive %")
#   performancetrain[x,4]<-t$auc  
#   #results[,1]<-coef[c(2:I(covariatenumber+1)),2]
#   # resultslogistic<-merge(resultslogistic,coef,by=c('Parameter'),all.x=TRUE)
#   # #validate<-subset(LossRatios,random>79 & random<91)
#   # a<-as.matrix(validate[,c(6:I(covariatenumber+5))])
#   # b<-coef[c(2:I(covariatenumber+1)),2]
#   # test<- a%*%b
#   # validate$predict<-test
#   q<-predict(model,newdata=validate, type='response')
#   par(pty="s")
#   t<-roc(validate$NonZeroLoss,q,plot=FALSE, percent = TRUE, xlab="False Positive %", ylab="True Positive %")
#   performancevalidate[x,4]<-t$auc   
# }
# coef<-map_estimate(model, precision=2^10, method ='kernel')
# t<-tidy( model, conf.int = TRUE, conf.level=0.9)
# observed<-model$y
# predict<-model$fitted.values
# measure <- measureit(score = predict, class = observed,
#                      measure = c("ACC", "SENS","SPEC", "FSCR"))
# 
# plot(measure$SPEC~I(1-measure$SENS), type = "l")
#Diagnosis... meh
write.csv(performancetrain,file='D:/CorntoSpecialty/Data/lagNASSlogisticauctrain.csv',row.names = FALSE)
write.csv(performancevalidate,file='D:/CorntoSpecialty/Data/lagNASSlogisticaucvalidate.csv',row.names = FALSE)
#### Beta Reg Modern Loss Ratio Lag Weather ####
rm(LossRatios,LossRatiosx3)
load(file='D:/CorntoSpecialty/Data/LossRatiosx.rda')
LossRatios<-merge(dfy,LossRatiosx,by=c('FIPS','Year'))
LossRatios<-merge(LossRatios,WeatherLag,by=c('FIPS','Year'))
LossRatios<-merge(LossRatios,CoverageLevels,by=c('FIPS','Year'))
LossRatios<-merge(LossRatios,PotatoPolicies,by=c('FIPS','Year'))
LossRatios<-subset(LossRatios,PercentLoss<1)
LossRatios<-subset(LossRatios,PercentLoss>0)
LossRatios<-LossRatios %>% group_by(FIPS)%>% mutate(random = sample(100,1))
assignmentLR<-unique(LossRatios[,c('FIPS','random')])
LossRatios = subset(LossRatios, select = -c(random) )
LossRatios<-merge(LossRatios,assignmentLR,by=c('FIPS'),all.x=TRUE)
covariatenames<-colnames(LossRatios[,c(6:I(ncol(LossRatios)-1))])
covariatenumber<-length(covariatenames)
# resultslogistic <-data.frame(matrix(ncol=1,nrow=I(length(covariatenames)+1)))
# resultslogistic[,1]<-c('(Intercept)',covariatenames)
#colnames(resultslogistic)<-'Parameter'
performancevalidate<-data.frame(matrix(nrow=9,ncol=4))
performancevalidate<-performancevalidate%>%
  rename(
    Full=X1,
    Less1=X2,
    Less2=X3,
    Less3=X4
  )
performancetrain<-performancevalidate
x<-paste("PercentLoss ~", paste(covariatenames,collapse = " + "), "| totalpolicies + weightCovLevel.84")
linformula<-as.formula(x)
for (x in 1:9) {
  validate<-subset(LossRatios,I((x-1)*10)<=random & random<=I(x*10))
  excludelist<-unique(validate[c('FIPS')])
  train<-filter(LossRatios,!(FIPS %in% excludelist$FIPS) )
  train<-subset(train,random<91)
  # write.csv(train,file='D:/CorntoSpecialty/Data/Bugdata.csv',row.names=FALSE)
  model<-betareg(
    linformula ,
    link=c('logit'),
    data=train ,
    model = TRUE,
    x = TRUE,
    y = TRUE,
    weights = Insured.Acres
  )
  performancetrain[x,1]<-mean(model$residuals^2)
  #coef<-map_estimate(model, precision=2^10, method ='kernel')
  q<-predict(model,newdata=validate, type='response')
  # q<-posterior_predict(model,newdata=validate)
  # a<-as.matrix(validate[,c(6:I(covariatenumber+5))])
  # b<-model[["coefficients"]][["mean"]][c(2:I(covariatenumber+1))]
  # test<- a%*%b
  # validate$predict<-test
  performancevalidate[x,1]<-mean((validate$PercentLoss - q)^2)   
}

#1#
rm(LossRatios,LossRatiosx)
load(file='D:/CorntoSpecialty/Data/LossRatiosx1.rda')
LossRatios<-merge(dfy,LossRatiosx1,by=c('FIPS','Year'))
LossRatios<-merge(LossRatios,Weather,by=c('FIPS','Year'))
LossRatios<-merge(LossRatios,CoverageLevels,by=c('FIPS','Year'))
LossRatios<-merge(LossRatios,PotatoPolicies,by=c('FIPS','Year'))
LossRatios<-subset(LossRatios,PercentLoss<1)
LossRatios<-subset(LossRatios,PercentLoss>0)
LossRatios<-merge(LossRatios,assignmentLR,by=c('FIPS'),all.x=TRUE)
covariatenames<-colnames(LossRatios[,c(6:I(ncol(LossRatios)-1))])
covariatenumber<-length(covariatenames)

x<-paste("PercentLoss ~", paste(covariatenames,collapse = " + "), "| totalpolicies + weightCovLevel.84")
linformula<-as.formula(x)
for (x in 1:9) {
  validate<-subset(LossRatios,I((x-1)*10)<=random & random<=I(x*10))
  excludelist<-unique(validate[c('FIPS')])
  train<-filter(LossRatios,!(FIPS %in% excludelist$FIPS) )
  train<-subset(train,random<91)
  # write.csv(train,file='D:/CorntoSpecialty/Data/Bugdata.csv',row.names=FALSE)
  model<-betareg(
    linformula ,
    link=c('logit'),
    data=train ,
    model = TRUE,
    x = TRUE,
    y = TRUE,
    weights = Insured.Acres
  )
  performancetrain[x,2]<-mean(model$residuals^2)
  #coef<-map_estimate(model, precision=2^10, method ='kernel')
  q<-predict(model,newdata=validate, type='response')
  # q<-posterior_predict(model,newdata=validate)
  # a<-as.matrix(validate[,c(6:I(covariatenumber+5))])
  # b<-model[["coefficients"]][["mean"]][c(2:I(covariatenumber+1))]
  # test<- a%*%b
  # validate$predict<-test
  performancevalidate[x,2]<-mean((validate$PercentLoss - q)^2)   
}

#2#
rm(LossRatios,LossRatiosx1)
load(file='D:/CorntoSpecialty/Data/LossRatiosx2.rda')
LossRatios<-merge(dfy,LossRatiosx2,by=c('FIPS','Year'))
LossRatios<-merge(LossRatios,Weather,by=c('FIPS','Year'))
LossRatios<-merge(LossRatios,CoverageLevels,by=c('FIPS','Year'))
LossRatios<-merge(LossRatios,PotatoPolicies,by=c('FIPS','Year'))
LossRatios<-subset(LossRatios,PercentLoss<1)
LossRatios<-subset(LossRatios,PercentLoss>0)
LossRatios<-merge(LossRatios,assignmentLR,by=c('FIPS'),all.x=TRUE)
covariatenames<-colnames(LossRatios[,c(6:I(ncol(LossRatios)-1))])
covariatenumber<-length(covariatenames)

x<-paste("PercentLoss ~", paste(covariatenames,collapse = " + "), "| totalpolicies + weightCovLevel.84")
linformula<-as.formula(x)
for (x in 1:9) {
  validate<-subset(LossRatios,I((x-1)*10)<=random & random<=I(x*10))
  excludelist<-unique(validate[c('FIPS')])
  train<-filter(LossRatios,!(FIPS %in% excludelist$FIPS) )
  train<-subset(train,random<91)
  # write.csv(train,file='D:/CorntoSpecialty/Data/Bugdata.csv',row.names=FALSE)
  model<-betareg(
    linformula ,
    link=c('logit'),
    data=train ,
    model = TRUE,
    x = TRUE,
    y = TRUE,
    weights = Insured.Acres
  )
  performancetrain[x,3]<-mean(model$residuals^2)
  #coef<-map_estimate(model, precision=2^10, method ='kernel')
  q<-predict(model,newdata=validate, type='response')
  # q<-posterior_predict(model,newdata=validate)
  # a<-as.matrix(validate[,c(6:I(covariatenumber+5))])
  # b<-model[["coefficients"]][["mean"]][c(2:I(covariatenumber+1))]
  # test<- a%*%b
  # validate$predict<-test
  performancevalidate[x,3]<-mean((validate$PercentLoss - q)^2)   
}

#3#
rm(LossRatios,LossRatiosx2)
load(file='D:/CorntoSpecialty/Data/LossRatiosx3.rda')
LossRatios<-merge(dfy,LossRatiosx3,by=c('FIPS','Year'))
LossRatios<-merge(LossRatios,Weather,by=c('FIPS','Year'))
LossRatios<-merge(LossRatios,CoverageLevels,by=c('FIPS','Year'))
LossRatios<-merge(LossRatios,PotatoPolicies,by=c('FIPS','Year'))
LossRatios<-subset(LossRatios,PercentLoss<1)
LossRatios<-subset(LossRatios,PercentLoss>0)
LossRatios<-merge(LossRatios,assignmentLR,by=c('FIPS'),all.x=TRUE)
covariatenames<-colnames(LossRatios[,c(6:I(ncol(LossRatios)-1))])
covariatenumber<-length(covariatenames)

x<-paste("PercentLoss ~", paste(covariatenames,collapse = " + "), "| totalpolicies + weightCovLevel.84")
linformula<-as.formula(x)
for (x in 1:9) {
  validate<-subset(LossRatios,I((x-1)*10)<=random & random<=I(x*10))
  excludelist<-unique(validate[c('FIPS')])
  train<-filter(LossRatios,!(FIPS %in% excludelist$FIPS) )
  train<-subset(train,random<91)
  # write.csv(train,file='D:/CorntoSpecialty/Data/Bugdata.csv',row.names=FALSE)
  model<-betareg(
    linformula ,
    link=c('logit'),
    data=train ,
    model = TRUE,
    x = TRUE,
    y = TRUE,
    weights = Insured.Acres
  )
  performancetrain[x,4]<-mean(model$residuals^2)
  #coef<-map_estimate(model, precision=2^10, method ='kernel')
  q<-predict(model,newdata=validate, type='response')
  # q<-posterior_predict(model,newdata=validate)
  # a<-as.matrix(validate[,c(6:I(covariatenumber+5))])
  # b<-model[["coefficients"]][["mean"]][c(2:I(covariatenumber+1))]
  # test<- a%*%b
  # validate$predict<-test
  performancevalidate[x,4]<-mean((validate$PercentLoss - q)^2)   
}


write.csv(performancetrain,file='D:/CorntoSpecialty/Data/lagRMAmseBetaTrain.csv',row.names = FALSE)
write.csv(performancevalidate,file='D:/CorntoSpecialty/Data/lagRMAmseBetaValidate.csv',row.names = FALSE)


#### Beta Reg NASS Yields Lag Weather ####
rm(LossRatios,LossRatiosx3)
#load(file='D:/CorntoSpecialty/Data/LossRatiosx.rda')
LossRatios<-merge(dfy,Yields,by=c('FIPS','Year'))
LossRatios<-merge(LossRatios,Weather,by=c('FIPS','Year'))
LossRatios<-merge(LossRatios,CoverageLevels,by=c('FIPS','Year'))
LossRatios<-merge(LossRatios,PotatoPolicies,by=c('FIPS','Year'))
LossRatios<-subset(LossRatios,PercentLoss<1)
LossRatios<-subset(LossRatios,PercentLoss>0)
# LossRatios<-LossRatios %>% group_by(FIPS)%>% mutate(random = sample(100,1))
# assignmentLR<-unique(LossRatios[,c('FIPS','random')])
# LossRatios = subset(LossRatios, select = -c(random) )
LossRatios<-merge(LossRatios,assignmentLR,by=c('FIPS'),all.x=TRUE)
covariatenames<-colnames(LossRatios[,c(6:I(ncol(LossRatios)-1))])
covariatenumber<-length(covariatenames)
# resultslogistic <-data.frame(matrix(ncol=1,nrow=I(length(covariatenames)+1)))
# resultslogistic[,1]<-c('(Intercept)',covariatenames)
#colnames(resultslogistic)<-'Parameter'
performancevalidate<-data.frame(matrix(nrow=9,ncol=4))
performancevalidate<-performancevalidate%>%
  rename(
    Full=X1,
    Less1=X2,
    Less2=X3
  )
performancetrain<-performancevalidate
x<-paste("PercentLoss ~", paste(covariatenames,collapse = " + "), "| totalpolicies + weightCovLevel.84")
linformula<-as.formula(x)
for (x in 1:9) {
  validate<-subset(LossRatios,I((x-1)*10)<=random & random<=I(x*10))
  excludelist<-unique(validate[c('FIPS')])
  train<-filter(LossRatios,!(FIPS %in% excludelist$FIPS) )
  train<-subset(train,random<91)
  # write.csv(train,file='D:/CorntoSpecialty/Data/Bugdata.csv',row.names=FALSE)
  model<-betareg(
    linformula ,
    link=c('logit'),
    data=train ,
    model = TRUE,
    x = TRUE,
    y = TRUE,
    weights = Insured.Acres
  )
  performancetrain[x,1]<-mean(model$residuals^2)
  #coef<-map_estimate(model, precision=2^10, method ='kernel')
  q<-predict(model,newdata=validate, type='response')
  # q<-posterior_predict(model,newdata=validate)
  # a<-as.matrix(validate[,c(6:I(covariatenumber+5))])
  # b<-model[["coefficients"]][["mean"]][c(2:I(covariatenumber+1))]
  # test<- a%*%b
  # validate$predict<-test
  performancevalidate[x,1]<-mean((validate$PercentLoss - q)^2)   
}

#1#
rm(LossRatios,LossRatiosx)
load(file='D:/CorntoSpecialty/Data/LossRatiosx1.rda')
LossRatios<-merge(dfy,Yields1,by=c('FIPS','Year'))
LossRatios<-merge(LossRatios,Weather,by=c('FIPS','Year'))
LossRatios<-merge(LossRatios,CoverageLevels,by=c('FIPS','Year'))
LossRatios<-merge(LossRatios,PotatoPolicies,by=c('FIPS','Year'))
LossRatios<-subset(LossRatios,PercentLoss<1)
LossRatios<-subset(LossRatios,PercentLoss>0)
LossRatios<-merge(LossRatios,assignmentLR,by=c('FIPS'),all.x=TRUE)
covariatenames<-colnames(LossRatios[,c(6:I(ncol(LossRatios)-1))])
covariatenumber<-length(covariatenames)

x<-paste("PercentLoss ~", paste(covariatenames,collapse = " + "), "| totalpolicies + weightCovLevel.84")
linformula<-as.formula(x)
for (x in 1:9) {
  validate<-subset(LossRatios,I((x-1)*10)<=random & random<=I(x*10))
  excludelist<-unique(validate[c('FIPS')])
  train<-filter(LossRatios,!(FIPS %in% excludelist$FIPS) )
  train<-subset(train,random<91)
  # write.csv(train,file='D:/CorntoSpecialty/Data/Bugdata.csv',row.names=FALSE)
  model<-betareg(
    linformula ,
    link=c('logit'),
    data=train ,
    model = TRUE,
    x = TRUE,
    y = TRUE,
    weights = Insured.Acres
  )
  performancetrain[x,2]<-mean(model$residuals^2)
  #coef<-map_estimate(model, precision=2^10, method ='kernel')
  q<-predict(model,newdata=validate, type='response')
  # q<-posterior_predict(model,newdata=validate)
  # a<-as.matrix(validate[,c(6:I(covariatenumber+5))])
  # b<-model[["coefficients"]][["mean"]][c(2:I(covariatenumber+1))]
  # test<- a%*%b
  # validate$predict<-test
  performancevalidate[x,2]<-mean((validate$PercentLoss - q)^2)   
}

#2#
rm(LossRatios,LossRatiosx1)
load(file='D:/CorntoSpecialty/Data/LossRatiosx2.rda')
LossRatios<-merge(dfy,Yields2,by=c('FIPS','Year'))
LossRatios<-merge(LossRatios,Weather,by=c('FIPS','Year'))
LossRatios<-merge(LossRatios,CoverageLevels,by=c('FIPS','Year'))
LossRatios<-merge(LossRatios,PotatoPolicies,by=c('FIPS','Year'))
LossRatios<-subset(LossRatios,PercentLoss<1)
LossRatios<-subset(LossRatios,PercentLoss>0)
LossRatios<-merge(LossRatios,assignmentLR,by=c('FIPS'),all.x=TRUE)
covariatenames<-colnames(LossRatios[,c(6:I(ncol(LossRatios)-1))])
covariatenumber<-length(covariatenames)

x<-paste("PercentLoss ~", paste(covariatenames,collapse = " + "), "| totalpolicies + weightCovLevel.84")
linformula<-as.formula(x)
for (x in 1:9) {
  validate<-subset(LossRatios,I((x-1)*10)<=random & random<=I(x*10))
  excludelist<-unique(validate[c('FIPS')])
  train<-filter(LossRatios,!(FIPS %in% excludelist$FIPS) )
  train<-subset(train,random<91)
  # write.csv(train,file='D:/CorntoSpecialty/Data/Bugdata.csv',row.names=FALSE)
  model<-betareg(
    linformula ,
    link=c('logit'),
    data=train ,
    model = TRUE,
    x = TRUE,
    y = TRUE,
    weights = Insured.Acres
  )
  performancetrain[x,3]<-mean(model$residuals^2)
  #coef<-map_estimate(model, precision=2^10, method ='kernel')
  q<-predict(model,newdata=validate, type='response')
  # q<-posterior_predict(model,newdata=validate)
  # a<-as.matrix(validate[,c(6:I(covariatenumber+5))])
  # b<-model[["coefficients"]][["mean"]][c(2:I(covariatenumber+1))]
  # test<- a%*%b
  # validate$predict<-test
  performancevalidate[x,3]<-mean((validate$PercentLoss - q)^2)   
}




write.csv(performancetrain,file='D:/CorntoSpecialty/Data/lagNASSmseBetaTrain.csv',row.names = FALSE)
write.csv(performancevalidate,file='D:/CorntoSpecialty/Data/lagNASSmseBetaValidate.csv',row.names = FALSE)


#### Linear Regression Modern Loss Ratio Lag Weather ####
rm(model,LossRatios,LossRatiosx3)
load(file='D:/CorntoSpecialty/Data/LossRatiosx.rda')
load(file='D:/CorntoSpecialty/Data/LossRatiosy.rda')

LossRatiosy<-subset(LossRatiosy,ZeroIndemnities==0)
LossRatios<-merge(LossRatiosy,LossRatiosx,by=c('FIPS','Year'))
LossRatios<-LossRatios %>% group_by(FIPS)%>% mutate(random = sample(100,1))
assignment<-unique(LossRatios[,c('FIPS','random')])
LossRatios = subset(LossRatios, select = -c(random) )
LossRatios<-merge(LossRatios,WeatherLag,by=c('FIPS','Year'))
LossRatios<-merge(LossRatios,CoverageLevels,by=c('FIPS','Year'))
LossRatios<-merge(LossRatios,PotatoPolicies,by=c('FIPS','Year'))
LossRatios<-merge(LossRatios,assignment,by=c('FIPS'),all.x=TRUE)
covariatenames<-colnames(LossRatios[,c(6:I(ncol(LossRatios)-1))])
covariatenumber<-length(covariatenames)
results <-data.frame(matrix(ncol=1,nrow=I(length(covariatenames)+1)))
results[,1]<-c('(Intercept)',covariatenames)
colnames(results)<-'Parameter'
performancevalidate<-data.frame(matrix(nrow=9,ncol=4))
performancevalidate<-performancevalidate%>%
  rename(
    Full=X1,
    Less1=X2,
    Less2=X3,
    Less3=X4
  )
performancetrain<-performancevalidate
#LossRatios$State<-substr(LossRatios$FIPS,1,2)
x<-paste("LossRatio ~", paste(covariatenames,collapse = " + "))
linformula<-as.formula(x)
for (x in 1:9) {
  validate<-subset(LossRatios,I((x-1)*10)<=random & random<=I(x*10))
  excludelist<-unique(validate[c('FIPS')])
  train<-filter(LossRatios,!(FIPS %in% excludelist$FIPS) )
  train<-subset(train,random<91)
  model<-stan_glm(
    linformula ,
    family = gaussian(),
    data=train ,
    model = TRUE,
    x = TRUE,
    y = TRUE,
    contrasts = NULL,
    weights = Insured.Acres,
    prior = normal(0,2),
    prior_intercept = normal(0,2),
    algorithm = c("sampling")
  )
  performancetrain[x,1]<-mean(model$residuals^2)
  coef<-map_estimate(model, precision=2^10, method ='kernel')
  #results[,1]<-coef[c(2:I(covariatenumber+1)),2]
  #results<-merge(results,coef,by=c('Parameter'),all.x=TRUE)
  #validation<-subset(LossRatios,random>89 & random<96)
  a<-as.matrix(validate[,c(6:I(covariatenumber+5))])
  b<-coef[c(2:I(covariatenumber+1)),2]
  test<- a%*%b
  validate$predict<-test
  performancevalidate[x,1]<-mean((validate$LossRatio - validate$predict)^2)
}

## 1 ##
load(file='D:/CorntoSpecialty/Data/LossRatiosx1.rda')
rm(LossRatios, LossRatiosx)
LossRatios<-merge(LossRatiosy,LossRatiosx1,by=c('FIPS','Year'))
LossRatios<-merge(LossRatios,WeatherLag,by=c('FIPS','Year'))
LossRatios<-merge(LossRatios,CoverageLevels,by=c('FIPS','Year'))
LossRatios<-merge(LossRatios,PotatoPolicies,by=c('FIPS','Year'))
LossRatios<-merge(LossRatios,assignment,by=c('FIPS'),all.x=TRUE)
covariatenames<-colnames(LossRatios[,c(6:I(ncol(LossRatios)-1))])
covariatenumber<-length(covariatenames)
x<-paste("LossRatio ~", paste(covariatenames,collapse = " + "))
linformula<-as.formula(x)
for (x in 1:9) {
  validate<-subset(LossRatios,I((x-1)*10)<=random & random<=I(x*10))
  excludelist<-unique(validate[c('FIPS')])
  train<-filter(LossRatios,!(FIPS %in% excludelist$FIPS) )
  train<-subset(train,random<91)
  model<-stan_glm(
    linformula ,
    family = gaussian(),
    data=train ,
    model = TRUE,
    x = TRUE,
    y = TRUE,
    contrasts = NULL,
    weights = Insured.Acres,
    prior = normal(0,2),
    prior_intercept = normal(0,2),
    algorithm = c("sampling")
  )
  performancetrain[x,2]<-mean(model$residuals^2)
  coef<-map_estimate(model, precision=2^10, method ='kernel')
  #results[,1]<-coef[c(2:I(covariatenumber+1)),2]
  #results<-merge(results,coef,by=c('Parameter'),all.x=TRUE)
  #validate<-subset(LossRatios,random>89 & random<96)
  a<-as.matrix(validate[,c(6:I(covariatenumber+5))])
  b<-coef[c(2:I(covariatenumber+1)),2]
  test<- a%*%b
  validate$predict<-test
  performancevalidate[x,2]<-mean((validate$LossRatio - validate$predict)^2)
}

## 2 ##
load(file='D:/CorntoSpecialty/Data/LossRatiosx2.rda')
rm(LossRatios, LossRatiosx1)
LossRatios<-merge(LossRatiosy,LossRatiosx2,by=c('FIPS','Year'))
LossRatios<-merge(LossRatios,WeatherLag,by=c('FIPS','Year'))
LossRatios<-merge(LossRatios,CoverageLevels,by=c('FIPS','Year'))
LossRatios<-merge(LossRatios,PotatoPolicies,by=c('FIPS','Year'))
LossRatios<-merge(LossRatios,assignment,by=c('FIPS'),all.x=TRUE)
covariatenames<-colnames(LossRatios[,c(6:I(ncol(LossRatios)-1))])
covariatenumber<-length(covariatenames)
x<-paste("LossRatio ~", paste(covariatenames,collapse = " + "))
linformula<-as.formula(x)
for (x in 1:9) {
  validate<-subset(LossRatios,I((x-1)*10)<=random & random<=I(x*10))
  excludelist<-unique(validate[c('FIPS')])
  train<-filter(LossRatios,!(FIPS %in% excludelist$FIPS) )
  train<-subset(train,random<91)
  model<-stan_glm(
    linformula ,
    family = gaussian(),
    data=train ,
    model = TRUE,
    x = TRUE,
    y = TRUE,
    contrasts = NULL,
    weights = Insured.Acres,
    prior = normal(0,2),
    prior_intercept = normal(0,2),
    algorithm = c("sampling")
  )
  performancetrain[x,3]<-mean(model$residuals^2)
  coef<-map_estimate(model, precision=2^10, method ='kernel')
  #results[,1]<-coef[c(2:I(covariatenumber+1)),2]
  #results<-merge(results,coef,by=c('Parameter'),all.x=TRUE)
  #validation<-subset(LossRatios,random>89 & random<96)
  a<-as.matrix(validate[,c(6:I(covariatenumber+5))])
  b<-coef[c(2:I(covariatenumber+1)),2]
  test<- a%*%b
  validate$predict<-test
  performancevalidate[x,3]<-mean((validate$LossRatio - validate$predict)^2)
}
## 3 Mode##
load(file='D:/CorntoSpecialty/Data/LossRatiosx3.rda')
rm(LossRatios, LossRatiosx2)
LossRatios<-merge(LossRatiosy,LossRatiosx3,by=c('FIPS','Year'))
LossRatios<-merge(LossRatios,WeatherLag,by=c('FIPS','Year'))
LossRatios<-merge(LossRatios,CoverageLevels,by=c('FIPS','Year'))
LossRatios<-merge(LossRatios,PotatoPolicies,by=c('FIPS','Year'))
LossRatios<-merge(LossRatios,assignment,by=c('FIPS'),all.x=TRUE)
covariatenames<-colnames(LossRatios[,c(6:I(ncol(LossRatios)-1))])
covariatenumber<-length(covariatenames)
x<-paste("LossRatio ~", paste(covariatenames,collapse = " + "))
linformula<-as.formula(x)
for (x in 1:9) {
  validate<-subset(LossRatios,I((x-1)*10)<=random & random<=I(x*10))
  excludelist<-unique(validate[c('FIPS')])
  train<-filter(LossRatios,!(FIPS %in% excludelist$FIPS) )
  train<-subset(train,random<91)
  model<-stan_glm(
    linformula ,
    family = gaussian(),
    data=train ,
    model = TRUE,
    x = TRUE,
    y = TRUE,
    contrasts = NULL,
    weights = Insured.Acres,
    prior = normal(0,2),
    prior_intercept = normal(0,2),
    algorithm = c("sampling")
  )
  performancetrain[x,4]<-mean(model$residuals^2)
  coef<-map_estimate(model, precision=2^10, method ='kernel')
  #results[,1]<-coef[c(2:I(covariatenumber+1)),2]
  #results<-merge(results,coef,by=c('Parameter'),all.x=TRUE)
  #validation<-subset(LossRatios,random>89 & random<96)
  a<-as.matrix(validate[,c(6:I(covariatenumber+5))])
  b<-coef[c(2:I(covariatenumber+1)),2]
  test<- a%*%b
  validate$predict<-test
  performancevalidate[x,4]<-mean((validate$LossRatio - validate$predict)^2)
}

write.csv(performancetrain,file='D:/CorntoSpecialty/Data/lagRMAmselinearTrain.csv',row.names = FALSE)
write.csv(performancevalidate,file='D:/CorntoSpecialty/Data/lagRMAmselinearValidate.csv',row.names = FALSE)

#### Linear Regression Lag NASS Lag Weather ####
rm(model,LossRatios,LossRatiosx3)
load(file='D:/CorntoSpecialty/Data/LossRatiosx.rda')
load(file='D:/CorntoSpecialty/Data/LossRatiosy.rda')

LossRatiosy<-subset(LossRatiosy,ZeroIndemnities==0)
LossRatios<-merge(LossRatiosy,Yields,by=c('FIPS','Year'))
# LossRatios<-LossRatios %>% group_by(FIPS)%>% mutate(random = sample(100,1))
# assignment<-unique(LossRatios[,c('FIPS','random')])
# LossRatios = subset(LossRatios, select = -c(random) )
LossRatios<-merge(LossRatios,WeatherLag,by=c('FIPS','Year'))
LossRatios<-merge(LossRatios,CoverageLevels,by=c('FIPS','Year'))
LossRatios<-merge(LossRatios,PotatoPolicies,by=c('FIPS','Year'))
LossRatios<-merge(LossRatios,assignment,by=c('FIPS'),all.x=TRUE)
covariatenames<-colnames(LossRatios[,c(6:I(ncol(LossRatios)-1))])
covariatenumber<-length(covariatenames)
results <-data.frame(matrix(ncol=1,nrow=I(length(covariatenames)+1)))
results[,1]<-c('(Intercept)',covariatenames)
colnames(results)<-'Parameter'
performancevalidate<-data.frame(matrix(nrow=9,ncol=4))
performancevalidate<-performancevalidate%>%
  rename(
    Full=X1,
    Less1=X2,
    Less2=X3,
    Less3=X4
  )
performancetrain<-performancevalidate
#LossRatios$State<-substr(LossRatios$FIPS,1,2)
x<-paste("LossRatio ~", paste(covariatenames,collapse = " + "))
linformula<-as.formula(x)
for (x in 1:9) {
  validate<-subset(LossRatios,I((x-1)*10)<=random & random<=I(x*10))
  excludelist<-unique(validate[c('FIPS')])
  train<-filter(LossRatios,!(FIPS %in% excludelist$FIPS) )
  train<-subset(train,random<91)
  model<-stan_glm(
    linformula ,
    family = gaussian(),
    data=train ,
    model = TRUE,
    x = TRUE,
    y = TRUE,
    contrasts = NULL,
    weights = Insured.Acres,
    prior = normal(0,2),
    prior_intercept = normal(0,2),
    algorithm = c("sampling")
  )
  performancetrain[x,1]<-mean(model$residuals^2)
  coef<-map_estimate(model, precision=2^10, method ='kernel')
  #results[,1]<-coef[c(2:I(covariatenumber+1)),2]
  #results<-merge(results,coef,by=c('Parameter'),all.x=TRUE)
  #validation<-subset(LossRatios,random>89 & random<96)
  a<-as.matrix(validate[,c(6:I(covariatenumber+5))])
  b<-coef[c(2:I(covariatenumber+1)),2]
  test<- a%*%b
  validate$predict<-test
  performancevalidate[x,1]<-mean((validate$LossRatio - validate$predict)^2)
}

## 1 ##
load(file='D:/CorntoSpecialty/Data/LossRatiosx1.rda')
rm(LossRatios, LossRatiosx)
LossRatios<-merge(LossRatiosy,Yields1,by=c('FIPS','Year'))
LossRatios<-merge(LossRatios,WeatherLag,by=c('FIPS','Year'))
LossRatios<-merge(LossRatios,CoverageLevels,by=c('FIPS','Year'))
LossRatios<-merge(LossRatios,PotatoPolicies,by=c('FIPS','Year'))
LossRatios<-merge(LossRatios,assignment,by=c('FIPS'),all.x=TRUE)
covariatenames<-colnames(LossRatios[,c(6:I(ncol(LossRatios)-1))])
covariatenumber<-length(covariatenames)
x<-paste("LossRatio ~", paste(covariatenames,collapse = " + "))
linformula<-as.formula(x)
for (x in 1:9) {
  validate<-subset(LossRatios,I((x-1)*10)<=random & random<=I(x*10))
  excludelist<-unique(validate[c('FIPS')])
  train<-filter(LossRatios,!(FIPS %in% excludelist$FIPS) )
  train<-subset(train,random<91)
  model<-stan_glm(
    linformula ,
    family = gaussian(),
    data=train ,
    model = TRUE,
    x = TRUE,
    y = TRUE,
    contrasts = NULL,
    weights = Insured.Acres,
    prior = normal(0,2),
    prior_intercept = normal(0,2),
    algorithm = c("sampling")
  )
  performancetrain[x,2]<-mean(model$residuals^2)
  coef<-map_estimate(model, precision=2^10, method ='kernel')
  #results[,1]<-coef[c(2:I(covariatenumber+1)),2]
  #results<-merge(results,coef,by=c('Parameter'),all.x=TRUE)
  #validate<-subset(LossRatios,random>89 & random<96)
  a<-as.matrix(validate[,c(6:I(covariatenumber+5))])
  b<-coef[c(2:I(covariatenumber+1)),2]
  test<- a%*%b
  validate$predict<-test
  performancevalidate[x,2]<-mean((validate$LossRatio - validate$predict)^2)
}

## 2 ##
load(file='D:/CorntoSpecialty/Data/LossRatiosx2.rda')
rm(LossRatios, LossRatiosx1)
LossRatios<-merge(LossRatiosy,Yields2,by=c('FIPS','Year'))
LossRatios<-merge(LossRatios,WeatherLag,by=c('FIPS','Year'))
LossRatios<-merge(LossRatios,CoverageLevels,by=c('FIPS','Year'))
LossRatios<-merge(LossRatios,PotatoPolicies,by=c('FIPS','Year'))
LossRatios<-merge(LossRatios,assignment,by=c('FIPS'),all.x=TRUE)
covariatenames<-colnames(LossRatios[,c(6:I(ncol(LossRatios)-1))])
covariatenumber<-length(covariatenames)
x<-paste("LossRatio ~", paste(covariatenames,collapse = " + "))
linformula<-as.formula(x)
for (x in 1:9) {
  validate<-subset(LossRatios,I((x-1)*10)<=random & random<=I(x*10))
  excludelist<-unique(validate[c('FIPS')])
  train<-filter(LossRatios,!(FIPS %in% excludelist$FIPS) )
  train<-subset(train,random<91)
  model<-stan_glm(
    linformula ,
    family = gaussian(),
    data=train ,
    model = TRUE,
    x = TRUE,
    y = TRUE,
    contrasts = NULL,
    weights = Insured.Acres,
    prior = normal(0,2),
    prior_intercept = normal(0,2),
    algorithm = c("sampling")
  )
  performancetrain[x,3]<-mean(model$residuals^2)
  coef<-map_estimate(model, precision=2^10, method ='kernel')
  #results[,1]<-coef[c(2:I(covariatenumber+1)),2]
  #results<-merge(results,coef,by=c('Parameter'),all.x=TRUE)
  #validation<-subset(LossRatios,random>89 & random<96)
  a<-as.matrix(validate[,c(6:I(covariatenumber+5))])
  b<-coef[c(2:I(covariatenumber+1)),2]
  test<- a%*%b
  validate$predict<-test
  performancevalidate[x,3]<-mean((validate$LossRatio - validate$predict)^2)
}

write.csv(performancetrain,file='D:/CorntoSpecialty/Data/lagNASSmselinearTrain.csv',row.names = FALSE)
write.csv(performancevalidate,file='D:/CorntoSpecialty/Data/lagNASSmselinearValidate.csv',row.names = FALSE)
#### Test ####
rm(LossRatios, results, d)
load(file='D:/CorntoSpecialty/Data/Prev20Yields2.rda')
LossRatios<-merge(dfy,Yields2,by=c('FIPS','Year'))
LossRatios<-merge(LossRatios,WeatherLag,by=c('FIPS','Year'))
LossRatios<-merge(LossRatios,CoverageLevels,by=c('FIPS','Year'))
LossRatios<-merge(LossRatios,PotatoPolicies,by=c('FIPS','Year'))
LossRatios<-merge(LossRatios,assignmentLR,by=c('FIPS'),all.x=TRUE)
covariatenames<-colnames(LossRatios[,c(6:I(ncol(LossRatios)-1))])
covariatenumber<-length(covariatenames)
resultslogistic <-data.frame(matrix(ncol=1,nrow=I(length(covariatenames)+1)))
resultslogistic[,1]<-c('(Intercept)',covariatenames)
resultsbeta<-data.frame(matrix(ncol=1,nrow=I(length(covariatenames)+1)))
resultsbeta[,1]<-c('(Intercept)',covariatenames)
colnames(resultslogistic)<-'Parameter'
#### Logistic Test ####
LossRatios$NonZeroLoss<-factor(LossRatios$NonZeroLoss)
x<-paste("NonZeroLoss ~", paste(covariatenames,collapse = " + "))
linformula<-as.formula(x)
train<-subset(LossRatios,random<91)
test<-subset(LossRatios,random>90)
model<-stan_glm(
  linformula ,
  family = binomial(link="logit"),
  data=train ,
  model = TRUE,
  x = TRUE,
  y = TRUE,
  contrasts = NULL,
  weights = Insured.Acres,
  QR=TRUE,
  refresh=0,
  prior= normal(0,2),
  prior_intercept = normal(0,2)
)
temp<-summary(model, type = 'deviance')
capture.output(temp,file='D:/CorntoSpecialty/Results/finallogit.txt')

mapcoef<-map_estimate(model, precision=2^10, method ='kernel')
coef<-model$coefficients
sd<-model$ses
resultslogistic<-cbind(resultslogistic,coef)
resultslogistic<-cbind(resultslogistic,sd)
resultslogistic<-cbind(resultslogistic,mapcoef)
# resultslogistic<-resultslogistic[,c(2:3)]
write.csv(resultslogistic,file='D:/CorntoSpecialty/Results/LogisticCoefficientsLagNASS.csv',row.names = TRUE)

q<-predict(model,newdata=test, type='response')
par(pty="s")
roc_rose <- plot(roc(train$NonZeroLoss, model$fitted.values), print.auc = TRUE, col = "blue")
roc_rose <- plot(roc(test$NonZeroLoss, q), print.auc = TRUE, 
                 col = "red", print.auc.y = .4, add = TRUE)

#pred <- prediction( model$fitted.values, train$NonZeroLoss)
# perf<-performance(pred,measure="sens", x.measure="spec")
# plot(perf)
#### Beta Test ####
LossRatiosb<-subset(LossRatios,PercentLoss<1)
LossRatiosb<-subset(LossRatiosb,PercentLoss>0)
x<-paste("PercentLoss ~", paste(covariatenames,collapse = " + "), "| totalpolicies + weightCovLevel.84")
linformula<-as.formula(x)
trainb<-subset(LossRatiosb,random<91)
testb<-subset(LossRatiosb,random>90)
model<-betareg(
  linformula,
  link=c('logit'),
  data=trainb,
  model = TRUE,
  x = TRUE,
  y = TRUE,
  weights = Insured.Acres
)
n<-as.integer(model[["n"]])
temp<-summary(model, type = 'deviance')
capture.output(temp,file='D:/CorntoSpecialty/Results/finalbetaLLagNass.txt')
obs<-cbind(data.frame(y = model[["fitted.values"]]),data.frame(x = model[["y"]]))
p<-ggplot(obs,aes(x=x,y=y))+
  geom_bin2d()+
  labs(x="Observed % Indemnified",y="Fitted % Indemnified")+
  geom_abline(intercept=0, slope=1)


# coef<-model$coefficients
# sd<-model$ses
# resultsbeta<-cbind(resultsbeta,coef)
# resultsbeta<-cbind(resultsbeta,sd)
# # resultslogistic<-resultslogistic[,c(2:3)]
# write.csv(resultsbeta,file='D:/CorntoSpecialty/Results/BetaCoefficients.csv',row.names = TRUE)

s<-predict(model,newdata=test, type='response')
test<-cbind(test,s)
test<-cbind(test,q)
testy<-test[,c('FIPS','Year','s','q')]
load(file='D:/CorntoSpecialty/Data/CoverageLevels.rda')
potcov<-CoverageLevels[,c('FIPS','Year','weightCovLevel.84')]
testy<-merge(potcov,testy,by=c('FIPS','Year'),all.y=TRUE)

save(testy,file='D:/CorntoSpecialty/Results/testyLagNass.rda')
##### calc ####
load(file='D:/CorntoSpecialty/Data/SoB.rda')

sob<-subset(SOBSCCTPU,Commodity.Code==84)
a<-aggregate(cbind(Insured.Acres,Liability.Amount,Indemnity.Amount,Total.Premium.Amount)~FIPS+Year,data=sob,sum,na.rm = FALSE)
# load(file='D:/CorntoSpecialty/Results/testy.rda')
testy<-merge(testy,a,by=c('FIPS','Year'),all.x=TRUE)
#testy<-merge(testy,potcov,by=c('FIPS','Year'),all.x=TRUE)
testy$predictloss<-testy$q*testy$s*testy$Liability.Amount*testy$weightCovLevel.84
totallosspredict<-sum(testy$predictloss)
totalloss<-sum(testy$Indemnity.Amount)
off<-totallosspredict/totalloss
usdaindem<-sum(testy$Indemnity.Amount)
usdaprem<-sum(testy$Total.Premium.Amount)
usdaoff<-usdaprem/usdaindem
usdalossratio<-1/usdaoff

