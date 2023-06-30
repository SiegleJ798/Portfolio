rm(list = ls())
library(stringr)
library('dplyr')
library(tidyverse)
library(maps)
library(data.table)
library(readxl)
library(writexl)
library(ggpubr)
library(lubridate)
library(betareg)
Sys.setenv(JAGS_HOME='D:/JAGS-4.3.0')
library(rjags)
library(zoib)
library(repmod)
# save(CoLCornSoyCountiesWide,file='D:/CoLCounties.Rda')
# save(CoLCornSoyPlansWide,file='D:/CoLPlans.Rda')
# save(DamageGroupsMonths,file='D:/CoLPropByMonth.Rda')
# 
# save(TotalInsuredAcres,file='D:/InsuredAcres.Rda')
# save(soySOBSCCTPUaggcounty,file='D:/SoyCountySOB.Rda')
# save(cornSOBSCCTPUaggcounty,file='D:/CornCountySOB.Rda')
# save(soySOBSCCTPUaggplan,file='D:/SoyPlanSOB.Rda')
# save(cornSOBSCCTPUaggplan,file='D:/CornPlanSOB.Rda')
# 
# save(DroughtMonthly,file='D:/DroughtMonths.Rda')
# save(DroughtSeasonally,file='D:/DroughtSeasons.Rda')
# save(GrainCornYearlyDrought,file='D:/DroughtYears.Rda')
# save(DroughtMonitor,file='D:/DroughtMonitor.Rda')


### PCA: Find Orthogonal Components ####
# 
# Monthly.PCA<-prcomp(GrainCornMonthlyDrought[,c(13:18)], center=TRUE, scale.=TRUE)
# summary(Monthly.PCA)
# 
# Seasonal.PCA<-prcomp(GrainCornSeasonalDrought[,c(14:19)], center=TRUE, scale.=TRUE)
# 
# cov(GrainCornYearlyDrought[,c(3:18)])
# 
# Seasonal<-reshape(data=GrainCornSeasonalDrought, idvar=c('FIPS','GrainCornYear'),v.names=c('DNone','D0','D1','D2','D3','D4'),timevar="Season",direction='wide')
# Seasonal[is.na(Seasonal)]<-0
# Seasonal.PCA<-prcomp(Seasonal[,c(3:26)], center=TRUE, scale.=TRUE)
# Covariances<-cov(Seasonal[,c(3:26)])
# write.csv(Covariances,"D:/DroughtMonitor/SeasonalCovariances.csv", row.names = FALSE)



#### Drought to Loss ####

setwd("D:/")
load(file='InsuredAcres.rda')
load(file='CoLCounties.rda')
load(file='DroughtYears.rda')
load(file='IrrAcres.rda')
load(file='D:/FSADrought.Rda')
load(file='IrrSupplyFail.rda')

IrrigatedAcres[is.na(IrrigatedAcres)]<-0
IrrigatedAcres$PropIrr<-IrrigatedAcres$IrrInsurAcres/(IrrigatedAcres$IrrInsurAcres+IrrigatedAcres$NonIrrInsurAcres)
counties<-merge(TotalInsuredAcres,CoLCornSoyCountiesWide,by=c('Commodity.Code','FIPS','Year'),all.x=TRUE)
counties[is.na(counties)]<-0
counties$PropIndem<-counties$Total.Determined.Acres/counties$Insured.Acres
counties$PropIndem[counties$PropIndem>1]<-1
counties$PropIndem[counties$PropIndem<0]<-0
counties$DroughtIndem<-counties$Determined.Acres.1/(counties$Insured.Acres-counties$Determined.Acres.2-counties$Determined.Acres.3)
counties$DroughtIndem[counties$DroughtIndem>1]<-1
counties$DroughtIndem[counties$DroughtIndem<0]<-0

counties<-merge(counties,GrainCornYearlyDrought,by=c('FIPS','Year'))
counties<-merge(counties,IrrigatedAcres,by=c('Commodity.Code','FIPS','Year'),all.x=TRUE)
counties[is.na(counties)]<-0
counties$NonZero<-if_else(counties$PropIndem==0,0,1)
counties$MaxIndem<-if_else(counties$PropIndem==1,1,0)
counties$NonDZero<-if_else(counties$DroughtIndem==0,0,1)
counties$MaxDIndem<-if_else(counties$DroughtIndem==1,1,0)

counties$groups<-cumsum(!duplicated(counties[1:2]))
counties<-counties %>%
  group_by(groups)%>%
  mutate(lagD0 = dplyr:: lag(D0,n=1,default=NA))%>%
  mutate(lagD1 = dplyr:: lag(D1,n=1,default=NA))%>%
  mutate(lagD2 = dplyr:: lag(D2,n=1,default=NA))%>%
  mutate(lagD3 = dplyr:: lag(D3,n=1,default=NA))%>%
  mutate(lagD4 = dplyr:: lag(D4,n=1,default=NA))

counties$State<-substr(counties$FIPS,1,2)

counties<-merge(counties,FSADrought,by=c('FIPS','Year'),all.x=TRUE)
counties[is.na(counties)]<-0

counties$USDCI<- counties$D0+2*counties$D1+3*counties$D2+4*counties$D3+5*counties$D4
counties$LagUSDCI<- counties$lagD0+2*counties$lagD1+3*counties$lagD2+4*counties$lagD3+5*counties$lagD4

counties<-merge(counties,IrrSupplyFail,all.x=TRUE)
counties[is.na(counties)]<-0
counties$PropIrrFail<-counties$IrrSupplyFailAcres/counties$IrrInsurAcres
counties$PropIrrFail[counties$PropIrrFail>1]<-1
counties$PropIrrFail[counties$PropIrrFail<0]<-0
counties$IrrNonZero<-if_else(counties$PropIrrFail==0,0,1)
counties$IrrMaxIndem<-if_else(counties$PropIrrFail==1,1,0)


soycounties<-subset(counties,counties$Commodity.Code==81)
corncounties<-subset(counties,counties$Commodity.Code==41)

#### Corn Regressions ####
cornbernzero<-glm(NonZero ~ Primary.Declared.Drought+D0+D1+D2+D3+D4+lagD0+lagD1+lagD2+lagD3+lagD4 + I(USDCI*LagUSDCI)+ PropIrr + I(PropIrr*D0) +I(PropIrr*D1)+ I(PropIrr*D2) +I(PropIrr*D3)+I(PropIrr*D4) + factor(Year) , family = binomial , data=corncounties )
temp<-summary(cornbernzero)
capture.output(temp,file='Results/cornbernzero.txt')

cornbernone<-glm(MaxIndem ~ Primary.Declared.Drought+D0+D1+D2+D3+D4+lagD0+lagD1+lagD2+lagD3+lagD4 +I(USDCI*LagUSDCI)+ PropIrr + I(PropIrr*D0) +I(PropIrr*D1)+ I(PropIrr*D2) +I(PropIrr*D3)+I(PropIrr*D4) + factor(Year), family = binomial , data=corncounties,subset=NonZero==1 )
temp<-summary(cornbernone)
capture.output(temp,file='Results/cornbernone.txt')

cornbeta<-betareg(PropIndem~Primary.Declared.Drought+D0+D1+D2+D3+D4+lagD0+lagD1+lagD2+lagD3+lagD4 +I(USDCI*LagUSDCI)+ PropIrr + I(PropIrr*D0) +I(PropIrr*D1)+ I(PropIrr*D2) +I(PropIrr*D3)+I(PropIrr*D4)+ factor(Year),data=corncounties, subset = MaxIndem==0 & NonZero==1)
temp<-summary(cornbeta, type = 'deviance')
capture.output(temp,file='Results/cornbeta.txt')


cornbernzero<-glm(NonZero ~ Primary.Declared.Drought+D0+D1+D2+D3+D4+lagD0+lagD1+lagD2+lagD3+lagD4 +I(USDCI*LagUSDCI)+ PropIrr + I(PropIrr*D0) +I(PropIrr*D1)+ I(PropIrr*D2) +I(PropIrr*D3)+I(PropIrr*D4)+factor(State)+ factor(Year) , family = binomial , data=corncounties)
temp<-summary(cornbernzero)
capture.output(temp,file='Results/FE/cornbernzero.txt')

cornbernone<-glm(MaxIndem ~ Primary.Declared.Drought+D0+D1+D2+D3+D4+lagD0+lagD1+lagD2+lagD3+lagD4 +I(USDCI*LagUSDCI)+ PropIrr + I(PropIrr*D0) +I(PropIrr*D1)+ I(PropIrr*D2) +I(PropIrr*D3)+I(PropIrr*D4)+factor(State)+ factor(Year) , family = binomial , data=corncounties,subset=NonZero==1)
temp<-summary(cornbernone)
capture.output(temp,file='Results/FE/cornbernone.txt')

cornbeta<-betareg(PropIndem~Primary.Declared.Drought+D0+D1+D2+D3+D4+lagD0+lagD1+lagD2+lagD3+lagD4 +I(USDCI*LagUSDCI)+ PropIrr + I(PropIrr*D0) +I(PropIrr*D1)+ I(PropIrr*D2) +I(PropIrr*D3)+I(PropIrr*D4)+factor(State)+ factor(Year),data=corncounties, subset = MaxIndem==0 & NonZero==1)
temp<-summary(cornbeta, type= 'deviance')
capture.output(temp,file='Results/FE/cornbeta.txt')


cornbernzero<-glm(NonDZero ~ Primary.Declared.Drought+D0+D1+D2+D3+D4+lagD0+lagD1+lagD2+lagD3+lagD4 +I(USDCI*LagUSDCI)+ PropIrr + I(PropIrr*D0) +I(PropIrr*D1)+ I(PropIrr*D2) +I(PropIrr*D3)+I(PropIrr*D4) + factor(Year), family = binomial , data=corncounties )
temp<-summary(cornbernzero)
capture.output(temp,file='Results/cornDbernzero.txt')

cornbernone<-glm(MaxDIndem ~ Primary.Declared.Drought+D0+D1+D2+D3+D4+lagD0+lagD1+lagD2+lagD3+lagD4 +I(USDCI*LagUSDCI)+ PropIrr + I(PropIrr*D0) +I(PropIrr*D1)+ I(PropIrr*D2) +I(PropIrr*D3)+I(PropIrr*D4) + factor(Year), family = binomial , data=corncounties,subset=NonDZero==1 )
temp<-summary(cornbernone)
capture.output(temp,file='Results/cornDbernone.txt')

cornbeta<-betareg(DroughtIndem~Primary.Declared.Drought+D0+D1+D2+D3+D4+lagD0+lagD1+lagD2+lagD3+lagD4 +I(USDCI*LagUSDCI)+ PropIrr + I(PropIrr*D0) +I(PropIrr*D1)+ I(PropIrr*D2) +I(PropIrr*D3)+I(PropIrr*D4)+ factor(Year),data=corncounties, subset = MaxDIndem==0 & NonDZero==1)
temp<-summary(cornbeta, type='deviance')
capture.output(temp,file='Results/cornDbeta.txt')


cornbernzero<-glm(NonDZero ~ Primary.Declared.Drought+D0+D1+D2+D3+D4+lagD0+lagD1+lagD2+lagD3+lagD4 +I(USDCI*LagUSDCI)+ PropIrr + I(PropIrr*D0) +I(PropIrr*D1)+ I(PropIrr*D2) +I(PropIrr*D3)+I(PropIrr*D4)+factor(State) + factor(Year) , family = binomial , data=corncounties)
temp<-summary(cornbernzero)
capture.output(temp,file='Results/FE/cornDbernzero.txt')

cornbernone<-glm(MaxDIndem ~ Primary.Declared.Drought+D0+D1+D2+D3+D4+lagD0+lagD1+lagD2+lagD3+lagD4 +I(USDCI*LagUSDCI)+ PropIrr + I(PropIrr*D0) +I(PropIrr*D1)+ I(PropIrr*D2) +I(PropIrr*D3)+I(PropIrr*D4)+factor(State) + factor(Year) , family = binomial , data=corncounties,subset=NonDZero==1)
temp<-summary(cornbernone)
capture.output(temp,file='Results/FE/cornDbernone.txt')

cornbeta<-betareg(DroughtIndem~Primary.Declared.Drought+D0+D1+D2+D3+D4+lagD0+lagD1+lagD2+lagD3+lagD4 +I(USDCI*LagUSDCI)+ PropIrr + I(PropIrr*D0) +I(PropIrr*D1)+ I(PropIrr*D2) +I(PropIrr*D3)+I(PropIrr*D4)+factor(State) + factor(Year),data=corncounties, subset = MaxDIndem==0 & NonDZero==1)
temp<-summary(cornbeta,type='deviance')
capture.output(temp,file='Results/FE/cornDbeta.txt')

cornbernzero<-glm(IrrNonZero ~ Primary.Declared.Drought+D0+D1+D2+D3+D4+lagD0+lagD1+lagD2+lagD3+lagD4 +I(USDCI*LagUSDCI)+ factor(Year) , family = binomial , data=corncounties )
temp<-summary(cornbernzero)
capture.output(temp,file='Results/cornIrrbernzero.txt')

cornbeta<-betareg(PropIrrFail~Primary.Declared.Drought+D0+D1+D2+D3+D4+lagD0+lagD1+lagD2+lagD3+lagD4 +I(USDCI*LagUSDCI)+ factor(Year),data=corncounties, subset = IrrNonZero==1 & IrrMaxIndem==0 & is.na(PropIrrFail)==FALSE)
temp<-summary(cornbeta, type='deviance')
capture.output(temp,file='Results/cornIrrbeta.txt')


#### SoyBean Regression ####
soybernzero<-glm(NonZero ~ Primary.Declared.Drought+D0+D1+D2+D3+D4+lagD0+lagD1+lagD2+lagD3+lagD4 +I(USDCI*LagUSDCI)+ PropIrr + I(PropIrr*D0) +I(PropIrr*D1)+ I(PropIrr*D2) +I(PropIrr*D3)+I(PropIrr*D4) + factor(Year), family = binomial , data=soycounties )
temp<-summary(soybernzero)
capture.output(temp,file='Results/soybernzero.txt')

soybernone<-glm(MaxIndem ~ Primary.Declared.Drought+D0+D1+D2+D3+D4+lagD0+lagD1+lagD2+lagD3+lagD4 +I(USDCI*LagUSDCI)+ PropIrr + I(PropIrr*D0) +I(PropIrr*D1)+ I(PropIrr*D2) +I(PropIrr*D3)+I(PropIrr*D4) + factor(Year), family = binomial , data=soycounties,subset=NonZero==1 )
temp<-summary(cornbernzero)
capture.output(temp,file='Results/soybernone.txt')

soybeta<-betareg(PropIndem~Primary.Declared.Drought+D0+D1+D2+D3+D4+lagD0+lagD1+lagD2+lagD3+lagD4 +I(USDCI*LagUSDCI)+ PropIrr + I(PropIrr*D0) +I(PropIrr*D1)+ I(PropIrr*D2) +I(PropIrr*D3)+I(PropIrr*D4)+ factor(Year),data=soycounties, subset = MaxIndem==0 & NonZero==1)
temp<-summary(soybeta, type='deviance')
capture.output(temp,file='Results/soybeta.txt')


soybernzero<-glm(NonZero ~ Primary.Declared.Drought+D0+D1+D2+D3+D4+lagD0+lagD1+lagD2+lagD3+lagD4 +I(USDCI*LagUSDCI)+ PropIrr + I(PropIrr*D0) +I(PropIrr*D1)+ I(PropIrr*D2) +I(PropIrr*D3)+I(PropIrr*D4)+factor(State) + factor(Year) , family = binomial , data=soycounties)
temp<-summary(soybernzero)
capture.output(temp,file='Results/FE/soybernzero.txt')

soybernone<-glm(MaxIndem ~ Primary.Declared.Drought+D0+D1+D2+D3+D4+lagD0+lagD1+lagD2+lagD3+lagD4 +I(USDCI*LagUSDCI)+ PropIrr + I(PropIrr*D0) +I(PropIrr*D1)+ I(PropIrr*D2) +I(PropIrr*D3)+I(PropIrr*D4)+factor(State) + factor(Year), family = binomial , data=soycounties,subset=NonZero==1)
temp<-summary(soybernone)
capture.output(temp,file='Results/FE/soybernone.txt')

soybeta<-betareg(PropIndem~Primary.Declared.Drought+D0+D1+D2+D3+D4+lagD0+lagD1+lagD2+lagD3+lagD4 +I(USDCI*LagUSDCI)+ PropIrr + I(PropIrr*D0) +I(PropIrr*D1)+ I(PropIrr*D2) +I(PropIrr*D3)+I(PropIrr*D4)+factor(State) + factor(Year),data=soycounties, subset = MaxIndem==0 & NonZero==1)
temp<-summary(soybeta, type = 'deviance')
capture.output(temp,file='Results/FE/soybeta.txt')


soybernzero<-glm(NonDZero ~ Primary.Declared.Drought+D0+D1+D2+D3+D4+lagD0+lagD1+lagD2+lagD3+lagD4 +I(USDCI*LagUSDCI)+ PropIrr + I(PropIrr*D0) +I(PropIrr*D1)+ I(PropIrr*D2) +I(PropIrr*D3)+I(PropIrr*D4) + factor(Year), family = binomial , data=soycounties )
temp<-summary(soybernzero)
capture.output(temp,file='Results/soyDbernzero.txt')

soybernone<-glm(MaxDIndem ~ Primary.Declared.Drought+D0+D1+D2+D3+D4+lagD0+lagD1+lagD2+lagD3+lagD4 +I(USDCI*LagUSDCI)+ PropIrr + I(PropIrr*D0) +I(PropIrr*D1)+ I(PropIrr*D2) +I(PropIrr*D3)+I(PropIrr*D4) + factor(Year), family = binomial , data=soycounties,subset=NonDZero==1 )
temp<-summary(soybernone)
capture.output(temp,file='Results/soyDbernone.txt')

soybeta<-betareg(DroughtIndem~Primary.Declared.Drought+D0+D1+D2+D3+D4+lagD0+lagD1+lagD2+lagD3+lagD4 +I(USDCI*LagUSDCI)+ PropIrr + I(PropIrr*D0) +I(PropIrr*D1)+ I(PropIrr*D2) +I(PropIrr*D3)+I(PropIrr*D4) + factor(Year),data=soycounties, subset = MaxDIndem==0 & NonDZero==1)
temp<-summary(soybeta, type= 'deviance')
capture.output(temp,file='Results/soyDbeta.txt')


soybernzero<-glm(NonDZero ~ Primary.Declared.Drought+D0+D1+D2+D3+D4+lagD0+lagD1+lagD2+lagD3+lagD4 +I(USDCI*LagUSDCI)+ PropIrr + I(PropIrr*D0) +I(PropIrr*D1)+ I(PropIrr*D2) +I(PropIrr*D3)+I(PropIrr*D4)+factor(State) + factor(Year), family = binomial , data=soycounties)
temp<-summary(soybernzero)
capture.output(temp,file='Results/FE/soyDbernzero.txt')

soybernone<-glm(MaxDIndem ~ Primary.Declared.Drought+D0+D1+D2+D3+D4+lagD0+lagD1+lagD2+lagD3+lagD4 +I(USDCI*LagUSDCI)+ PropIrr + I(PropIrr*D0) +I(PropIrr*D1)+ I(PropIrr*D2) +I(PropIrr*D3)+I(PropIrr*D4)+factor(State) + factor(Year), family = binomial , data=soycounties,subset=NonDZero==1)
temp<-summary(soybernone)
capture.output(temp,file='Results/FE/soyDbernone.txt')

soybeta<-betareg(DroughtIndem~Primary.Declared.Drought+D0+D1+D2+D3+D4+lagD0+lagD1+lagD2+lagD3+lagD4 +I(USDCI*LagUSDCI)+ PropIrr + I(PropIrr*D0) +I(PropIrr*D1)+ I(PropIrr*D2) +I(PropIrr*D3)+I(PropIrr*D4)+factor(State)+ factor(Year),data=soycounties, subset = MaxDIndem==0 & NonDZero==1)
temp<-summary(soybeta, type='deviance')
capture.output(temp,file='Results/FE/soyDbeta.txt')

soybernzero<-glm(IrrNonZero ~ Primary.Declared.Drought+D0+D1+D2+D3+D4+lagD0+lagD1+lagD2+lagD3+lagD4 +I(USDCI*LagUSDCI)+ factor(Year) , family = binomial , data=soycounties )
temp<-summary(soybernzero)
capture.output(temp,file='Results/soyIrrbernzero.txt')

soybeta<-betareg(PropIrrFail~Primary.Declared.Drought+D0+D1+D2+D3+D4+lagD0+lagD1+lagD2+lagD3+lagD4 +I(USDCI*LagUSDCI)+ factor(Year),data=soycounties, subset = IrrNonZero==1 & IrrMaxIndem==0 & is.na(PropIrrFail)==FALSE)
temp<-summary(soybeta, type='deviance')
capture.output(temp,file='Results/soyIrrbeta.txt')
