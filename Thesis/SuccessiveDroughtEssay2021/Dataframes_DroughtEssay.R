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
library(coefplot)
library(prism)
library(raster)
library(sp)
library(rgdal)

# Sys.setenv(JAGS_HOME='D:/JAGS-4.3.0')
# library(rjags)
# library(zoib)
#### Price Read In ####



#### Drought Monitor Load In ####
file.path = "D:/DroughtMonitor/"

file.name = paste0(file.path,"dm_export_20050101_20211115.csv")
file.name.2 = paste0(file.path,"dm_export_19980101_20041231.csv")


DroughtMonitor1<-read.csv(file=file.name, stringsAsFactors = FALSE)
DroughtMonitor2<-read.csv(file=file.name.2,stringsAsFactors = FALSE)  
DroughtMonitor = rbind(DroughtMonitor1,DroughtMonitor2)
remove(DroughtMonitor1,DroughtMonitor2)
DroughtMonitor$State<-substr(DroughtMonitor$FIPS,1,2)
DroughtMonitor<-DroughtMonitor[DroughtMonitor$State == '17' | DroughtMonitor$State == '18' | DroughtMonitor$State == '19'  | DroughtMonitor$State == '20' | DroughtMonitor$State == '26' | DroughtMonitor$State == '27' |DroughtMonitor$State == '29' | DroughtMonitor$State == '31' |DroughtMonitor$State == '38' | DroughtMonitor$State == '39' | DroughtMonitor$State == '42' | DroughtMonitor$State == '46' | DroughtMonitor$State == '55' ,]
DroughtMonitor$State<-NA
# 
# D2data<-DroughtMonitor[,c(2,8,11)]
# D2data$weeks<-ymd(D2data$ValidStart)
# D2data<-D2data[order(D2data$FIPS,D2data$weeks),]
# D2data$Year<-year(D2data$weeks)
# D2data$ID<-as.numeric(as.factor(with(D2data,paste(FIPS,Year,sep="_"))))
# 
# FUN <- function(x, negate = FALSE, na.rm = FALSE) {
#   rles <- rle(x > 0)
#   if(negate) {
#     max(rles$lengths[!rles$values], na.rm = na.rm)
#   } else {
#     max(rles$lengths[rles$values], na.rm = na.rm)
#   }
# }
# wins <- lapply(split(D2data$D2,D2data$ID), FUN)
# series<-as.data.frame(wins)
# sw<-pivot_longer(series,cols=everything(),values_to ="Value",names_to = 'ID')
# sw$ID<-as.numeric(sub('^.','',sw$ID))
# D2data<-merge(D2data,sw,by=c('ID'))
# temp<-unique(D2data[,c(2,6,7)])
# temp$Value<-ifelse(temp$Value>=8,1,0)
# temp<-temp%>%
#   rename(
#     D28weeks=Value
#   )
# D2weeksyn<-temp

DroughtMonitor$Begin.Day<-as.integer(substr(DroughtMonitor$ValidStart,9,10))
DroughtMonitor$End.Day<-as.integer(substr(DroughtMonitor$ValidEnd,9,10))
DroughtMonitor$Begin.Month<-as.integer(substr(DroughtMonitor$ValidStart,6,7))
DroughtMonitor$End.Month<-as.integer(substr(DroughtMonitor$ValidEnd,6,7))
DroughtMonitor$Begin.Year<-as.integer(substr(DroughtMonitor$ValidStart,1,4))
DroughtMonitor$End.Year<-as.integer(substr(DroughtMonitor$ValidEnd,1,4))



DroughtMonitor$Begin.Weight = if_else(DroughtMonitor$Begin.Month==1 | DroughtMonitor$Begin.Month==3 | DroughtMonitor$Begin.Month==5 | DroughtMonitor$Begin.Month==7 | DroughtMonitor$Begin.Month==8 | DroughtMonitor$Begin.Month==10 | DroughtMonitor$Begin.Month==12 ,32-DroughtMonitor$Begin.Day,31-DroughtMonitor$Begin.Day )
DroughtMonitor$Begin.Weight = if_else(DroughtMonitor$Begin.Month==2,29-DroughtMonitor$Begin.Day,DroughtMonitor$Begin.Weight )
DroughtMonitor$Begin.Weight = if_else(DroughtMonitor$Begin.Month==2 & leap_year(DroughtMonitor$Begin.Year)==TRUE,30-DroughtMonitor$Begin.Day,DroughtMonitor$Begin.Weight )
DroughtMonitor$Begin.Weight= if_else(DroughtMonitor$Begin.Weight>7,7,DroughtMonitor$Begin.Weight)
DroughtMonitor$End.Weight=7-DroughtMonitor$Begin.Weight

DroughtMonitor$BeginNone= DroughtMonitor$None*DroughtMonitor$Begin.Weight
DroughtMonitor$BeginD0= DroughtMonitor$D0*DroughtMonitor$Begin.Weight
DroughtMonitor$BeginD1= DroughtMonitor$D1*DroughtMonitor$Begin.Weight
DroughtMonitor$BeginD2= DroughtMonitor$D2*DroughtMonitor$Begin.Weight
DroughtMonitor$BeginD3= DroughtMonitor$D3*DroughtMonitor$Begin.Weight
DroughtMonitor$BeginD4= DroughtMonitor$D4*DroughtMonitor$Begin.Weight
DroughtMonitor$EndNone= DroughtMonitor$None*DroughtMonitor$End.Weight
DroughtMonitor$EndD0= DroughtMonitor$D0*DroughtMonitor$End.Weight
DroughtMonitor$EndD1= DroughtMonitor$D1*DroughtMonitor$End.Weight
DroughtMonitor$EndD2= DroughtMonitor$D2*DroughtMonitor$End.Weight
DroughtMonitor$EndD3= DroughtMonitor$D3*DroughtMonitor$End.Weight
DroughtMonitor$EndD4= DroughtMonitor$D4*DroughtMonitor$End.Weight

# DroughtMonitor$BeginD0Count= if_else(DroughtMonitor$D0>30,1,0)
# DroughtMonitor$BeginD0FullCount= if_else(DroughtMonitor$D0>50,1,0)
# DroughtMonitor$BeginD1Count= if_else(DroughtMonitor$D1>30,1,0)
# DroughtMonitor$BeginD1FullCount= if_else(DroughtMonitor$D1>50,1,0)
# DroughtMonitor$BeginD2Count= if_else(DroughtMonitor$D2>30,1,0)
# DroughtMonitor$BeginD2FullCount= if_else(DroughtMonitor$D2>50,1,0)
# DroughtMonitor$BeginD3Count= if_else(DroughtMonitor$D3>30,1,0)
# DroughtMonitor$BeginD3FullCount= if_else(DroughtMonitor$D3>50,1,0)
# DroughtMonitor$BeginD4Count= if_else(DroughtMonitor$D4>30,1,0)
# DroughtMonitor$BeginD4FullCount= if_else(DroughtMonitor$D4>50,1,0)


BeginDroughtMonitor <- aggregate(list( DroughtMonitor$BeginNone, DroughtMonitor$BeginD0, DroughtMonitor$BeginD1, DroughtMonitor$BeginD2,DroughtMonitor$BeginD3,DroughtMonitor$BeginD4), by = list(DroughtMonitor$Begin.Month, DroughtMonitor$Begin.Year,DroughtMonitor$FIPS), sum)  
colnames<-list("Month","Year","FIPS","BNone","BD0","BD1","BD2","BD3","BD4")
colnames(BeginDroughtMonitor)<-colnames

EndDroughtMonitor <- aggregate(list(DroughtMonitor$EndNone, DroughtMonitor$EndD0, DroughtMonitor$EndD1, DroughtMonitor$EndD2,DroughtMonitor$EndD3,DroughtMonitor$EndD4), by = list(DroughtMonitor$End.Month, DroughtMonitor$End.Year,DroughtMonitor$FIPS), sum)  
colnames<-list("Month","Year","FIPS","ENone","ED0","ED1","ED2","ED3","ED4")
colnames(EndDroughtMonitor)<-colnames

DroughtMonitor<-merge(BeginDroughtMonitor,EndDroughtMonitor, by = c("Month","Year","FIPS"))
rm(BeginDroughtMonitor,EndDroughtMonitor)

# 
# DroughtMonitor$MonthDays = ifelse(DroughtMonitor$Month==1 | DroughtMonitor$Month==3 | DroughtMonitor$Month==5 | DroughtMonitor$Month==7 | DroughtMonitor$Month==8 | DroughtMonitor$Month==10 | DroughtMonitor$Month==12,31,30 )
# DroughtMonitor$MonthDays = if_else(DroughtMonitor$Month==2,28,DroughtMonitor$MonthDays )
# DroughtMonitor$MonthDays = if_else(DroughtMonitor$Month==2 & leap_year(DroughtMonitor$Year)==TRUE,29,DroughtMonitor$MonthDays )
# DroughtMonitor$MonthDays = if_else(DroughtMonitor$Month==1 & DroughtMonitor$Year==2000,28,DroughtMonitor$MonthDays )

DroughtMonitor$None<-(DroughtMonitor$BNone+DroughtMonitor$ENone)
DroughtMonitor$D0<-(DroughtMonitor$BD0+DroughtMonitor$ED0)
DroughtMonitor$D1<-(DroughtMonitor$BD1+DroughtMonitor$ED1)
DroughtMonitor$D2<-(DroughtMonitor$BD2+DroughtMonitor$ED2)
DroughtMonitor$D3<-(DroughtMonitor$BD3+DroughtMonitor$ED3)
DroughtMonitor$D4<-(DroughtMonitor$BD4+DroughtMonitor$ED4)




DroughtMonitor<-DroughtMonitor[,c(1:3,16:21)]
DroughtMonitor<-DroughtMonitor %>%
  rename(
    DNone=None
  )

DroughtMonitor$FIPS<-as.character(DroughtMonitor$FIPS)
DroughtMonitor$FIPS<-str_pad(DroughtMonitor$FIPS, 5, pad = "0")
DroughtMonitor$FIPS<-trimws(DroughtMonitor$FIPS, which=c("both"))

DroughtMonitor$GrainCornYear=ifelse(DroughtMonitor$Month==12,DroughtMonitor$Year+1,DroughtMonitor$Year)

GrainCornYearlyDrought<-aggregate(cbind(DNone,D0,D1,D2,D3,D4)~ FIPS + GrainCornYear,data=DroughtMonitor,sum,na.rm = TRUE)
GrainCornYearlyDrought$DNone<-if_else(leap_year(GrainCornYearlyDrought$GrainCornYear)==TRUE,GrainCornYearlyDrought$DNone/366,GrainCornYearlyDrought$DNone/365 )
GrainCornYearlyDrought$D0<-if_else(leap_year(GrainCornYearlyDrought$GrainCornYear)==TRUE,GrainCornYearlyDrought$D0/366,GrainCornYearlyDrought$D0/365 )
GrainCornYearlyDrought$D1<-if_else(leap_year(GrainCornYearlyDrought$GrainCornYear)==TRUE,GrainCornYearlyDrought$D1/366,GrainCornYearlyDrought$D1/365 )
GrainCornYearlyDrought$D2<-if_else(leap_year(GrainCornYearlyDrought$GrainCornYear)==TRUE,GrainCornYearlyDrought$D2/366,GrainCornYearlyDrought$D2/365 )
GrainCornYearlyDrought$D3<-if_else(leap_year(GrainCornYearlyDrought$GrainCornYear)==TRUE,GrainCornYearlyDrought$D3/366,GrainCornYearlyDrought$D3/365 )
GrainCornYearlyDrought$D4<-if_else(leap_year(GrainCornYearlyDrought$GrainCornYear)==TRUE,GrainCornYearlyDrought$D4/366,GrainCornYearlyDrought$D4/365 )

GrainCornYearlyDrought<-GrainCornYearlyDrought[!(GrainCornYearlyDrought$GrainCornYear==2000),]
GrainCornYearlyDrought<-GrainCornYearlyDrought %>%
  rename(
    Year=GrainCornYear
  )

DroughtMonitor$TotalMonth= 12*DroughtMonitor$Year + DroughtMonitor$Month

GrainCornMonthlyDrought<-aggregate(cbind( DNone,D0,D1,D2,D3,D4)~ FIPS + TotalMonth,data=DroughtMonitor,sum,na.rm = TRUE)
GrainCornMonthlyDrought$Month<-GrainCornMonthlyDrought$TotalMonth%%12
GrainCornMonthlyDrought$Year<-GrainCornMonthlyDrought$TotalMonth%/%12
GrainCornMonthlyDrought$DNone<-if_else(GrainCornMonthlyDrought$Month==4 |GrainCornMonthlyDrought$Month==6 |GrainCornMonthlyDrought$Month==9 |GrainCornMonthlyDrought$Month==11  ,GrainCornMonthlyDrought$DNone/30,GrainCornMonthlyDrought$DNone/31 )
GrainCornMonthlyDrought$D0<-if_else(GrainCornMonthlyDrought$Month==4 |GrainCornMonthlyDrought$Month==6 |GrainCornMonthlyDrought$Month==9 |GrainCornMonthlyDrought$Month==11  ,GrainCornMonthlyDrought$D0/30,GrainCornMonthlyDrought$D0/31 )
GrainCornMonthlyDrought$D1<-if_else(GrainCornMonthlyDrought$Month==4 |GrainCornMonthlyDrought$Month==6 |GrainCornMonthlyDrought$Month==9 |GrainCornMonthlyDrought$Month==11  ,GrainCornMonthlyDrought$D1/30,GrainCornMonthlyDrought$D1/31 )
GrainCornMonthlyDrought$D2<-if_else(GrainCornMonthlyDrought$Month==4 |GrainCornMonthlyDrought$Month==6 |GrainCornMonthlyDrought$Month==9 |GrainCornMonthlyDrought$Month==11  ,GrainCornMonthlyDrought$D2/30,GrainCornMonthlyDrought$D2/31 )
GrainCornMonthlyDrought$D3<-if_else(GrainCornMonthlyDrought$Month==4 |GrainCornMonthlyDrought$Month==6 |GrainCornMonthlyDrought$Month==9 |GrainCornMonthlyDrought$Month==11  ,GrainCornMonthlyDrought$D3/30,GrainCornMonthlyDrought$D3/31 )
GrainCornMonthlyDrought$D4<-if_else(GrainCornMonthlyDrought$Month==4 |GrainCornMonthlyDrought$Month==6 |GrainCornMonthlyDrought$Month==9 |GrainCornMonthlyDrought$Month==11  ,GrainCornMonthlyDrought$D4/30,GrainCornMonthlyDrought$D4/31 )

GrainCornMonthlyDrought$DNone<-if_else(GrainCornMonthlyDrought$Month==2, if_else(leap_year(GrainCornMonthlyDrought$Year)==TRUE,GrainCornMonthlyDrought$DNone*(31/28),GrainCornMonthlyDrought$DNone*(31/29)  ),GrainCornMonthlyDrought$DNone  )
GrainCornMonthlyDrought$D0<-if_else(GrainCornMonthlyDrought$Month==2, if_else(leap_year(GrainCornMonthlyDrought$Year)==TRUE,GrainCornMonthlyDrought$D0*(31/28),GrainCornMonthlyDrought$D0*(31/29)  ),GrainCornMonthlyDrought$D0  )
GrainCornMonthlyDrought$D1<-if_else(GrainCornMonthlyDrought$Month==2, if_else(leap_year(GrainCornMonthlyDrought$Year)==TRUE,GrainCornMonthlyDrought$D1*(31/28),GrainCornMonthlyDrought$D1*(31/29)  ),GrainCornMonthlyDrought$D1  )
GrainCornMonthlyDrought$D2<-if_else(GrainCornMonthlyDrought$Month==2, if_else(leap_year(GrainCornMonthlyDrought$Year)==TRUE,GrainCornMonthlyDrought$D2*(31/28),GrainCornMonthlyDrought$D2*(31/29)  ),GrainCornMonthlyDrought$D2  )
GrainCornMonthlyDrought$D3<-if_else(GrainCornMonthlyDrought$Month==2, if_else(leap_year(GrainCornMonthlyDrought$Year)==TRUE,GrainCornMonthlyDrought$D3*(31/28),GrainCornMonthlyDrought$D3*(31/29)  ),GrainCornMonthlyDrought$D3  )
GrainCornMonthlyDrought$D4<-if_else(GrainCornMonthlyDrought$Month==2, if_else(leap_year(GrainCornMonthlyDrought$Year)==TRUE,GrainCornMonthlyDrought$D4*(31/28),GrainCornMonthlyDrought$D4*(31/29)  ),GrainCornMonthlyDrought$D4  )

GrainCornMonthlyDrought$GrainCornYear=ifelse(GrainCornMonthlyDrought$Month==12,GrainCornMonthlyDrought$Year+1,GrainCornMonthlyDrought$Year)
GrainCornMonthlyDrought$MonthofSeason<-if_else(GrainCornMonthlyDrought$Month==12,1,GrainCornMonthlyDrought$Month+1)

GrainCornMonthlyDrought<-GrainCornMonthlyDrought[,c('FIPS','GrainCornYear','MonthofSeason','D0','D1','D2','D3','D4')]
GrainCornMonthlyDrought<-GrainCornMonthlyDrought%>%
  rename(
    Year=GrainCornYear
  )



#1 = fallow, 2 = plant, 4 = harvest
#basically the same, and follow winter, spring etc. except soybeans typically planted a little later... not in march
DroughtMonitor$Season<-3
DroughtMonitor$Season[DroughtMonitor$Month==12 | DroughtMonitor$Month==1 | DroughtMonitor$Month==2]<-1
DroughtMonitor$Season[DroughtMonitor$Month==3 | DroughtMonitor$Month==4 | DroughtMonitor$Month==5]<-2
DroughtMonitor$Season[DroughtMonitor$Month==9 | DroughtMonitor$Month==10 | DroughtMonitor$Month==11]<-4


GrainCornSeasonalDrought<-aggregate(cbind(DNone,D0,D1,D2,D3,D4)~ FIPS + Season + GrainCornYear,data=DroughtMonitor,sum,na.rm = TRUE)

GrainCornSeasonalDrought$DNone<-if_else(GrainCornSeasonalDrought$Season==4 |GrainCornSeasonalDrought$Season==1  , if_else(GrainCornSeasonalDrought$Season==1 & leap_year(GrainCornSeasonalDrought$GrainCornYear)==FALSE,GrainCornSeasonalDrought$DNone/90,GrainCornSeasonalDrought$DNone/91  ),GrainCornSeasonalDrought$DNone/92  )
GrainCornSeasonalDrought$D0<-if_else(GrainCornSeasonalDrought$Season==4 |GrainCornSeasonalDrought$Season==1  , if_else(GrainCornSeasonalDrought$Season==1 & leap_year(GrainCornSeasonalDrought$GrainCornYear)==FALSE,GrainCornSeasonalDrought$D0/90,GrainCornSeasonalDrought$D0/91  ),GrainCornSeasonalDrought$D0/92  )
GrainCornSeasonalDrought$D1<-if_else(GrainCornSeasonalDrought$Season==4 |GrainCornSeasonalDrought$Season==1  , if_else(GrainCornSeasonalDrought$Season==1 & leap_year(GrainCornSeasonalDrought$GrainCornYear)==FALSE,GrainCornSeasonalDrought$D1/90,GrainCornSeasonalDrought$D1/91  ),GrainCornSeasonalDrought$D1/92  )
GrainCornSeasonalDrought$D2<-if_else(GrainCornSeasonalDrought$Season==4 |GrainCornSeasonalDrought$Season==1  , if_else(GrainCornSeasonalDrought$Season==1 & leap_year(GrainCornSeasonalDrought$GrainCornYear)==FALSE,GrainCornSeasonalDrought$D2/90,GrainCornSeasonalDrought$D2/91  ),GrainCornSeasonalDrought$D2/92  )
GrainCornSeasonalDrought$D3<-if_else(GrainCornSeasonalDrought$Season==4 |GrainCornSeasonalDrought$Season==1  , if_else(GrainCornSeasonalDrought$Season==1 & leap_year(GrainCornSeasonalDrought$GrainCornYear)==FALSE,GrainCornSeasonalDrought$D3/90,GrainCornSeasonalDrought$D3/91  ),GrainCornSeasonalDrought$D3/92  )
GrainCornSeasonalDrought$D4<-if_else(GrainCornSeasonalDrought$Season==4 |GrainCornSeasonalDrought$Season==1  , if_else(GrainCornSeasonalDrought$Season==1 & leap_year(GrainCornSeasonalDrought$GrainCornYear)==FALSE,GrainCornSeasonalDrought$D4/90,GrainCornSeasonalDrought$D4/91  ),GrainCornSeasonalDrought$D4/92  )
GrainCornSeasonalDrought<- GrainCornSeasonalDrought%>%
  rename(
    Year=GrainCornYear
  )
GrainCornSeasonalDrought$DNone<-NULL

DroughtMonthly<-reshape(data=GrainCornMonthlyDrought, idvar=c('FIPS','Year'),v.names=c('D0','D1','D2','D3','D4'),timevar="MonthofSeason",direction='wide')
DroughtMonthly[is.na(DroughtMonthly)]<-0
DroughtSeasonally<-reshape(data=GrainCornSeasonalDrought, idvar=c('FIPS','Year'),v.names=c('D0','D1','D2','D3','D4'),timevar="Season",direction='wide')
DroughtSeasonally[is.na(DroughtSeasonally)]<-0
# GrainCornYearlyDrought<-merge(GrainCornYearlyDrought,D2weeksyn,by=c('FIPS','Year'))
# GrainCornYearlyDrought$FSAeligible<-if_else(GrainCornYearlyDrought$D28weeks==1 | GrainCornYearlyDrought$D3>0 |  GrainCornYearlyDrought$D4>0 ,1,0)
GrainCornYearlyDrought<-GrainCornYearlyDrought[order(GrainCornYearlyDrought$FIPS),]
GrainCornYearlyDrought<-GrainCornYearlyDrought %>%
  group_by(FIPS)%>%
  mutate(lagD0 = dplyr:: lag(D0,n=1,default=NA))%>%
  mutate(lagD1 = dplyr:: lag(D1,n=1,default=NA))%>%
  mutate(lagD2 = dplyr:: lag(D2,n=1,default=NA))%>%
  mutate(lagD3 = dplyr:: lag(D3,n=1,default=NA))%>%
  mutate(lagD4 = dplyr:: lag(D4,n=1,default=NA))

GrainCornYearlyDrought$State<-substr(GrainCornYearlyDrought$FIPS,1,2)
GrainCornYearlyDrought<-GrainCornYearlyDrought[GrainCornYearlyDrought$State == '17' | GrainCornYearlyDrought$State == '18' | GrainCornYearlyDrought$State == '19'  | GrainCornYearlyDrought$State == '20' | GrainCornYearlyDrought$State == '26' | GrainCornYearlyDrought$State == '27'| GrainCornYearlyDrought$State == '29' | GrainCornYearlyDrought$State == '31' | GrainCornYearlyDrought$State == '38'| GrainCornYearlyDrought$State == '39' | GrainCornYearlyDrought$State == '42' | GrainCornYearlyDrought$State == '46' | GrainCornYearlyDrought$State == '55' ,]

temp<-summary(GrainCornYearlyDrought)
capture.output(temp,file='D:/DroughtEssay/SummaryStats/USDM.txt')
# GrainCornYearlyDrought$State<-as.numeric(GrainCornYearlyDrought$State)
# dt <- data.table(GrainCornYearlyDrought)
# dt[,list(mean=mean(age),sd=sd(age)),by=group]

GrainCornYearlyDrought$ExtremeDroughtOccur<-ifelse((GrainCornYearlyDrought$D4+GrainCornYearlyDrought$D3)>1 ,1,0)
GrainCornYearlyDrought$DroughtOccur<-ifelse((GrainCornYearlyDrought$D4+GrainCornYearlyDrought$D3+GrainCornYearlyDrought$D2)>1 ,1,0)
GrainCornYearlyDrought<-GrainCornYearlyDrought[order(GrainCornYearlyDrought$FIPS,GrainCornYearlyDrought$Year),]
GrainCornYearlyDrought<-GrainCornYearlyDrought %>%
  group_by(FIPS)%>%
  mutate(lagDroughtOccur = dplyr:: lag(DroughtOccur,n=1,default=NA))%>%
  mutate(lagExtremeDroughtOccur = dplyr:: lag(ExtremeDroughtOccur,n=1,default=NA))

droughtrim<-GrainCornYearlyDrought[,c('State','D0','D1','D2','D3','D4','DroughtOccur','ExtremeDroughtOccur')]
test<-aggregate( x=droughtrim[,colnames(droughtrim) != "State"], by = list(droughtrim$State),FUN= 'quantile',probs=c(0.75,0.9,0.95))
droughtoccurence<-aggregate( cbind(DroughtOccur,ExtremeDroughtOccur)~ FIPS ,data=GrainCornYearlyDrought,sum,na.rm = TRUE)
#droughtoccurence$Dcolorcode<-rgb(droughtoccurence$DroughtOccur,0,0,maxColorValue=21)

##### FSA #####
file.path = "D:/FSA_Disaster/"

file.name = paste(file.path,'METADATA_CY',c(2012:2021),".txt", sep = "")


FSADisaster2012= read.table(file.name[1], colClasses= "character", header=TRUE, sep="\t" ,fill = TRUE,flush = TRUE, quote ="", blank.lines.skip = FALSE)
FSADisaster2012<- FSADisaster2012 %>%
  rename(
    Designation.Code=X.Designation.Code..1...primary..2...contiguous..,
    Flood=X.FLOOD..Flash.flooding.,
    Rain.Humidity=X.Excessive.rain..moisture..humidity.,
    Thunderstorms=X.Severe.Storms..thunderstorms.,
    Ground.Saturation=X.Ground.Saturation.Standing.Water.,
    Wind=X.Wind..High.Winds.,
    Fire=X.Fire..Wildfire.,
    Heat=X.Heat..Excessive.heat.High.temp...incl..low.humidity..,
    Winterstorms=X.Winter.Storms..Ice.Storms..Snow..Blizzard.,
    Freeze=X.Frost..FREEZE.,
    Hurricanes=X.Hurricanes..Typhoons..Tropical.Storms.,
    Mudslides=X.Mudslides..Debris.Flows..Landslides.,
    Cold.Wet=X.Cold..wet.weather.,
    Cool.Temp=X.Cool.Cold..Below.normal.Temperatures.,
    Description.of.disaster=Description.of.Disaster
  )
FSADisaster2012$Year=2012
FSADisaster2013= read.table(file.name[2], colClasses= "character", header=TRUE, sep="\t" ,fill = TRUE,flush = TRUE, quote ="", blank.lines.skip = FALSE)
FSADisaster2013<- FSADisaster2013 %>%
  rename(
    Designation.Code=Desig_Cd,
    Designation.Number=Desig..No.,
    Flood=X.FLOOD..Flash.flooding.,
    Rain.Humidity=X.Excessive.rain..moisture..humidity.,
    Thunderstorms=X.Severe.Storms..thunderstorms.,
    Ground.Saturation=X.Ground.Saturation.Standing.Water.,
    Wind=X.Wind..High.Winds.,
    Fire=X.Fire..Wildfire.,
    Heat=X.Heat..Excessive.heat.High.temp...incl..low.humidity..,
    Winterstorms=X.Winter.Storms..Ice.Storms..Snow..Blizzard.,
    Freeze=X.Frost..FREEZE.,
    Hurricanes=X.Hurricanes..Typhoons..Tropical.Storms.,
    Volcano=X.Volcano....VOG...,
    Mudslides=X.Mudslides..Debris.Flows..Landslides.,
    Cold.Wet=X.Cold..wet.weather.,
    Cool.Temp=X.Cool.Cold..Below.normal.Temperatures.,
    CROP.DISASTER.YEAR=Crop.Year
  )
FSADisaster2013$Year=2013
FSADisaster2014= read.table(file.name[3], colClasses= "character", header=TRUE, sep="\t" ,fill = TRUE,flush = TRUE, quote ="", blank.lines.skip = FALSE)
FSADisaster2014<- FSADisaster2014 %>%
  rename(
    Flood=X.FLOOD..Flash.flooding.,
    Rain.Humidity=X.Excessive.rain..moisture..humidity.,
    Thunderstorms=X.Severe.Storms..thunderstorms.,
    Ground.Saturation=X.Ground.Saturation.Standing.Water.,
    Wind=X.Wind..High.Winds.,
    Fire=X.Fire..Wildfire.,
    Heat=X.Heat..Excessive.heat.High.temp...incl..low.humidity..,
    Winterstorms=X.Winter.Storms..Ice.Storms..Snow..Blizzard.,
    Freeze=X.Frost..FREEZE.,
    Hurricanes=X.Hurricanes..Typhoons..Tropical.Storms.,
    Mudslides=X.Mudslides..Debris.Flows..Landslides.,
    Cold.Wet=X.Cold..wet.weather.,
    Cool.Temp=X.Cool.Cold..Below.normal.Temperatures.,
  )

FSADisaster2014$X<- NULL
FSADisaster2014$X.1<-NULL

FSADisaster2014$Year=2014
FSADisaster2015= read.table(file.name[4], colClasses= "character", header=TRUE, sep="\t" ,fill = TRUE,flush = TRUE, quote ="", blank.lines.skip = FALSE)

FSADisaster2015<- FSADisaster2015 %>%
  rename(
    Flood=X.FLOOD..Flash.flooding.,
    Rain.Humidity=X.Excessive.rain..moisture..humidity.,
    Thunderstorms=X.Severe.Storms..thunderstorms.,
    Ground.Saturation=X.Ground.Saturation.Standing.Water.,
    Wind=X.Wind..High.Winds.,
    Fire=X.Fire..Wildfire.,
    Heat=X.Heat..Excessive.heat.High.temp...incl..low.humidity..,
    Winterstorms=X.Winter.Storms..Ice.Storms..Snow..Blizzard.,
    Freeze=X.Frost..FREEZE.,
    Hurricanes=X.Hurricanes..Typhoons..Tropical.Storms.,
    Mudslides=X.Mudslides..Debris.Flows..Landslides.,
    Cold.Wet=X.Cold..wet.weather.,
    Cool.Temp=X.Cool.Cold..Below.normal.Temperatures.,
  )
FSADisaster2015$X<- NULL
FSADisaster2015$X.1<-NULL

FSADisaster2015$Year=2015

FSADisaster2016= read.table(file.name[5], colClasses= "character", header=TRUE, sep="\t" ,fill = TRUE,flush = TRUE, quote ="", blank.lines.skip = FALSE)

FSADisaster2016<- FSADisaster2016 %>%
  rename(
    Flood=X.FLOOD..Flash.flooding.,
    Rain.Humidity=X.Excessive.rain..moisture..humidity.,
    Thunderstorms=X.Severe.Storms..thunderstorms.,
    Ground.Saturation=X.Ground.Saturation.Standing.Water.,
    Wind=X.Wind..High.Winds.,
    Fire=X.Fire..Wildfire.,
    Heat=X.Heat..Excessive.heat.High.temp...incl..low.humidity..,
    Winterstorms=X.Winter.Storms..Ice.Storms..Snow..Blizzard.,
    Freeze=X.Frost..FREEZE.,
    Hurricanes=X.Hurricanes..Typhoons..Tropical.Storms.,
    Mudslides=X.Mudslides..Debris.Flows..Landslides.,
    Cold.Wet=X.Cold..wet.weather.,
    Cool.Temp=X.Cool.Cold..Below.normal.Temperatures.,
  )
FSADisaster2016$X<- NULL
FSADisaster2016$X.1<-NULL
FSADisaster2016$Year=2016

FSADisaster2017= read.table(file.name[6], colClasses= "character", header=TRUE, sep="\t" ,fill = TRUE,flush = TRUE, quote ="", blank.lines.skip = FALSE)

FSADisaster2017<- FSADisaster2017 %>%
  rename(
    Flood=X.FLOOD..Flash.flooding.,
    Rain.Humidity=X.Excessive.rain..moisture..humidity.,
    Thunderstorms=X.Severe.Storms..thunderstorms.,
    Ground.Saturation=X.Ground.Saturation.Standing.Water.,
    Wind=X.Wind..High.Winds.,
    Fire=X.Fire..Wildfire.,
    Heat=X.Heat..Excessive.heat.High.temp...incl..low.humidity..,
    Winterstorms=X.Winter.Storms..Ice.Storms..Snow..Blizzard.,
    Freeze=X.Frost..FREEZE.,
    Hurricanes=X.Hurricanes..Typhoons..Tropical.Storms.,
    Mudslides=X.Mudslides..Debris.Flows..Landslides.,
    Cold.Wet=X.Cold..wet.weather.,
    Cool.Temp=X.Cool.Cold..Below.normal.Temperatures.,
  )
FSADisaster2017$X<- NULL
FSADisaster2017$X.1<-NULL
FSADisaster2017$Year=2017

FSADisaster2018= read.table(file.name[7], colClasses= "character", header=TRUE, sep="\t" ,fill = TRUE,flush = TRUE, quote ="", blank.lines.skip = FALSE)

FSADisaster2018<- FSADisaster2018 %>%
  rename(
    Flood=X.FLOOD..Flash.flooding.,
    Rain.Humidity=X.Excessive.rain..moisture..humidity.,
    Thunderstorms=X.Severe.Storms..thunderstorms.,
    Ground.Saturation=X.Ground.Saturation.Standing.Water.,
    Wind=X.Wind..High.Winds.,
    Fire=X.Fire..Wildfire.,
    Heat=X.Heat..Excessive.heat.High.temp...incl..low.humidity..,
    Winterstorms=X.Winter.Storms..Ice.Storms..Snow..Blizzard.,
    Freeze=X.Frost..FREEZE.,
    Hurricanes=X.Hurricanes..Typhoons..Tropical.Storms.,
    Mudslides=X.Mudslides..Debris.Flows..Landslides.,
    Cold.Wet=X.Cold..wet.weather.,
    Cool.Temp=X.Cool.Cold..Below.normal.Temperatures.,
  )
FSADisaster2018$X<- NULL
FSADisaster2018$X.1<-NULL
FSADisaster2018$Year=2018


FSADisaster2019= read.table(file.name[8], colClasses= "character", header=TRUE, sep="\t" ,fill = TRUE,flush = TRUE, quote ="", blank.lines.skip = FALSE)

FSADisaster2019<- FSADisaster2019 %>%
  rename(
    Flood=X.FLOOD..Flash.flooding.,
    Rain.Humidity=X.Excessive.rain..moisture..humidity.,
    Thunderstorms=X.Severe.Storms..thunderstorms.,
    Ground.Saturation=X.Ground.Saturation.Standing.Water.,
    Wind=X.Wind..High.Winds.,
    Fire=X.Fire..Wildfire.,
    Heat=X.Heat..Excessive.heat.High.temp...incl..low.humidity..,
    Winterstorms=X.Winter.Storms..Ice.Storms..Snow..Blizzard.,
    Freeze=X.Frost..FREEZE.,
    Hurricanes=X.Hurricanes..Typhoons..Tropical.Storms.,
    Mudslides=X.Mudslides..Debris.Flows..Landslides.,
    Cold.Wet=X.Cold..wet.weather.,
    Cool.Temp=X.Cool.Cold..Below.normal.Temperatures.,
  )
FSADisaster2019$Year=2019
FSADisaster2020= read.table(file.name[9], colClasses= "character", header=TRUE, sep="\t" ,fill = TRUE,flush = TRUE, quote ="", blank.lines.skip = FALSE)

FSADisaster2020<- FSADisaster2020 %>%
  rename(
    Flood=X.FLOOD..Flash.flooding.,
    Rain.Humidity=X.Excessive.rain..moisture..humidity.,
    Thunderstorms=X.Severe.Storms..thunderstorms.,
    Ground.Saturation=X.Ground.Saturation.Standing.Water.,
    Wind=X.Wind..High.Winds.,
    Fire=X.Fire..Wildfire.,
    Heat=X.Heat..Excessive.heat.High.temp...incl..low.humidity..,
    Winterstorms=X.Winter.Storms..Ice.Storms..Snow..Blizzard.,
    Freeze=X.Frost..FREEZE.,
    Hurricanes=X.Hurricanes..Typhoons..Tropical.Storms.,
    Mudslides=X.Mudslides..Debris.Flows..Landslides.,
    Cold.Wet=X.Cold..wet.weather.,
    Cool.Temp=X.Cool.Cold..Below.normal.Temperatures.,
  )
FSADisaster2020$Year=2020
FSADisaster2021= read.table(file.name[10], colClasses= "character", header=TRUE, sep="\t" ,fill = TRUE,flush = TRUE, quote ="", blank.lines.skip = FALSE)

FSADisaster2021<- FSADisaster2021 %>%
  rename(
    Flood=X.FLOOD..Flash.flooding.,
    Rain.Humidity=X.Excessive.rain..moisture..humidity.,
    Thunderstorms=X.Severe.Storms..thunderstorms.,
    Ground.Saturation=X.Ground.Saturation.Standing.Water.,
    Wind=X.Wind..High.Winds.,
    Fire=X.Fire..Wildfire.,
    Heat=X.Heat..Excessive.heat.High.temp...incl..low.humidity..,
    Winterstorms=X.Winter.Storms..Ice.Storms..Snow..Blizzard.,
    Freeze=X.Frost..FREEZE.,
    Hurricanes=X.Hurricanes..Typhoons..Tropical.Storms.,
    Mudslides=X.Mudslides..Debris.Flows..Landslides.,
    Cold.Wet=X.Cold..wet.weather.,
    Cool.Temp=X.Cool.Cold..Below.normal.Temperatures.,
  )
FSADisaster2021$Year=2021
FSADisaster= FSADisaster2021
FSADisaster = bind_rows(  FSADisaster2012  ,FSADisaster)
FSADisaster = bind_rows(  FSADisaster2013  ,FSADisaster)
FSADisaster = bind_rows(  FSADisaster2014  ,FSADisaster)
FSADisaster = bind_rows(  FSADisaster2015  ,FSADisaster)
FSADisaster = bind_rows(  FSADisaster2016  ,FSADisaster)
FSADisaster = bind_rows(  FSADisaster2017  ,FSADisaster)
FSADisaster = bind_rows(  FSADisaster2018  ,FSADisaster)
FSADisaster = bind_rows(  FSADisaster2019  ,FSADisaster)
FSADisaster = bind_rows(  FSADisaster2020  ,FSADisaster)

names(FSADisaster)[names(FSADisaster) == "CROP.DISASTER.YEAR"] <- "Year"
FSADisaster$Year<- as.character(FSADisaster$Year)
FSADisaster$FIPS<- as.character(FSADisaster$FIPS)
FSADisaster$FIPS<-str_pad(FSADisaster$FIPS, 5, pad = "0")
FSADisaster$FIPS<-trimws(FSADisaster$FIPS, which=c("both"))
FSADisaster$Year<-trimws(FSADisaster$Year, which=c("both"))
FSADisaster$Freeze<-ifelse(FSADisaster$Freeze==4,1,FSADisaster$Freeze)
FSADisaster$Winter<-if_else(FSADisaster$Freeze==1 | FSADisaster$Winterstorms==1 | FSADisaster$Cold.Wet==1 ,1,0)

FSADisaster$PrimaryCounty<-if_else(FSADisaster$Designation.Code==1,1,0)
fipsyear<-FSADisaster[,c(1,35,38)]
temp<-FSADisaster[,c(6:29)]
g<-temp%>% mutate_if(is.character,as.numeric)
FSADisaster<-cbind(fipsyear,g)

temp<-aggregate(cbind(DROUGHT,Flood,Rain.Humidity,Thunderstorms,Ground.Saturation,Hail,Wind,Fire,Heat,Winterstorms,Freeze,Hurricanes,Tornadoes,Volcano,Mudslides,Heavy.Surf,Ice.Jams,Insects,Storm.Tidal.Surges,Cold.Wet,Cool.Temp,Lightning,Disease,Insufficient.Chill.Hours)~FIPS+Year+PrimaryCounty,data=FSADisaster,sum,na.rm = FALSE)
tempy<-temp[,c(4:27)]
fipsyear<-temp[,c(1:3)]
tempy[tempy>0]<-1
temp<-cbind(fipsyear,tempy)
FSADisaster<-reshape(data=temp, idvar=c('FIPS','Year'),v.names=c('DROUGHT','Flood','Rain.Humidity','Thunderstorms','Ground.Saturation','Hail','Wind','Fire','Heat','Winterstorms','Freeze','Hurricanes','Tornadoes','Volcano','Mudslides','Heavy.Surf','Ice.Jams','Insects','Storm.Tidal.Surges','Cold.Wet','Cool.Temp','Lightning','Disease','Insufficient.Chill.Hours'),timevar="PrimaryCounty",direction='wide')

FSADrought<-FSADisaster[,c(1:3,27)]
FSADrought<-FSADrought%>%
  rename(
    Primary.Declared.Drought=DROUGHT.1,
    Contiguous.Declared.Drought=DROUGHT.0
  )

#FSADisaster$ContiguousCounty<-if_else(FSADisaster$Designation.Code==2,1,0)
# FSADisaster$Begin.Month<-as.integer(format(as.Date(FSADisaster$Begin.Date,format="%m/%d/%y"),"%m"))
# FSADisaster$End.Date<-ifelse(FSADisaster$End.Date=="N/A","12/31/0000",FSADisaster$End.Date)
# FSADisaster$End.Month<-as.integer(format(as.Date(FSADisaster$End.Date,format="%m/%d/%y"),"%m"))
# FSADisaster$End.Month<-ifelse(FSADisaster$End.Date=='continuing',12,FSADisaster$End.Month)



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
#### SOB ####

file.path = "D:/SoB/Practices/"
filename.2=paste(file.path,"SOBSCCTPU0",c(2:9),".txt", sep = "")
filename.1=paste(file.path,"sobtpu_",c(1999:2001),".txt", sep = "")
file.name.0 = paste(file.path,"SOBSCCTPU",c(10:21),".txt", sep = "")

# file.name.000 = paste(file.path,"sobcov",c(89:99),".txt", sep = "")

file.name = c(filename.1,filename.2, file.name.0)

SOBSCCTPU = read.table(file.name[1], header = FALSE,
                       col.names  =  c("Year", "State Code", "State Name", "State Abbr", "County Code",
                                       "County Name", "Commodity Code","Commodity Name", "Insurance Plan Code", "Insurance Plan Abbr", "Coverage Category",
                                       "Coverage Level", "Delivery ID","Type Code", "Type Name", "Practice Code", "Practice Name", "Unit Structure Code", "Unit Structure Name", "Net Reporting Level Amount", 
                                       "Reporting Level Type", "Liability Amount" , "Total Premium Amount","Subsidy Amount","Indemnity Amount",  "Loss Ratio", "Endorsed Commodity Reporting Level Amount"), sep="|", fill = TRUE,flush = TRUE, quote ="", blank.lines.skip = FALSE)


for (i in 2:23)
{
  temp= read.table(file.name[i], header = FALSE,
                   col.names  =  c("Year", "State Code", "State Name", "State Abbr", "County Code",
                                   "County Name", "Commodity Code","Commodity Name", "Insurance Plan Code", "Insurance Plan Abbr", "Coverage Category",
                                   "Coverage Level", "Delivery ID","Type Code", "Type Name", "Practice Code", "Practice Name", "Unit Structure Code", "Unit Structure Name", "Net Reporting Level Amount", 
                                   "Reporting Level Type", "Liability Amount" , "Total Premium Amount","Subsidy Amount","Indemnity Amount",  "Loss Ratio", "Endorsed Commodity Reporting Level Amount"), sep="|", fill = TRUE,flush = TRUE, quote ="", blank.lines.skip = FALSE)
  
  SOBSCCTPU = rbind(SOBSCCTPU,temp)
  rm(temp)
}

SOBSCCTPU<-subset(SOBSCCTPU,Commodity.Code==41 | Commodity.Code==81)

  
  
SOBSCCTPU$FIPS <- SOBSCCTPU$State.Code*1000+SOBSCCTPU$County.Code
SOBSCCTPU[is.na(SOBSCCTPU)==TRUE]=0
SOBSCCTPU$FIPS<-str_pad(SOBSCCTPU$FIPS, 5, pad = "0")
SOBSCCTPU$Year<-as.character(SOBSCCTPU$Year)
SOBSCCTPU$Year<-trimws(SOBSCCTPU$Year, which=c("both"))
SOBSCCTPU$FIPS<-trimws(SOBSCCTPU$FIPS, which=c("both"))
SOBSCCTPU$FIPS<-as.character(SOBSCCTPU$FIPS)
SOBSCCTPU<-SOBSCCTPU%>%
  rename(
    Insured.Acres=Net.Reporting.Level.Amount
  )
temp<-SOBSCCTPU
temp$RevenueProtection<-0
temp$RevenueProtection[temp$Insurance.Plan.Code==2 | temp$Insurance.Plan.Code== 3 |temp$Insurance.Plan.Code==4 |temp$Insurance.Plan.Code==12 | temp$Insurance.Plan.Code==16 | temp$Insurance.Plan.Code==17| temp$Insurance.Plan.Code==25 | temp$Insurance.Plan.Code==32 | temp$Insurance.Plan.Code==33 | temp$Insurance.Plan.Code==44 | temp$Insurance.Plan.Code==73 ]<-1
temp$GroupProtection<-0 
temp$GroupProtection[temp$Insurance.Plan.Code==4 | temp$Insurance.Plan.Code==5 | temp$Insurance.Plan.Code==6 |temp$Insurance.Plan.Code==12 | temp$Insurance.Plan.Code==16 | temp$Insurance.Plan.Code==17| temp$Insurance.Plan.Code==31 | temp$Insurance.Plan.Code==32 | temp$Insurance.Plan.Code==33 | temp$Insurance.Plan.Code==73 ]<-1
SOBSCCTPU<-temp
# yearstate<-unique(SOBSCCTPU[,c(1,2,3)])
# yearstate<-yearstate[order(yearstate$State.Code,yearstate$Year),]
# write.csv(yearstate,"D:\\Futures\\Yearstate.csv",row.names=FALSE)

##practices<-unique(SOBSCCTPU[c('Practice.Code','Practice.Name')])
tempcorn<-SOBSCCTPU[SOBSCCTPU$Commodity.Code==41 & as.numeric(SOBSCCTPU$Year)>=2000,]
StateCornTotals<-aggregate(Insured.Acres~State.Code,data=tempcorn,FUN='sum')
StateCornTotals<-StateCornTotals[order(-StateCornTotals$Insured.Acres),]
write.csv(StateCornTotals,"D:/DroughtEssay/SummaryStats/Total21stCenturyCornInsuredAcreage.csv", row.names = FALSE)

tempsoy<-SOBSCCTPU[SOBSCCTPU$Commodity.Code==81 & as.numeric(SOBSCCTPU$Year)>=2000,]
StateSoyTotals<-aggregate(Insured.Acres~State.Code,data=tempsoy,FUN='sum')
StateSoyTotals<-StateSoyTotals[order(-StateSoyTotals$Insured.Acres),]
write.csv(StateSoyTotals,"D:/DroughtEssay/SummaryStats/Total21stCenturySoyInsuredAcreage.csv", row.names = FALSE)


SOBSCCTPU$Irrigated<-ifelse(SOBSCCTPU$Practice.Code== 2 | SOBSCCTPU$Practice.Code== 94 | SOBSCCTPU$Practice.Code== 95 | SOBSCCTPU$Practice.Code== 154 | SOBSCCTPU$Practice.Code== 156 | SOBSCCTPU$Practice.Code== 157 | SOBSCCTPU$Practice.Code== 702 | SOBSCCTPU$Practice.Code== 712 | SOBSCCTPU$Practice.Code== 739 | SOBSCCTPU$Practice.Code== 740 | SOBSCCTPU$Practice.Code== 741 | SOBSCCTPU$Practice.Code== 742 ,1,0)

SOBSCCTPUaggplan<-aggregate(cbind(Insured.Acres,Liability.Amount,Total.Premium.Amount,Indemnity.Amount)~FIPS+Commodity.Code+RevenueProtection+GroupProtection+Coverage.Category+Year,data=SOBSCCTPU,sum,na.rm = FALSE)

SOBSCCTPUaggirr<-aggregate(cbind(Insured.Acres,Indemnity.Amount)~FIPS+Commodity.Code+Year+Irrigated,data=SOBSCCTPU,sum,na.rm = FALSE)
IrrigatedAcres<-reshape(data=SOBSCCTPUaggirr, idvar=c('FIPS','Year','Commodity.Code'),v.names=c('Insured.Acres','Indemnity.Amount'),timevar="Irrigated",direction='wide')
IrrigatedAcres<-IrrigatedAcres%>%
  rename(
    IrrInsurAcres=Insured.Acres.1,
    NonIrrInsurAcres=Insured.Acres.0,
    IrrIndem=Indemnity.Amount.1,
    NonIrrIndem=Indemnity.Amount.0
  )
SOBSCCTPUaggirrplan<-aggregate(cbind(Insured.Acres,Indemnity.Amount)~FIPS+Commodity.Code+Year+Irrigated+RevenueProtection+GroupProtection+Coverage.Category,data=SOBSCCTPU,sum,na.rm = FALSE)
IrrigatedAcresplan<-reshape(data=SOBSCCTPUaggirrplan, idvar=c('FIPS','Year','Commodity.Code','RevenueProtection','GroupProtection','Coverage.Category'),v.names=c('Insured.Acres','Indemnity.Amount'),timevar="Irrigated",direction='wide')
IrrigatedAcresplan<-IrrigatedAcresplan%>%
  rename(
    IrrInsurAcres=Insured.Acres.1,
    NonIrrInsurAcres=Insured.Acres.0,
    IrrIndem=Indemnity.Amount.1,
    NonIrrIndem=Indemnity.Amount.0
  )

SOBSCCTPUaggcounty<-aggregate(cbind(Insured.Acres,Liability.Amount,Total.Premium.Amount,Indemnity.Amount)~FIPS+Commodity.Code+Year,data=SOBSCCTPUaggplan,sum,na.rm = FALSE)

soySOBSCCTPUaggplan<-subset(SOBSCCTPUaggplan,Commodity.Code==81)
soySOBSCCTPUaggcounty<-subset(SOBSCCTPUaggcounty,Commodity.Code==81)
cornSOBSCCTPUaggplan<-subset(SOBSCCTPUaggplan,Commodity.Code==41)
cornSOBSCCTPUaggcounty<-subset(SOBSCCTPUaggcounty,Commodity.Code==41)



TotalInsuredAcres<-SOBSCCTPUaggcounty[,c('Year','FIPS','Commodity.Code','Insured.Acres','Total.Premium.Amount','Indemnity.Amount')]
TotalInsuredAcresplan<-SOBSCCTPUaggplan[,c('Year','FIPS','Commodity.Code','RevenueProtection','GroupProtection','Coverage.Category','Insured.Acres','Total.Premium.Amount','Indemnity.Amount')]

#rm(SOBSCCTPU,SOBSCCTPUaggcounty,SOBSCCTPUaggplan,TotalInsuredAcres,soySOBSCCTPUaggcounty,cornSOBSCCTPUaggcounty,soySOBSCCTPUaggplan,cornSOBSCCTPUaggplan)


SOBPlanagg<-aggregate(cbind(Insured.Acres)~FIPS+Commodity.Code+Year+Coverage.Category+RevenueProtection+GroupProtection,data=SOBSCCTPU,sum,na.rm = FALSE)

temp<-reshape(data=SOBPlanagg,idvar=c('FIPS','Year','Commodity.Code','Coverage.Category','GroupProtection'),timevar='RevenueProtection',direction='wide')
tempy<-reshape(data=temp,idvar=c('FIPS','Year','Commodity.Code','Coverage.Category'),timevar='GroupProtection',direction='wide')
SOBPlanWide<-reshape(data=tempy,idvar=c('FIPS','Year','Commodity.Code'),timevar='Coverage.Category',direction='wide')
  
SOBPlanWide[is.na(SOBPlanWide)==TRUE]=0
SOBPlanWide$TotalRevenueProtected<- SOBPlanWide$Insured.Acres.1.0.A+SOBPlanWide$Insured.Acres.1.1.A+SOBPlanWide$Insured.Acres.1.0.C+SOBPlanWide$Insured.Acres.1.1.C+SOBPlanWide$Insured.Acres.1.0.L+SOBPlanWide$Insured.Acres.1.1.L
SOBPlanWide$TotalGroupProtected<- SOBPlanWide$Insured.Acres.0.1.A+SOBPlanWide$Insured.Acres.1.1.A+SOBPlanWide$Insured.Acres.0.1.C+SOBPlanWide$Insured.Acres.1.1.C+SOBPlanWide$Insured.Acres.0.1.L+SOBPlanWide$Insured.Acres.1.1.L
SOBPlanWide$TotalBuyUp<- SOBPlanWide$Insured.Acres.1.0.A+SOBPlanWide$Insured.Acres.1.1.A+SOBPlanWide$Insured.Acres.0.1.A+SOBPlanWide$Insured.Acres.0.0.A

SOBPlanWide$YieldBuyUp<-SOBPlanWide$Insured.Acres.0.0.A
SOBPlanWide$GroupBuyUp<-SOBPlanWide$Insured.Acres.0.1.A
SOBPlanWide$RevBuyUp<-SOBPlanWide$Insured.Acres.1.0.A
SOBPlanWide$BothBuyUp<-SOBPlanWide$Insured.Acres.1.1.A
SOBPlanWide$YieldCat<-SOBPlanWide$Insured.Acres.0.0.C+SOBPlanWide$Insured.Acres.0.0.L
SOBPlanWide$RevCat<-SOBPlanWide$Insured.Acres.1.0.C+SOBPlanWide$Insured.Acres.1.0.L
SOBPlanWide$GroupCat<-SOBPlanWide$Insured.Acres.0.1.C+SOBPlanWide$Insured.Acres.0.1.L
SOBPlanWide$BothCat<-SOBPlanWide$Insured.Acres.1.1.C+SOBPlanWide$Insured.Acres.1.1.L


SOBPlanWide<-merge(SOBPlanWide,TotalInsuredAcres,by=c('FIPS','Year','Commodity.Code'),all.x=TRUE)
# SOBPlanWide$PropRevProt<- SOBPlanWide$TotalRevenueProtected/SOBPlanWide$Insured.Acres
# SOBPlanWide$PropGrpProt<- SOBPlanWide$TotalGroupProtected/SOBPlanWide$Insured.Acres
# SOBPlanWide$PropBuyUp<- SOBPlanWide$TotalBuyUp/SOBPlanWide$Insured.Acres
SOBPlanWide<-SOBPlanWide[,c(1:3,18:28)]
SOBPlanWide$State<-substr(SOBPlanWide$FIPS,1,2)
SOBPlanWide<-SOBPlanWide[SOBPlanWide$State == '17' | SOBPlanWide$State == '18' | SOBPlanWide$State == '19'  | SOBPlanWide$State == '20' | SOBPlanWide$State == '26' | SOBPlanWide$State == '27' | SOBPlanWide$State == '29' |SOBPlanWide$State == '31' |SOBPlanWide$State == '38' | SOBPlanWide$State == '39' | SOBPlanWide$State == '42' | SOBPlanWide$State == '46' | SOBPlanWide$State == '55' ,]

##### #### CoL ####
file.path = "D:/CoL/"

file.name.00 = paste(file.path,"colsom",c(89:99),".txt", sep = "")
file.name.0 = paste(file.path,"colsom0",c(0:9),".txt", sep = "")
file.name.1 = paste(file.path,"colsom",c(10:15),".txt", sep = "")
file.name.2 = paste(file.path,"colsom_",c(2016:2021),".txt", sep = "")
file.name = c(file.name.00,file.name.0, file.name.1,file.name.2)

CauseOfLoss = read.table(file.name[1], header = FALSE,
                         col.names  =  c("Year", "State Code", "State Abbr", "County Code",
                                         "County Name", "Commodity Code","Commodity Name", "Insurance Plan Code", "Insurance Plan Abbr", "Coverage Category",
                                         "Stage Code", "Damage Cause Code", "Damage Causes Descr","Month of Loss", "Month of Loss Name", "Year of Loss", "PoliciesEarningPremium", "Policies Indemnified", "Net Planted Acres", "Net Endorsed Acres", "Liability", 
                                         "Total Premium", "Producer Paid Premium","Subsidy","State/Provate Subsidy", "Additional Subsidy", "EFA Premium Discount", "Determined Acres", "Indemnity Amount", "Loss Ratio"), sep="|", fill = TRUE,flush = TRUE, quote ="", skipNul=TRUE, blank.lines.skip = FALSE)

for (i in 2:33)
{
  temp= read.table(file.name[i], header = FALSE,
                   col.names  =  c("Year", "State Code", "State Abbr", "County Code",
                                   "County Name", "Commodity Code","Commodity Name", "Insurance Plan Code", "Insurance Plan Abbr", "Coverage Category",
                                   "Stage Code", "Damage Cause Code", "Damage Causes Descr","Month of Loss", "Month of Loss Name", "Year of Loss", "PoliciesEarningPremium", "Policies Indemnified", "Net Planted Acres", "Net Endorsed Acres", "Liability", 
                                   "Total Premium", "Producer Paid Premium","Subsidy","State/Provate Subsidy", "Additional Subsidy", "EFA Premium Discount", "Determined Acres", "Indemnity Amount", "Loss Ratio"), sep="|", fill = TRUE,flush = TRUE, quote ="", skipNul=TRUE, blank.lines.skip = FALSE)
  CauseOfLoss = rbind(CauseOfLoss,temp)
  rm(temp)
}

CauseOfLoss$Damage.Cause.Code[CauseOfLoss$Damage.Cause.Code == "09"] <- "9"
CauseOfLoss$Damage.Cause.Code[CauseOfLoss$Damage.Cause.Code == "01"] <- "1"
CauseOfLoss$Damage.Cause.Code[CauseOfLoss$Damage.Cause.Code == "XX"] <- "999"

temp<-CauseOfLoss
temp$RevenueProtection<-0
temp$RevenueProtection[temp$Insurance.Plan.Code==2 | temp$Insurance.Plan.Code== 3 |temp$Insurance.Plan.Code==4 |temp$Insurance.Plan.Code==12 | temp$Insurance.Plan.Code==16 | temp$Insurance.Plan.Code==17| temp$Insurance.Plan.Code==25 | temp$Insurance.Plan.Code==32 | temp$Insurance.Plan.Code==33 | temp$Insurance.Plan.Code==44 | temp$Insurance.Plan.Code==73 ]<-1
temp$GroupProtection<-0 
temp$GroupProtection[temp$Insurance.Plan.Code==4 | temp$Insurance.Plan.Code==5 | temp$Insurance.Plan.Code==6 |temp$Insurance.Plan.Code==12 | temp$Insurance.Plan.Code==16 | temp$Insurance.Plan.Code==17| temp$Insurance.Plan.Code==31 | temp$Insurance.Plan.Code==32 | temp$Insurance.Plan.Code==33 | temp$Insurance.Plan.Code==73 ]<-1
CauseOfLoss<-temp
# DamageCauseDictionary<-unique(CauseOfLoss[,c(12:13)])
# DamageCauseDictionary<-DamageCauseDictionary[order(DamageCauseDictionary$Damage.Cause.Code),]
# write.csv(DamageCauseDictionary,"H:/DamageCauseDictionary.csv", row.names = FALSE)

CauseOfLoss$Damage.Group=3
CauseOfLoss$Damage.Group[CauseOfLoss$Damage.Cause.Code==11 | CauseOfLoss$Damage.Cause.Code==12 | CauseOfLoss$Damage.Cause.Code==13 | CauseOfLoss$Damage.Cause.Code==22 | CauseOfLoss$Damage.Cause.Code==62 ]<-1 #Drought
CauseOfLoss$Damage.Group[CauseOfLoss$Damage.Cause.Code==51 | CauseOfLoss$Damage.Cause.Code==31 ]<-2 #Wet

# 
# CoLDrought<-subset(CauseOfLoss,Damage.Group==1)
# CoLDroughtCornSoy<-subset(CauseOfLoss,Commodity.Code==41 | Commodity.Code==81)
# # 
# temp<-dplyr::count(CoLDroughtCornSoy, Year, State.Code, Damage.Cause.Code)
# 
# year<-unique(CoLDroughtCornSoy$Year)
# state<-unique(CoLDroughtCornSoy$State.Code)
# causes<-unique(CoLDroughtCornSoy$Damage.Cause.Code)
# 
# testing<-expand_grid(year,state,causes)
# testing<-testing%>%
#   rename(
#     Year=year,
#     State.Code=state,
#     Damage.Cause.Code=causes
#   )
# Active<-merge(testing,temp,by=c('Year','State.Code','Damage.Cause.Code'),all.x=TRUE)
# Active[is.na(Active)==TRUE]=0
# Active$n=ifelse(Active$n>0,1,0)
# Active<-Active %>%
#   rename(
#     Present=n
#   )
# RelevantDamages<-reshape(data=Active, idvar=c('State.Code','Year'),v.names=c('Present'),timevar="Damage.Cause.Code",direction='wide')
# RelevantDamages<-RelevantDamages[order(RelevantDamages$State.Code,RelevantDamages$Year),]
# 
# 


CauseOfLoss$FIPS <- CauseOfLoss$State.Code*1000+CauseOfLoss$County.Code
CauseOfLoss$FIPS<-as.character(CauseOfLoss$FIPS)
CauseOfLoss$FIPS<-str_pad(CauseOfLoss$FIPS, 5, pad = "0")
CauseOfLoss$FIPS<-trimws(CauseOfLoss$FIPS, which=c("both"))

CoLCornSoy<-subset(CauseOfLoss,Commodity.Code==41 | Commodity.Code==81)
#practices<-unique(CoLCornSoy[c('Insurance.Plan.Code','Month.of.Loss')])

CoLCornSoy<-CoLCornSoy[order(CoLCornSoy$FIPS,CoLCornSoy$Year,CoLCornSoy$Commodity.Code,CoLCornSoy$Insurance.Plan.Code,CoLCornSoy$Coverage.Category,CoLCornSoy$Damage.Group),]
#merge(CornYield,CornAcres, by= c('Year','FIPS','Commodity'))
CoLCornSoyCounties<-aggregate(cbind(Determined.Acres, Indemnity.Amount, Total.Premium, Liability, Net.Planted.Acres)~FIPS+Commodity.Code+Year+Damage.Group,data=CoLCornSoy,sum,na.rm = FALSE)
CoLCornSoyCounties<-CoLCornSoyCounties %>%
  rename(
    CoL.Indemnity.Amount=Indemnity.Amount,
    CoL.PremiumTotal=Total.Premium,
    CoL.LiabilityTotal=Liability,
    CoL.PlantedAcres=Net.Planted.Acres
  )


CoLCornSoyCountiesCause<-aggregate(cbind(Determined.Acres, Indemnity.Amount, Total.Premium, Liability, Net.Planted.Acres)~FIPS+Commodity.Code+Year+Damage.Cause.Code,data=CoLCornSoy,sum,na.rm = FALSE)
IrrSupplyFail<-subset(CoLCornSoyCountiesCause,CoLCornSoyCountiesCause$Damage.Cause.Code=='13')
IrrSupplyFail<-IrrSupplyFail[,c(1:3,5)]
IrrSupplyFail<-IrrSupplyFail%>%
  rename(
    IrrSupplyFailAcres=Determined.Acres
  )
  
  
CoLCornSoyCountiesWide<-reshape(data=CoLCornSoyCounties, idvar=c('FIPS','Commodity.Code','Year'),v.names=c('Determined.Acres','CoL.Indemnity.Amount','CoL.PremiumTotal','CoL.LiabilityTotal','CoL.PlantedAcres'),timevar="Damage.Group",direction='wide')
CoLCornSoyCountiesWide[is.na(CoLCornSoyCountiesWide)]<-0
CoLCornSoyCountiesWide$TotalPremiumCoL<-CoLCornSoyCountiesWide$CoL.PremiumTotal.1+CoLCornSoyCountiesWide$CoL.PremiumTotal.2+CoLCornSoyCountiesWide$CoL.PremiumTotal.3
CoLCornSoyCountiesWide$TotalLiabilityCoL<-CoLCornSoyCountiesWide$CoL.LiabilityTotal.1+CoLCornSoyCountiesWide$CoL.LiabilityTotal.2+CoLCornSoyCountiesWide$CoL.LiabilityTotal.3
CoLCornSoyCountiesWide$TotalPlantedAcresCoL<-CoLCornSoyCountiesWide$CoL.PlantedAcres.1+CoLCornSoyCountiesWide$CoL.PlantedAcres.2+CoLCornSoyCountiesWide$CoL.PlantedAcres.3
CoLCornSoyCountiesWide$Total.Determined.Acres<-CoLCornSoyCountiesWide$Determined.Acres.1+CoLCornSoyCountiesWide$Determined.Acres.2+CoLCornSoyCountiesWide$Determined.Acres.3
CoLCornSoyCountiesWide$TotalIndemnityCoL<-CoLCornSoyCountiesWide$CoL.Indemnity.Amount.1+CoLCornSoyCountiesWide$CoL.Indemnity.Amount.2+CoLCornSoyCountiesWide$CoL.Indemnity.Amount.3


CoLCornSoyPlans<-aggregate(cbind(Determined.Acres, Indemnity.Amount, Total.Premium, Liability, Net.Planted.Acres)~FIPS+Commodity.Code+RevenueProtection+GroupProtection+Coverage.Category+Year+Damage.Group,data=CoLCornSoy,sum,na.rm = FALSE)
CoLCornSoyPlans<-CoLCornSoyPlans %>%
  rename(
    CoL.Indemnity.Amount=Indemnity.Amount,
    CoL.PremiumTotal=Total.Premium,
    CoL.LiabilityTotal=Liability,
    CoL.PlantedAcres=Net.Planted.Acres
  )

CoLCornSoyPlansCause<-aggregate(cbind(Determined.Acres, Indemnity.Amount, Total.Premium, Liability, Net.Planted.Acres)~FIPS+Commodity.Code+Insurance.Plan.Code+Coverage.Category+Year+Damage.Cause.Code,data=CoLCornSoy,sum,na.rm = FALSE)
IrrSupplyFailplan<-subset(CoLCornSoyPlansCause,CoLCornSoyPlansCause$Damage.Cause.Code=='13')
IrrSupplyFailplan<-IrrSupplyFailplan[,c(1:5,7)]
IrrSupplyFailplan<-IrrSupplyFailplan%>%
  rename(
    IrrSupplyFailAcres=Determined.Acres
  )



CoLCornSoyPlansWide<-reshape(data=CoLCornSoyPlans, idvar=c('FIPS','Commodity.Code','Year','RevenueProtection','GroupProtection','Coverage.Category'),v.names=c('Determined.Acres','CoL.Indemnity.Amount','CoL.PremiumTotal','CoL.LiabilityTotal','CoL.PlantedAcres'),timevar="Damage.Group",direction='wide')
CoLCornSoyPlansWide[is.na(CoLCornSoyPlansWide)]<-0
CoLCornSoyPlansWide$TotalPremiumCoL<-CoLCornSoyPlansWide$CoL.PremiumTotal.1+CoLCornSoyPlansWide$CoL.PremiumTotal.2+CoLCornSoyPlansWide$CoL.PremiumTotal.3
CoLCornSoyPlansWide$TotalLiabilityCoL<-CoLCornSoyPlansWide$CoL.LiabilityTotal.1+CoLCornSoyPlansWide$CoL.LiabilityTotal.2+CoLCornSoyPlansWide$CoL.LiabilityTotal.3
CoLCornSoyPlansWide$TotalPlantedAcresCoL<-CoLCornSoyPlansWide$CoL.PlantedAcres.1+CoLCornSoyPlansWide$CoL.PlantedAcres.2+CoLCornSoyPlansWide$CoL.PlantedAcres.3
CoLCornSoyPlansWide$Total.Determined.Acres<-CoLCornSoyPlansWide$Determined.Acres.1+CoLCornSoyPlansWide$Determined.Acres.2+CoLCornSoyPlansWide$Determined.Acres.3
CoLCornSoyPlansWide$TotalIndemnityCoL<-CoLCornSoyPlansWide$CoL.Indemnity.Amount.1+CoLCornSoyPlansWide$CoL.Indemnity.Amount.2+CoLCornSoyPlansWide$CoL.Indemnity.Amount.3






CoLCornSoyMonths<-CoLCornSoy[order(CoLCornSoy$FIPS,CoLCornSoy$Year,CoLCornSoy$Month.of.Loss,CoLCornSoy$Commodity.Code,CoLCornSoy$Insurance.Plan.Code,CoLCornSoy$Coverage.Category,CoLCornSoy$Damage.Group),]
CoLCornSoyMonths<-subset(CoLCornSoyMonths,Month.of.Loss>0)
#1804137-?1786562, <1% lost

#merge(CornYield,CornAcres, by= c('Year','FIPS','Commodity'))
CoLMonths<-aggregate(cbind(Determined.Acres, Indemnity.Amount, Total.Premium, Liability, Net.Planted.Acres)~FIPS+Month.of.Loss+Commodity.Code+Year+Damage.Group,data=CoLCornSoyMonths,sum,na.rm = FALSE)
CoLMonths<-CoLMonths %>%
  rename(
    CoL.Indemnity.Amount=Indemnity.Amount,
    CoL.PremiumTotal=Total.Premium,
    CoL.LiabilityTotal=Liability,
    CoL.PlantedAcres=Net.Planted.Acres
  )

DamageGroupsMonths<-reshape(data=CoLMonths[,c(1:6)], idvar=c('FIPS','Year','Month.of.Loss','Commodity.Code'),v.names=c('Determined.Acres'),timevar="Damage.Group",direction='wide')
DamageGroupsMonths[is.na(DamageGroupsMonths)==TRUE]=0
DamageGroupsMonths<-DamageGroupsMonths[order(DamageGroupsMonths$Commodity.Code,DamageGroupsMonths$FIPS,DamageGroupsMonths$Year,DamageGroupsMonths$Month.of.Loss),]
DamageGroupsMonths$TotalDetermined<-DamageGroupsMonths$Determined.Acres.1+DamageGroupsMonths$Determined.Acres.2+DamageGroupsMonths$Determined.Acres.3
DamageGroupsMonths$CumulativeDetermined<-ave(DamageGroupsMonths$TotalDetermined,DamageGroupsMonths[,c('Commodity.Code','FIPS','Year')],FUN=cumsum)
DamageGroupsMonths<-DamageGroupsMonths %>%
  group_by(Commodity.Code,FIPS,Year) %>%
  dplyr::mutate(PreviouslyDetermined = dplyr::lag(CumulativeDetermined, n = 1, default = NA)) %>% 
  as.data.frame()
DamageGroupsMonths[is.na(DamageGroupsMonths)==TRUE]=0 
DamageGroupsMonths<-merge(DamageGroupsMonths,TotalInsuredAcres,by=c('Year','FIPS','Commodity.Code'),all.x=TRUE)
DamageGroupsMonths<-subset(DamageGroupsMonths,is.na(DamageGroupsMonths$Insured.Acres)==FALSE)
DamageGroupsMonths<-DamageGroupsMonths[order(DamageGroupsMonths$Commodity.Code,DamageGroupsMonths$FIPS,DamageGroupsMonths$Year,DamageGroupsMonths$Month.of.Loss),]
DamageGroupsMonths$RemainingAcres<-DamageGroupsMonths$Insured.Acres-DamageGroupsMonths$PreviouslyDetermined
DamageGroupsMonths$Propotion.Drought<-DamageGroupsMonths$Determined.Acres.1/DamageGroupsMonths$RemainingAcres
DamageGroupsMonths$Proportion.Wet<-DamageGroupsMonths$Determined.Acres.2/DamageGroupsMonths$RemainingAcres
DamageGroupsMonths$Propotion.Other<-DamageGroupsMonths$Determined.Acres.3/DamageGroupsMonths$RemainingAcres






CoLCornSoyMonths<-CoLCornSoy[order(CoLCornSoy$FIPS,CoLCornSoy$Year,CoLCornSoy$Month.of.Loss,CoLCornSoy$Commodity.Code,CoLCornSoy$Insurance.Plan.Code,CoLCornSoy$Coverage.Category,CoLCornSoy$Damage.Group),]

CoLMonths<-merge(CoLMonths,TotalInsuredAcres,by=c('Year','FIPS','Commodity.Code'),all.x=TRUE)

#### NASS ####
CornYield<-read.csv(file="D:/NASS/CornGrain_Yield_2000_2021.csv", stringsAsFactors = FALSE)
CornYield$FIPS <- CornYield$State.ANSI*1000+CornYield$County.ANSI
CornYield<-CornYield %>%
  rename(
    Yield=Value
  )
CornYield<-CornYield[,c("Year","FIPS","Yield")]
CornYield$Commodity.Code<-41

CornAcres<-read.csv(file="D:/NASS/Corn_AcresPlanted_2000_2021.csv", stringsAsFactors = FALSE)
CornAcres$FIPS <- CornAcres$State.ANSI*1000+CornAcres$County.ANSI
CornAcres<-CornAcres %>%
  rename(
    Acres.Planted=Value
  )
CornAcres$Acres.Planted<-as.numeric(sub(",","",CornAcres$Acres.Planted))
StateCornTotals<-aggregate(Acres.Planted~State.ANSI,data=CornAcres,FUN='sum')
StateCornTotals<-StateCornTotals[order(-StateCornTotals$Acres.Planted),]
write.csv(StateCornTotals,"D:/DroughtEssay/SummaryStats/Total21stCenturyCornAcreage.csv", row.names = FALSE)

CornAcres<-CornAcres[,c("Year","FIPS","Acres.Planted")]
CornAcres$Commodity.Code<-41


  
SoyYield<-read.csv(file="D:/NASS/Soybeans_Yield_2000_2021.csv", stringsAsFactors = FALSE)
SoyYield$FIPS <- SoyYield$State.ANSI*1000+SoyYield$County.ANSI
SoyYield<-SoyYield %>%
  rename(
    Yield=Value
  )
SoyYield<-SoyYield[,c("Year","FIPS","Yield")]
SoyYield$Commodity.Code<-81


SoyAcres<-read.csv(file="D:/NASS/Soybeans_AcresPlanted_2000_2021.csv", stringsAsFactors = FALSE)
SoyAcres$FIPS <- SoyAcres$State.ANSI*1000+SoyAcres$County.ANSI
SoyAcres<-SoyAcres %>%
  rename(
    Acres.Planted=Value
  )
SoyAcres$Acres.Planted<-as.numeric(sub(",","",SoyAcres$Acres.Planted))
StateSoyTotals<-aggregate(Acres.Planted~State.ANSI,data=SoyAcres,FUN='sum')
StateSoyTotals<-StateSoyTotals[order(-StateSoyTotals$Acres.Planted),]
write.csv(StateSoyTotals,"D:/DroughtEssay/SummaryStats/Total21stCenturySoyAcreage.csv", row.names = FALSE)
SoyAcres<-SoyAcres[,c("Year","FIPS","Acres.Planted")]
SoyAcres$Commodity.Code<-81


#### Save Files #####



save(SOBPlanWide, file='D:/DroughtEssay/Data/SOBPlanWide.rda')
save(SOBPlanagg,file='D:/DroughtEssay/Data/SobPlanagg.rda')

save(CoLCornSoyCountiesWide,file='D:/DroughtEssay/Data/CoLCounties.Rda')
save(CoLCornSoyPlansWide,file='D:/DroughtEssay/Data/CoLPlans.Rda')
save(DamageGroupsMonths,file='D:/DroughtEssay/Data/CoLPropByMonth.Rda')
save(IrrSupplyFail,file='D:/DroughtEssay/Data/IrrSupplyFail.rda')
save(IrrSupplyFailplan,file='D:/DroughtEssay/Data/IrrSupplyFailplan.rda')

save(TotalInsuredAcres,file='D:/DroughtEssay/Data/InsuredAcres.Rda')
save(TotalInsuredAcresplan,file='D:/DroughtEssay/Data/InsuredAcresPlan.Rda')

save(soySOBSCCTPUaggcounty,file='D:/DroughtEssay/Data/SoyCountySOB.Rda')
save(cornSOBSCCTPUaggcounty,file='D:/DroughtEssay/Data/CornCountySOB.Rda')
save(soySOBSCCTPUaggplan,file='D:/DroughtEssay/Data/SoyPlanSOB.Rda')
save(cornSOBSCCTPUaggplan,file='D:/DroughtEssay/Data/CornPlanSOB.Rda')
save(IrrigatedAcres,file='D:/DroughtEssay/Data/IrrAcres.rda')
save(IrrigatedAcresplan,file='D:/DroughtEssay/Data/IrrAcresPlan.rda')

save(DroughtMonthly,file='D:/DroughtEssay/Data/DroughtMonths.Rda')
save(DroughtSeasonally,file='D:/DroughtEssay/Data/DroughtSeasons.Rda')
save(GrainCornYearlyDrought,file='D:/DroughtEssay/Data/DroughtYears.Rda')
save(DroughtMonitor,file='D:/DroughtEssay/Data/DroughtMonitor.Rda')

save(FSADrought,file='D:/DroughtEssay/Data/FSADrought.Rda')
save(FSADisaster,file='D:/DroughtEssay/Data/FSADisaster.Rda')

save(CornYield,file='D:/DroughtEssay/Data/CornYield.Rda')
save(SoyYield,file='D:/DroughtEssay/Data/SoyYield.Rda')
save(CornAcres,file='D:/DroughtEssay/Data/CornAcres.Rda')
save(SoyAcres,file='D:/DroughtEssay/Data/SoyAcres.Rda')


#### Graphics ####

setwd("D:/test")
inshp="cb_2018_us_county_500k"

#countymap<-us_counties(states = c("michigan",'ohio','minnesota','north dakota','south dakota','iowa','indiana','illinois'))
countymap<- readOGR(dsn=getwd(), layer=inshp)
countymap<-subset(countymap,STATEFP == "17"| STATEFP == "18" | STATEFP == "19" | STATEFP == "20" |STATEFP == "26" | STATEFP == "27" | STATEFP == "29" |STATEFP == "31" | STATEFP == "38" | STATEFP== "39" | STATEFP== "42"| STATEFP== "46"| STATEFP== "55")
#countymap<-countymap[,c('GEOID')]
#countymap<-setNames(countymap, c('FIPS'))
counties<-fortify(countymap, region = "GEOID")

h<-left_join(droughtoccurence, counties, by= c("FIPS"="id"))
ggplot(h, aes(long, lat, group=FIPS))+
  geom_polygon(aes(fill=DroughtOccur))+
  xlab("Longitude")+
  ylab("Latitude")+
  coord_quickmap()

temp<-subset(SOBSCCTPU,Commodity.Code==41 | Commodity.Code==81)
temp<-subset(temp,Year>=2000 &  Year<=2021)             
InsuredAcreage<-aggregate( Insured.Acres~ FIPS ,data=temp,sum,na.rm = TRUE)
i<-left_join(InsuredAcreage, counties, by= c("FIPS"="id"))
ggplot(i, aes(long, lat, group=FIPS))+
  geom_polygon(aes(fill=Insured.Acres))+
  xlab("Longitude")+
  ylab("Latitude")+
  coord_quickmap()

#### testing ground ####
SOBSCCTPU$State<-substr(SOBSCCTPU$FIPS,1,2)
SOBSCCTPUy<-SOBSCCTPU[SOBSCCTPU$State == '17' | SOBSCCTPU$State == '18' | SOBSCCTPU$State == '19'  | SOBSCCTPU$State == '20' | SOBSCCTPU$State == '26' | SOBSCCTPU$State == '27' | SOBSCCTPU$State == '31' | SOBSCCTPU$State == '39' | SOBSCCTPU$State == '42' | SOBSCCTPU$State == '46' | SOBSCCTPU$State == '55' ,]
SOBSCCTPUy<-subset(SOBSCCTPUy,SOBSCCTPUy$Commodity.Code==41 | SOBSCCTPUy$Commodity.Code==81)
SOBSCCTPUy<-subset(SOBSCCTPUy,SOBSCCTPUy$Year>2000)

test<-SOBSCCTPUy%>%
  group_by(Commodity.Code,Insurance.Plan.Code, Coverage.Category) %>%
  summarise(total = sum(Insured.Acres))


test<-test[order(test$Commodity.Code,-test$total),]
write.csv(test,"H:/CommonInsurance.csv",row.names=FALSE)

InsuranceCodeDictionary<-unique(SOBSCCTPU[,c(1,9)])
InsuranceCodeDictionary<-InsuranceCodeDictionary[order(InsuranceCodeDictionary$Insurance.Plan.Code),]
InsuranceCodeDictionary$Year<-as.numeric(InsuranceCodeDictionary$Year)
InsuranceCodeDictionary<- InsuranceCodeDictionary%>%
  group_by(Insurance.Plan.Code) %>%
  mutate(
    MaxYear = max(Year, na.rm=T),
    MinYear = min(Year, na.rm=T)
  )
InsuranceCodeDictionary<-unique(InsuranceCodeDictionary[,c(2,3,4)])
write.csv(InsuranceCodeDictionary,"H:/InsurancePlansDictionary.csv", row.names = FALSE)


InsuranceCode<-read.csv(file='H:/InsuranceCodeDictionary.csv', stringsAsFactors = FALSE)

InsuranceCodeDictionary<-merge(InsuranceCodeDictionary,InsuranceCode,by=c('Insurance.Plan.Code'),all.x=TRUE)
write.csv(InsuranceCodeDictionary,"H:/InsurancePlansDictionary.csv", row.names = FALSE)


