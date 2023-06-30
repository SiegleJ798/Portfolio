#### Start Up and BPR ####
rm(list = ls())
library(stringr)
library(tidyverse)
library(data.table)
library(readxl)
library(ggpubr)
library(lubridate)
library(devtools)
library(FactoMineR)
library(glmnet)
library(Rcpp)
library(fastDummies)
library(coefplot)
library(prism)
library(raster)
library(sp)
library(rgdal)
library(janitor)
library(ggplot2)
####BucketDictionary####
BucketDictionary<-read.csv(file="H:/CommodityBucketsDictionary.csv")
BucketDictionary[BucketDictionary$BucketCode==19,]$Bucket<-"Commodities"
BucketDictionary[BucketDictionary$BucketCode==8 |BucketDictionary$BucketCode==15, ]$BucketCode<-8
BucketDictionary[BucketDictionary$BucketCode==8 ,]$Bucket<-"PotatoesRootBulbs"
BucketDictionary[BucketDictionary$BucketCode==6 |BucketDictionary$BucketCode==3, ]$BucketCode<-3
BucketDictionary[BucketDictionary$BucketCode==3 ,]$Bucket<-"AllTreeFruit"
#BucketDictionary[BucketDictionary$Commodity.Code==41 |BucketDictionary$Commodity.Code==81 |BucketDictionary$Commodity.Code==11 |BucketDictionary$Commodity.Code==88 |BucketDictionary$Commodity.Code==21 |BucketDictionary$Commodity.Code==51 |BucketDictionary$Commodity.Code==48 | ,]
BucketDictionary<-subset(BucketDictionary,BucketCode==3 | BucketCode==8 |BucketCode==9 |BucketCode==16 |BucketCode==19)

PotatoesList<-list(11,15,16,41,51,78,81,84,91)


##### Cause of Loss ####
file.path = "D:/CoL/"
# 
# file.name.00 = paste(file.path,"colsom",c(89:99),".txt", sep = "")
file.name.0 = paste(file.path,"colsom0",c(0:9),".txt", sep = "")
file.name.1 = paste(file.path,"colsom",c(10:15),".txt", sep = "")
file.name.2 = paste(file.path,"colsom_",c(2016:2021),".txt", sep = "")
file.name = c(file.name.0, file.name.1,file.name.2)

CauseOfLoss = read.table(file.name[1], header = FALSE,
                         col.names  =  c("Year", "State Code", "State Abbr", "County Code",
                                         "County Name", "Commodity Code","Commodity Name", "Insurance Plan Code", "Insurance Plan Abbr", "Coverage Category",
                                         "Stage Code", "Damage Cause Code", "Damage Causes Descr","Month of Loss", "Month of Loss Name", "Year of Loss", "PoliciesEarningPremium", "Policies Indemnified", "Net Planted Acres", "Net Endorsed Acres", "Liability", 
                                         "Total Premium", "Producer Paid Premium","Subsidy","State/Provate Subsidy", "Additional Subsidy", "EFA Premium Discount", "Determined Acres", "Indemnity Amount", "Loss Ratio"), sep="|", fill = TRUE,flush = TRUE, quote ="", skipNul=TRUE, blank.lines.skip = FALSE)

for (i in 2:22)
{
  temp= read.table(file.name[i], header = FALSE,
                   col.names  =  c("Year", "State Code", "State Abbr", "County Code",
                                   "County Name", "Commodity Code","Commodity Name", "Insurance Plan Code", "Insurance Plan Abbr", "Coverage Category",
                                   "Stage Code", "Damage Cause Code", "Damage Causes Descr","Month of Loss", "Month of Loss Name", "Year of Loss", "PoliciesEarningPremium", "Policies Indemnified", "Net Planted Acres", "Net Endorsed Acres", "Liability", 
                                   "Total Premium", "Producer Paid Premium","Subsidy","State/Provate Subsidy", "Additional Subsidy", "EFA Premium Discount", "Determined Acres", "Indemnity Amount", "Loss Ratio"), sep="|", fill = TRUE,flush = TRUE, quote ="", skipNul=TRUE, blank.lines.skip = FALSE)
  CauseOfLoss = rbind(CauseOfLoss,temp)
  rm(temp)
}
CauseOfLoss<-subset(CauseOfLoss,Year>1999)
CauseOfLoss$Damage.Cause.Code[CauseOfLoss$Damage.Cause.Code == "09"] <- "9"
CauseOfLoss$Damage.Cause.Code[CauseOfLoss$Damage.Cause.Code == "01"] <- "1"
CauseOfLoss$Damage.Cause.Code[CauseOfLoss$Damage.Cause.Code == "XX"] <- "999"

DamageCauseDictionary<-unique(CauseOfLoss[,c(12:13)])
DamageCauseDictionary<-DamageCauseDictionary[order(DamageCauseDictionary$Damage.Cause.Code),]
write.csv(DamageCauseDictionary,"H:/DamageCauseDictionary.csv", row.names = FALSE)

CauseOfLoss$Damage.Group=999
CauseOfLoss$Damage.Group[CauseOfLoss$Damage.Cause.Code==11 | CauseOfLoss$Damage.Cause.Code==12 | CauseOfLoss$Damage.Cause.Code==13 | CauseOfLoss$Damage.Cause.Code==22 | CauseOfLoss$Damage.Cause.Code==62 ]<-1 #Drought
CauseOfLoss$Damage.Group[CauseOfLoss$Damage.Cause.Code==51]<-2 #Flood
CauseOfLoss$Damage.Group[CauseOfLoss$Damage.Cause.Code==63 | CauseOfLoss$Damage.Cause.Code==92 ]<-3 #Hurricane
CauseOfLoss$Damage.Group[CauseOfLoss$Damage.Cause.Code==91]<-4 #Fire
CauseOfLoss$Damage.Group[CauseOfLoss$Damage.Cause.Code==61]<-5 #Wind
CauseOfLoss$Damage.Group[CauseOfLoss$Damage.Cause.Code==31]<-6 #Rain.Humidity
CauseOfLoss$Damage.Group[CauseOfLoss$Damage.Cause.Code==64]<-7 #Tornadoes
CauseOfLoss$Damage.Group[CauseOfLoss$Damage.Cause.Code==41 |CauseOfLoss$Damage.Cause.Code==42 | CauseOfLoss$Damage.Cause.Code==43 | CauseOfLoss$Damage.Cause.Code==44]<-8 #Winter
CauseOfLoss$Damage.Group[CauseOfLoss$Damage.Cause.Code==70 |CauseOfLoss$Damage.Cause.Code==71 | CauseOfLoss$Damage.Cause.Code==80 |CauseOfLoss$Damage.Cause.Code==81 | CauseOfLoss$Damage.Cause.Code==82 | CauseOfLoss$Damage.Cause.Code==9]<-9 #DiseaseandPests
CauseOfLoss$Damage.Group[CauseOfLoss$Damage.Cause.Code==21]<-10 #Hail


#CauseOfLoss = read.table(file.name[1], header = FALSE, sep="|", quote = " ")
#CauseOfLoss$Year<-as.integer(CauseOfLoss$Year)
# GDPdeflators<-read_excel("D:/CoL/GDP_chain10-12-2021.xlsx", sheet ="price deflators")
# GDPdeflators<-GDPdeflators[,c(1,3)]
# names(GDPdeflators)[2]<-'deflator'

#CauseOfLoss<-merge(CauseOfLoss,GDPdeflators,by=c('Year'),all.x=TRUE)
#CauseOfLoss$Indemnity.Amount<-(CauseOfLoss$Indemnity.Amount/CauseOfLoss$deflator)*100
# CauseOfLoss$Commodity.Name<-tolower(CauseOfLoss$Commodity.Name)
# CauseOfLoss$Commodity.Name<-trimws(CauseOfLoss$Commodity.Name, which=c("both"))
#rename similar varieties
##CauseOfLoss$Commodity.Name<-ifelse(CauseOfLoss$Commodity.Name=='income protection corn'  |CauseOfLoss$Commodity.Name=='hybrid corn seed'|CauseOfLoss$Commodity.Name=='grp corn' ,'corn',CauseOfLoss$Commodity.Name)
#CauseOfLoss$Commodity.Name<-ifelse(CauseOfLoss$Commodity.Name=='hybrid sweet corn seed','sweet corn',CauseOfLoss$Commodity.Name)
#CauseOfLoss$Commodity.Name<-ifelse(CauseOfLoss$Commodity.Name=='income protection wheat' |CauseOfLoss$Commodity.Name=='grp wheat' ,'wheat',CauseOfLoss$Commodity.Name)
#CauseOfLoss$Commodity.Name<-ifelse(CauseOfLoss$Commodity.Name=='grp soybeans' |CauseOfLoss$Commodity.Name=='revenue coverage soybeans' ,'soybeans',CauseOfLoss$Commodity.Name)


CauseOfLoss$FIPS <- CauseOfLoss$State.Code*1000+CauseOfLoss$County.Code
CauseOfLoss$FIPS<-as.character(CauseOfLoss$FIPS)
CauseOfLoss$FIPS<-str_pad(CauseOfLoss$FIPS, 5, pad = "0")
CauseOfLoss$FIPS<-trimws(CauseOfLoss$FIPS, which=c("both"))

#
# temp<-dplyr::count(CauseOfLoss, Year, State.Code, Damage.Cause.Code)
# 
# year<-unique(CauseOfLoss$Year)
# state<-unique(CauseOfLoss$State.Code)
# causes<-unique(CauseOfLoss$Damage.Cause.Code)
# commidities<-unique(CauseOfLoss$Commodity.Code)
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
save(CauseOfLoss, file='D:/CorntoSpecialty/Data/CoLBase.rda')
CauseOfLoss<-merge(CauseOfLoss,BucketDictionary,by=c('Commodity.Code'))
# ### rgb graph ####
# 
# forgraph<-merge(CauseOfLoss,BucketDictionary,by=c('Commodity.Code'))
# forgraph$Specialty<-1
# forgraph$Specialty[forgraph$BucketCode==19 | is.na(forgraph$BucketCode) ]<-0
# forgraph<-subset(forgraph,forgraph$Specialty==1)
# 
# totals<-aggregate(Indemnity.Amount~FIPS+Damage.Group,data=forgraph,sum,na.rm = FALSE)
# totalwise<-reshape(data=totals, idvar=c('FIPS'),v.names='Indemnity.Amount',timevar="Damage.Group", direction='wide')
# totalwise[is.na(totalwise)]<-0
# totalwise$Hot=totalwise$Indemnity.Amount.1+totalwise$Indemnity.Amount.4
# totalwise$Wet=totalwise$Indemnity.Amount.2+totalwise$Indemnity.Amount.3+totalwise$Indemnity.Amount.6
# totalwise$Cold=totalwise$Indemnity.Amount.8
# totalwise<-subset(totalwise,select=-c(2:11))
# totalwise$total=totalwise$Hot+totalwise$Wet+totalwise$Cold
# totalwise$Hot<-totalwise$Hot/totalwise$total
# totalwise$Wet<-totalwise$Wet/totalwise$total
# totalwise$Cold<-totalwise$Cold/totalwise$total
# totalwise<-subset(totalwise,select=-c(5))
# totalwise[is.na(totalwise)]<-0
# totalwise$colorcode<-rgb(totalwise$Hot,totalwise$Wet,totalwise$Cold,maxColorValue=1)
# totalwise<-subset(totalwise,select=-c(2:4))
# totalwise<-totalwise%>%
#   rename(
#     GEOID=FIPS
#   )
# 
# setwd("D:/test")
# inshp="cb_2018_us_county_500k"
# counties<- readOGR(dsn=getwd(), layer=inshp)
# counties<-subset(counties,STATEFP != "15" & STATEFP != "02" & STATEFP != "60" & STATEFP != "66" & STATEFP != "69" & STATEFP != "72" & STATEFP != "78")
# graphdata<-merge(counties,totalwise, by=c('GEOID'),all.x=TRUE)
# graphdata@data$colorcode[is.na(graphdata@data$colorcode)]<-'#7f7f7f'
# graphdata@data$id<-rownames(graphdata@data)
# graphy<-fortify(graphdata,region="id")
# graphdf<-merge(graphy,graphdata@data,by="id")
# cols<-c("Drought, Heat, and Fire"="red","Floods, Hurricane and excessive Rain"="blue", "Winter Weather"="Green")
# p0 <- ggplot(data = graphdf)
# p0+
#   geom_polygon(aes(x=long,y=lat,group=group),fill=p0$data$colorcode) +
#   coord_equal() +
#   theme(panel.background=element_blank())+
#   theme(panel.background= element_rect(color="black")) +
#   theme(axis.title = element_blank(), axis.text = element_blank()) +
#   labs(title = "Relative Share of Specialty Crop Indemnities by Cause") 
# 
# 
# print(p0)
# 
# plot(counties)
# 

#### Summary of Business ####

file.path = "D:/SoB/"
filename.1=paste(file.path,"sobcov0",c(0:9),".txt", sep = "")
filename.0=paste(file.path,"sobcov",c(89:99),".txt", sep = "")
file.name.2 = paste(file.path,"sobcov",c(10:15),".txt", sep = "")
file.name.3 = paste(file.path,"sobcov_",c(2016:2021),".txt", sep = "")
 #file.name.000 = paste(file.path,"sobcov",c(89:99),".txt", sep = "")

file.name = c(filename.0,filename.1,file.name.2, file.name.3)

SOBSCCTPU = read.table(file.name[1], header = FALSE,
                       col.names  =  c("Year", "State Code", "State Abbr", "County Code",
                                       "County Name", "Commodity Code","Commodity Name", "Insurance Plan Code", "Insurance Plan Abbr", "Coverage Category","Delivery Type",
                                       "Coverage Level", "Policies Sold","Policies Earning Premium", "Policies Indemnified", "Units Earning Premium", "Units Indemnified", "Reporting Level Type","Net Reporting Level Amount", 
                                       "EndorsedCompanion Acres", "Liability Amount" , "Total Premium Amount","Subsidy Amount","StatePrivateSubsidy","Additional Subsidy","EFA Premium Discount","Indemnity Amount",  "Loss Ratio"), sep="|", fill = TRUE,flush = TRUE, quote ="", blank.lines.skip = FALSE)


for (i in 2:length(file.name))
{
  temp = read.table(file.name[i], header = FALSE,
                         col.names  =  c("Year", "State Code", "State Abbr", "County Code",
                                         "County Name", "Commodity Code","Commodity Name", "Insurance Plan Code", "Insurance Plan Abbr", "Coverage Category","Delivery Type",
                                         "Coverage Level", "Policies Sold","Policies Earning Premium", "Policies Indemnified", "Units Earning Premium", "Units Indemnified", "Reporting Level Type","Net Reporting Level Amount", 
                                         "EndorsedCompanion Acres", "Liability Amount" , "Total Premium Amount","Subsidy Amount","StatePrivateSubsidy","Additional Subsidy","EFA Premium Discount","Indemnity Amount",  "Loss Ratio"), sep="|", fill = TRUE,flush = TRUE, quote ="", blank.lines.skip = FALSE)
  
  
  SOBSCCTPU = rbind(SOBSCCTPU,temp)
  rm(temp)
}

#SOBSCCTPU<-subset(SOBSCCTPU,Commodity.Code==41 | Commodity.Code==81)

SOBSCCTPU$FIPS <- SOBSCCTPU$State.Code*1000+SOBSCCTPU$County.Code
SOBSCCTPU[is.na(SOBSCCTPU)==TRUE]=0
SOBSCCTPU$FIPS<-str_pad(SOBSCCTPU$FIPS, 5, pad = "0")

SOBSCCTPU$Year<-as.character(SOBSCCTPU$Year)
SOBSCCTPU$Year<-trimws(SOBSCCTPU$Year, which=c("both"))
SOBSCCTPU$FIPS<-trimws(SOBSCCTPU$FIPS, which=c("both"))
SOBSCCTPU$Reporting.Level.Type<-trimws(SOBSCCTPU$Reporting.Level.Type, which=c("both"))
SOBSCCTPU$FIPS<-as.character(SOBSCCTPU$FIPS)
SOBSCCTPU<-SOBSCCTPU%>%
  rename(
    Insured.Acres=Net.Reporting.Level.Amount
  )

##practices<-unique(SOBSCCTPU[c('Practice.Code','Practice.Name')])
# 
# SOBSCCTPU$Irrigated<-ifelse(SOBSCCTPU$Practice.Code== 2 | SOBSCCTPU$Practice.Code== 94 | SOBSCCTPU$Practice.Code== 95 | SOBSCCTPU$Practice.Code== 154 | SOBSCCTPU$Practice.Code== 156 | SOBSCCTPU$Practice.Code== 157 | SOBSCCTPU$Practice.Code== 702 | SOBSCCTPU$Practice.Code== 712 | SOBSCCTPU$Practice.Code== 739 | SOBSCCTPU$Practice.Code== 740 | SOBSCCTPU$Practice.Code== 741 | SOBSCCTPU$Practice.Code== 742 ,1,0)
save(SOBSCCTPU, file='D:/CorntoSpecialty/Data/SoBBase.rda')

SOBSCCTPU<-merge(SOBSCCTPU,BucketDictionary,by=c('Commodity.Code'))
SOBSCCTPU$Specialty<-1
SOBSCCTPU$Specialty[SOBSCCTPU$BucketCode==19| is.na(SOBSCCTPU$BucketCode) ]<-0
soball<-SOBSCCTPU
SOBSCCTPU<-subset(SOBSCCTPU,Year>1999)
SOBSCCTPU$Year <- trimws(SOBSCCTPU$Year)
SOBSCCTPU$FIPS <- trimws(SOBSCCTPU$FIPS)
# SOBSCCTPUaggplan<-aggregate(cbind(Insured.Acres,Liability.Amount,Total.Premium.Amount,Indemnity.Amount)~FIPS+Commodity.Code+Insurance.Plan.Code+Coverage.Category+Year+Irrigated,data=SOBSCCTPU,sum,na.rm = FALSE)

# SoBCornSoy<-subset(SumOfBusiness,Commodity.Code==41 | Commodity.Code==81)
InsuredAcres<-SOBSCCTPU[,c('FIPS','Year','Commodity.Code','Insured.Acres')]
InsuredAcres<-aggregate(cbind(Insured.Acres)~FIPS+Year+Commodity.Code,data=InsuredAcres,sum,na.rm = FALSE)
save(SOBSCCTPU, file='D:/CorntoSpecialty/Data/SoB.rda')


#### Identify Study Area ####
Commodities<-subset(SOBSCCTPU,BucketCode==19)
Commodities<-aggregate(data=Commodities, Insured.Acres ~ FIPS+Commodity.Code+Year, sum)
#Berries
# berries<-subset(SOBSCCTPU,BucketCode==9)
# berries<-aggregate(data=berries, Insured.Acres ~ FIPS+Commodity.Code, sum)
# berries<-reshape(data=berries, idvar=c('FIPS'),v.names='Insured.Acres',timevar="Commodity.Code",direction='wide')
# fipslistberries<-unique(berries$FIPS)
#potatoes
potatoes<-subset(SOBSCCTPU,Commodity.Code==84)
potatoessum<-aggregate(data=potatoes, Insured.Acres ~ FIPS+Year, sum)
potatoessumpol<-aggregate(data=potatoes, Insured.Acres ~ FIPS+Year+Coverage.Level, sum)
fipsyearlistpotatoes<-unique(potatoessum[c('FIPS','Year')])
Commodities<-reshape(data=Commodities, idvar=c('FIPS','Year'),v.names='Insured.Acres',timevar="Commodity.Code",direction='wide')
PotatoesCommodities<-merge(fipsyearlistpotatoes,Commodities,by=c('FIPS','Year'),all.x=TRUE)
CommodityPresenceWithPotatoes<-nrow(PotatoesCommodities)-colSums(is.na(PotatoesCommodities))
mostappearing<-CommodityPresenceWithPotatoes[order(unlist(CommodityPresenceWithPotatoes),decreasing=TRUE)]
#Potatoes... 11 wheat, 15 canola, 16 oats, 32 forage seeding, 33 forage production, 41 corn, 51 grain sorghum, 76 whole farm rev, 78 sunflowers, 81 soybeans, 88 pasture, 91 barley
#Potatoes actually... 11 wheat, 15 canola, 16 oats, 41 corn, 51 grain sorghum, 78 sunflowers, 81 soybeans, 88 pasture, 91 barley
PotCList<-list(11,15,16,41,51,78,81,91)
#list in order of most to least appearing... 11 41 91 16 81 78 32 88 15 51 
PotCListless1<-list(11,15,16,41,78,81,91)
PotCListless2<-list(11,16,41,78,81,91)
PotCListless3<-list(11,16,41,81,91)
write.csv(mostappearing,'D:/CorntoSpecialty/mostappearing.csv', row.names = TRUE)

#Legumes
# Legumes<-subset(SOBSCCTPU,BucketCode==16)
# Legumes<-aggregate(data=Legumes, Insured.Acres ~ FIPS+Commodity.Code, sum)
# Legumes<-reshape(data=Legumes, idvar=c('FIPS'),v.names='Insured.Acres',timevar="Commodity.Code",direction='wide')
# fipslistLegumes<-unique(Legumes$FIPS)

# ##### Generate Summary Graphic ####
setwd("D:/test")
inshp="cb_2018_us_county_500k"
counties<- readOGR(dsn=getwd(), layer=inshp)
counties<-subset(counties,STATEFP != "15" & STATEFP != "02" & STATEFP != "60" & STATEFP != "66" & STATEFP != "69" & STATEFP != "72" & STATEFP != "78")

# potatoes<-potatoes%>%
#   rename(
#     GEOID=FIPS
#   )
# graphdata<-merge(counties,potatoes, by=c('GEOID'))
# graphdata@data$id<-rownames(graphdata@data)
# graphy<-fortify(graphdata,region="id")
# graphdf<-merge(graphy,graphdata@data,by="id")
# 
# graphdf$Insured.Acres[graphdf$Insured.Acres>0]<-1
# graphdf$Insured.Acres[graphdf$Insured.Acres==0 ]<-NA
# 
# p0 <- ggplot(data = graphdf, aes(x = long, y = lat, group = group, fill = factor(Insured.Acres))) +
#   geom_polygon() +
#   geom_path(color = NA, size = 0) +
#   scale_fill_manual(values=c('brown')) +
#   coord_equal() +
#   theme(panel.background=element_blank())+
#   theme(panel.background= element_rect(color="black")) +
#   theme(axis.title = element_blank(), axis.text = element_blank()) +
#   labs(title = "Geographic Extent of Insured Potatoe Acres")+
#   theme(legend.position='none')
# print(p0)
# 
SpecialtyofBusiness<-aggregate(Liability.Amount~FIPS+Commodity.Code+Year,data=SOBSCCTPU,sum,na.rm = FALSE)
SpecialtyofBusiness<-merge(SpecialtyofBusiness,BucketDictionary,by=c('Commodity.Code'),all.x=TRUE)

SpecialtyofBusiness$Specialty<-1
SpecialtyofBusiness$Specialty[SpecialtyofBusiness$BucketCode==19| is.na(SpecialtyofBusiness$BucketCode) ]<-0
SpecialtyofBusiness<-aggregate(Liability.Amount~FIPS+Specialty,data=SpecialtyofBusiness,sum,na.rm = FALSE)

SpecialtyofBusiness<-reshape(data=SpecialtyofBusiness, idvar=c('FIPS'),v.names='Liability.Amount',timevar="Specialty",direction='wide')
SpecialtyofBusiness$PercentSpecialty<-SpecialtyofBusiness$Liability.Amount.1/(SpecialtyofBusiness$Liability.Amount.0+SpecialtyofBusiness$Liability.Amount.1)
SpecialtyofBusiness=subset(SpecialtyofBusiness, select=-c(Liability.Amount.0,Liability.Amount.1))
SpecialtyofBusiness<-SpecialtyofBusiness%>%
  rename(
    GEOID=FIPS
  )


graphdata<-merge(counties,SpecialtyofBusiness, by=c('GEOID'))
 graphdata@data$id<-rownames(graphdata@data)
 graphy<-fortify(graphdata,region="id")
 graphdf<-merge(graphy,graphdata@data,by="id")

 p0 <- ggplot(data = graphdf, aes(x = long, y = lat, group = group, fill = PercentSpecialty)) +
   geom_polygon() +
   geom_path(color = NA, size = 0) +
   scale_fill_gradient(breaks=c(0.25,0.50,0.75), labels=c("25%","50%","75%"),low="grey", high="green") +
 coord_equal() +
  theme(panel.background=element_blank())+
  theme(panel.background= element_rect(color="black")) +
  theme(axis.title = element_blank(), axis.text = element_blank()) +
  labs(fill='% Specialty')
 print(p0)
# 
# 
#  
#  
# # 
# # CommodityDiversityCount<-aggregate(data=SOBSCCTPU, Commodity.Code ~ FIPS+BucketCode, function(x) length(unique(x)))
# # CommodityDiversityCount<-CommodityDiversityCount%>%
# #   rename(
# #     CommodityDiversity=Commodity.Code
# #   )
# # CommodityDiversityCount$CommodityDiversity<-if_else(CommodityDiversityCount$CommodityDiversity>2,1,0)
# #   
# # temp<-reshape(data=CommodityDiversityCount, idvar=c('FIPS'),v.names='CommodityDiversity',timevar='BucketCode',direction='wide')
# # temp[is.na(temp)]<-0
# # 
# # summarise_all(temp,funs(mean))
# # 
# # temp<-subset(SOBSCCTPU,Specialty=1 & BucketCode!=999)
# # CommodityPresenceSpread<-aggregate(data=temp, Commodity.Code ~ FIPS, function(x) length(unique(x)))
# # CommodityPresenceSpread<-CommodityPresenceSpread%>%
# #   rename(
# #     GEOID=FIPS
# #   )
# # graphdata<-merge(counties,CommodityPresenceSpread, by=c('GEOID'))
# # 
# # graphdata@data$id<-rownames(graphdata@data)
# # graphy<-fortify(graphdata,region="id")
# # graphdf<-merge(graphy,graphdata@data,by="id")
# # 
# # p0 <- ggplot(data = graphdf, aes(x = long, y = lat, group = group, fill = Commodity.Code)) +
# #   geom_polygon() +
# #   geom_path(color = NA, size = 0) +
# #   scale_fill_gradient(breaks=c(5,20,40), labels=c("5","20","40"),low="grey", high="green") +
# #   coord_equal() +
# #   theme(panel.background=element_blank())+
# #   theme(panel.background= element_rect(color="black")) +
# #   theme(axis.title = element_blank(), axis.text = element_blank()) +
# #   labs(title = "FCIP Specialty Crop Diversity 2000-2021")+
# #   labs(fill='# Crops Insured')
# # print(p0)
# # 
# # CommodityPresenceCount<-aggregate(data=temp, FIPS ~ Commodity.Code, function(x) length(unique(x)))
# # CommodityPresenceCount<-CommodityPresenceCount%>%
# #   rename(
# #     UniqueFIPS=FIPS
# #   )
# # CommodityPresenceCount<-CommodityPresenceCount[order(-CommodityPresenceCount$UniqueFIPS),]
# # CommodityPresenceCount<-merge(CommodityPresenceCount,BucketDictionary,by=c('Commodity.Code'),all.x=TRUE)
# # SumOfBusinessB<-reshape(data=SumOfBusiness, idvar=c('FIPS','Commodity.Code','Insurance.Plan.Code','Delivery.Type','Coverage.Level',"Coverage.Category"),v.names='Net.Reported.Quantity',timevar='Year',direction='wide')
# # SumOfBusinessB[is.na(SumOfBusinessB)==TRUE]=0
# # SumOfBusinessB<-aggregate(cbind(Net.Reported.Quantity.1989, Net.Reported.Quantity.1990, Net.Reported.Quantity.1991, Net.Reported.Quantity.1992, Net.Reported.Quantity.1993, Net.Reported.Quantity.1994,Net.Reported.Quantity.1995, Net.Reported.Quantity.1996, Net.Reported.Quantity.1997, Net.Reported.Quantity.1998, Net.Reported.Quantity.1999, Net.Reported.Quantity.2000, Net.Reported.Quantity.2001, Net.Reported.Quantity.2002, Net.Reported.Quantity.2003, Net.Reported.Quantity.2004, Net.Reported.Quantity.2005, Net.Reported.Quantity.2006, Net.Reported.Quantity.2007, Net.Reported.Quantity.2008, Net.Reported.Quantity.2009, Net.Reported.Quantity.2010, Net.Reported.Quantity.2011, Net.Reported.Quantity.2012, Net.Reported.Quantity.2013, Net.Reported.Quantity.2014, Net.Reported.Quantity.2015, Net.Reported.Quantity.2016, Net.Reported.Quantity.2017, Net.Reported.Quantity.2018, Net.Reported.Quantity.2019, Net.Reported.Quantity.2020, Net.Reported.Quantity.2021)~ FIPS +Commodity.Code+Coverage.Category,data=SumOfBusinessB,sum,na.rm = FALSE)
# 
# 
# #CoverageDictionary<-unique(SumOfBusinessB[c("Insurance.Plan.Code","Coverage.Level")])
# 
# #SumOfBusinessB$Coverage.Category<-ifelse(SumOfBusinessB$Coverage.Level==0.5,"C","A")
# 
# 
# #### Summarize Key Drivers ####
LargeCropBuckets<-subset(CauseOfLoss,BucketCode==19 | BucketCode==9 |BucketCode==16| BucketCode==8| BucketCode==3| BucketCode==6 | BucketCode==15 )
#LargeCropBuckets<-subset(LargeCropBuckets,Damage.Group==999)
KeyDrivers<-aggregate(cbind(Indemnity.Amount)~BucketCode+Damage.Group,data=LargeCropBuckets,sum,na.rm = FALSE)
KeyDrivers<-reshape(data=KeyDrivers, idvar=c('BucketCode'),v.names='Indemnity.Amount',timevar="Damage.Group", direction='wide')
KeyDrivers[is.na(KeyDrivers)]<-0
l<-as.numeric(ncol(KeyDrivers))
KeyDrivers$Indemnity.Total<-rowSums(KeyDrivers[,c(2:l)])
for (x in 2:l) {
  KeyDrivers[,x]<-round(KeyDrivers[,x]/KeyDrivers$Indemnity.Total,digits=2)
}
table<-t(KeyDrivers)
write.csv(table,'D:/CorntoSpecialty/KeyDriversBuckets.csv', row.names = TRUE)

# 
#### Tables for Thesis ####
sumstatsloss<-merge(fipsyearlistpotatoes,CauseOfLoss,by=c('FIPS','Year'))
sumstatsloss<-subset(sumstatsloss,Commodity.Code %in% PotatoesList)
sumstatsloss<-aggregate(cbind(Determined.Acres) ~ Commodity.Code+Damage.Group,data=sumstatsloss,sum,na.rm= FALSE)
acres<-merge(fipsyearlistpotatoes,InsuredAcres,by=c('FIPS','Year'))
acres<-subset(acres,Commodity.Code %in% PotatoesList)
acres<-aggregate(Insured.Acres~Commodity.Code,data=acres,sum,na.rm=FALSE)
sumstatsloss<-merge(sumstatsloss,acres,by=c('Commodity.Code'))
sumstatsloss$PercentLost<-sumstatsloss$Determined.Acres/sumstatsloss$Insured.Acres
sumstatsloss<-sumstatsloss[,c('Commodity.Code','Damage.Group','PercentLost')]
sumstatsloss<-reshape(data=sumstatsloss, idvar=c('Commodity.Code'),v.names=c('PercentLost'),timevar="Damage.Group", direction='wide')
#sumstatsloss$sum<-rowSums(sumstatsloss[,c(2:12)],na.rm=TRUE)
table<-t(sumstatsloss)
table<-table %>%
  row_to_names(row_number = 1)
lossratioavg<-merge(fipsyearlistpotatoes,SOBSCCTPU,by=c('FIPS','Year'))
lossratioavg<-subset(lossratioavg,Commodity.Code %in% PotatoesList)
lossratioavg<-aggregate(cbind(Indemnity.Amount,Total.Premium.Amount)~Commodity.Code,data=lossratioavg,sum,na.rm=FALSE)
lossratioavg$LossRatio<-lossratioavg$Indemnity.Amount/lossratioavg$Total.Premium.Amount
lossratioavg<-lossratioavg[,c('Commodity.Code','LossRatio')]
lossratioavg<-t(lossratioavg)
lossratioavg<-lossratioavg %>%
  row_to_names(row_number = 1)
table<-rbind(table,lossratioavg)
table[is.na(table)]<-0
table<-round(table,digits=3)
table<-table[order(row.names(table)), ]
write.csv(table,'D:/CorntoSpecialty/SumStatsLoss.csv', row.names = TRUE)

KeyDrivers<-c(1,10,5,6,8,9,999)
# table<-table%>%
#   rename(
#     Wheat='11',
#     Canola='15',
#     Oats='16',
#     Corn='41',
#     GrainSorghum='51',
#     Sunflowers='78',
#     Soybeans='81',
#     Potatoes='84',
#     Barley='91'
#   )
#soball$Reporting.Level.Type<-trimws(soball$Reporting.Level.Type, which=c("both"))

sumstats<-subset(soball,Reporting.Level.Type=='Acres')
sumstats<-aggregate(Insured.Acres ~ Specialty + Year,data=sumstats,sum,na.rm=FALSE)
sumstats<-subset(sumstats,Specialty==1)
sumstats<-sumstats[,c('Year','Insured.Acres')]

ggplot(sumstats,aes(Year,Insured.Acres))+
  geom_bar(stat='identity')+
  xlab("Year")+
  ylab("Insured Acreage")+
  theme(axis.text.x = element_text(angle=-90, vjust = 0.5))


#### Cov Level ####
test<-SOBSCCTPU%>%
  group_by(Commodity.Code,FIPS,Year) %>%
  summarize(weightCovLevel = weighted.mean(Coverage.Level, Insured.Acres))%>%
  as.data.frame()
test<-filter(test, Commodity.Code %in% PotatoesList)
test<-merge(test,fipsyearlistpotatoes,by=c('FIPS','Year'))
CoverageLevels<-reshape(data=test, idvar=c('FIPS','Year'),v.names='weightCovLevel',timevar="Commodity.Code",direction='wide')
potcov<-CoverageLevels[,c('FIPS','Year','weightCovLevel.84')]
save(potcov, file='D:/CorntoSpecialty/Data/CoverageLevels.rda')
CoverageLevels<-CoverageLevels[,c('FIPS','Year','weightCovLevel.84')]
CoverageLevels$weightCovLevel.84<-log(CoverageLevels$weightCovLevel.84)
CoverageLevels[!is.na(CoverageLevels[,c('weightCovLevel.84')]), c('weightCovLevel.84')] <- (CoverageLevels[!is.na(CoverageLevels[,c('weightCovLevel.84')]),c('weightCovLevel.84')]-mean(CoverageLevels[,c('weightCovLevel.84')], na.rm = TRUE))/sd(CoverageLevels[,c('weightCovLevel.84')], na.rm = TRUE)
CoverageLevels[is.na(CoverageLevels[,c('weightCovLevel.84')]), c('weightCovLevel.84')] <- 0
save(CoverageLevels, file='D:/CorntoSpecialty/Data/PotatoeCoverageLevels.rda')

test<-SOBSCCTPU%>%
  group_by(Commodity.Code,FIPS,Year) %>%
  summarize(totalpolicies = sum(Policies.Sold))%>%
  as.data.frame()
test<-subset(test,Commodity.Code==84)
test<-merge(test,fipsyearlistpotatoes,by=c('FIPS','Year'))
PotatoPolicies<-test[,c('FIPS','Year','totalpolicies')]
PotatoPolicies$totalpolicies<-log(PotatoPolicies$totalpolicies)
PotatoPolicies[!is.na(PotatoPolicies[,c('totalpolicies')]), c('totalpolicies')] <- (PotatoPolicies[!is.na(PotatoPolicies[,c('totalpolicies')]),c('totalpolicies')]-mean(PotatoPolicies[,c('totalpolicies')], na.rm = TRUE))/sd(PotatoPolicies[,c('totalpolicies')], na.rm = TRUE)
PotatoPolicies[is.na(PotatoPolicies[,c('totalpolicies')]), c('totalpolicies')] <- 0
save(PotatoPolicies, file='D:/CorntoSpecialty/Data/PotatoePolicies.rda')


#### NASS ####
#write.csv(test,"D:/NASS/test.csv", row.names = FALSE)
old<-read.csv(file='D:/NASS/Corn_Yield_1980_1999.csv')
new<-read.csv(file='D:/NASS/Corn_Yield_2000_2021.csv')
Yield<-rbind(old,new)
Yield$FIPS <- Yield$State.ANSI*1000+Yield$County.ANSI
Yield<-Yield[,c('FIPS','Year','Value')]
Yield<-na.omit(Yield)
Yield$FIPS<-str_pad(Yield$FIPS, 5, pad = "0")
Yield<-Yield[order(Yield$FIPS,Yield$Year),]
cumvar<- function(x) {
  sapply( seq_along(x), function(i) var(x[I(max(c(1,I(i-20)))):I(i-1)]))
}
cummean<- function(x) {
  sapply( seq_along(x), function(i) mean(x[I(max(c(1,I(i-20)))):I(i-1)]))
}
Yield<-Yield %>%
  group_by(FIPS) %>%
  mutate(cvar = cumvar(Value)) %>%
  group_by(FIPS)%>%
  mutate(cmean = cummean(Value))
Yield<-Yield%>%
  rename(
    BaseVariance=cvar,
    BaseMean=cmean
  )
CornYield<-Yield[,c('FIPS','Year','BaseVariance','BaseMean')]
CornYield$Commodity.Code<-41
save(CornYield,file='D:/CorntoSpecialty/Data/CornYield.rda')

old<-read.csv(file='D:/NASS/Barley_Yield_1980_2000.csv')
new<-read.csv(file='D:/NASS/Barley_Yield_2001_2021.csv')
Yield<-rbind(old,new)
Yield$FIPS <- Yield$State.ANSI*1000+Yield$County.ANSI
Yield<-Yield[,c('FIPS','Year','Value')]
Yield<-na.omit(Yield)
Yield$FIPS<-str_pad(Yield$FIPS, 5, pad = "0")
Yield<-Yield[order(Yield$FIPS,Yield$Year),]
cumvar<- function(x) {
  sapply( seq_along(x), function(i) var(x[I(max(c(1,I(i-20)))):I(i-1)]))
}
cummean<- function(x) {
  sapply( seq_along(x), function(i) mean(x[I(max(c(1,I(i-20)))):I(i-1)]))
}

Yield<-Yield %>%
  group_by(FIPS) %>%
  mutate(cvar = cumvar(Value)) %>%
  group_by(FIPS)%>%
  mutate(cmean = cummean(Value))
Yield<-Yield%>%
  rename(
    BaseVariance=cvar,
    BaseMean=cmean
  )
BarleyYield<-Yield[,c('FIPS','Year','BaseVariance','BaseMean')]
BarleyYield$Commodity.Code<-91
save(BarleyYield,file='D:/CorntoSpecialty/Data/BarleyYield.rda')


old<-read.csv(file='D:/NASS/WinterWheat_Yield_1980_1999.csv')
new<-read.csv(file='D:/NASS/WinterWheat_Yield_2000_2021.csv')
Yield<-rbind(old,new)
Yield$FIPS <- Yield$State.ANSI*1000+Yield$County.ANSI
Yield<-Yield[,c('FIPS','Year','Value')]
Yield<-na.omit(Yield)
Yield$FIPS<-str_pad(Yield$FIPS, 5, pad = "0")
Yield<-Yield[order(Yield$FIPS,Yield$Year),]
cumvar<- function(x) {
  sapply( seq_along(x), function(i) var(x[I(max(c(1,I(i-20)))):I(i-1)]))
}
cummean<- function(x) {
  sapply( seq_along(x), function(i) mean(x[I(max(c(1,I(i-20)))):I(i-1)]))
}
Yield<-Yield %>%
  group_by(FIPS) %>%
  mutate(cvar = cumvar(Value)) %>%
  group_by(FIPS)%>%
  mutate(cmean = cummean(Value))
Yield<-Yield%>%
  rename(
    BaseVariance=cvar,
    BaseMean=cmean
  )
WinterWheatYield<-Yield[,c('FIPS','Year','BaseVariance','BaseMean')]
WinterWheatYield$Commodity.Code<-11
save(WinterWheatYield,file='D:/CorntoSpecialty/Data/WinterWheatYield.rda')

old<-read.csv(file='D:/NASS/Soybeans_Yield_1980_1999.csv')
new<-read.csv(file='D:/NASS/Soybeans_Yield_2000_2021.csv')
Yield<-rbind(old,new)
Yield$FIPS <- Yield$State.ANSI*1000+Yield$County.ANSI
Yield<-Yield[,c('FIPS','Year','Value')]
Yield<-na.omit(Yield)
Yield$FIPS<-str_pad(Yield$FIPS, 5, pad = "0")
Yield<-Yield[order(Yield$FIPS,Yield$Year),]
cumvar<- function(x) {
  sapply( seq_along(x), function(i) var(x[I(max(c(1,I(i-20)))):I(i-1)]))
}
cummean<- function(x) {
  sapply( seq_along(x), function(i) mean(x[I(max(c(1,I(i-20)))):I(i-1)]))
}
Yield<-Yield %>%
  group_by(FIPS) %>%
  mutate(cvar = cumvar(Value)) %>%
  group_by(FIPS)%>%
  mutate(cmean = cummean(Value))
Yield<-Yield%>%
  rename(
    BaseVariance=cvar,
    BaseMean=cmean
  )
SoybeansYield<-Yield[,c('FIPS','Year','BaseVariance','BaseMean')]
SoybeansYield$Commodity.Code<-81
save(SoybeansYield,file='D:/CorntoSpecialty/Data/SoybeansYield.rda')

old<-read.csv(file='D:/NASS/Sorghum_Yield_1980_1999.csv')
new<-read.csv(file='D:/NASS/Sorghum_Yield_2000_2021.csv')
Yield<-rbind(old,new)
Yield$FIPS <- Yield$State.ANSI*1000+Yield$County.ANSI
Yield<-Yield[,c('FIPS','Year','Value')]
Yield<-na.omit(Yield)
Yield$FIPS<-str_pad(Yield$FIPS, 5, pad = "0")
Yield<-Yield[order(Yield$FIPS,Yield$Year),]
cumvar<- function(x) {
  sapply( seq_along(x), function(i) var(x[I(max(c(1,I(i-20)))):I(i-1)]))
}
cummean<- function(x) {
  sapply( seq_along(x), function(i) mean(x[I(max(c(1,I(i-20)))):I(i-1)]))
}
Yield<-Yield %>%
  group_by(FIPS) %>%
  mutate(cvar = cumvar(Value)) %>%
  group_by(FIPS)%>%
  mutate(cmean = cummean(Value))
Yield<-Yield%>%
  rename(
    BaseVariance=cvar,
    BaseMean=cmean
  )
SorghumYield<-Yield[,c('FIPS','Year','BaseVariance','BaseMean')]
SorghumYield$Commodity.Code<-51
save(SorghumYield,file='D:/CorntoSpecialty/Data/SorghumYield.rda')

old<-read.csv(file='D:/NASS/Oats_Yield_1980_1999.csv')
new<-read.csv(file='D:/NASS/Oats_Yield_2000_2021.csv')
Yield<-rbind(old,new)
Yield$FIPS <- Yield$State.ANSI*1000+Yield$County.ANSI
Yield<-Yield[,c('FIPS','Year','Value')]
Yield<-na.omit(Yield)
Yield$FIPS<-str_pad(Yield$FIPS, 5, pad = "0")
Yield<-Yield[order(Yield$FIPS,Yield$Year),]
cumvar<- function(x) {
  sapply( seq_along(x), function(i) var(x[I(max(c(1,I(i-20)))):I(i-1)]))
}
cummean<- function(x) {
  sapply( seq_along(x), function(i) mean(x[I(max(c(1,I(i-20)))):I(i-1)]))
}
Yield<-Yield %>%
  group_by(FIPS) %>%
  mutate(cvar = cumvar(Value)) %>%
  group_by(FIPS)%>%
  mutate(cmean = cummean(Value))
Yield<-Yield%>%
  rename(
    BaseVariance=cvar,
    BaseMean=cmean
  )
OatsYield<-Yield[,c('FIPS','Year','BaseVariance','BaseMean')]
OatsYield$Commodity.Code<-16
save(OatsYield,file='D:/CorntoSpecialty/Data/OatsYield.rda')

Yield<-rbind(CornYield,OatsYield)
Yield<-rbind(Yield,SorghumYield)
Yield<-rbind(Yield,SoybeansYield)
Yield<-rbind(Yield,WinterWheatYield)
Yield<-rbind(Yield,BarleyYield)
Yield<-Yield %>%
  ungroup()
Yield<-as.data.frame(Yield)
Yields<-reshape(data=Yield, v.names=c('BaseVariance','BaseMean'),idvar=c('FIPS','Year'),timevar='Commodity.Code',direction='wide')
Yields$Year<-as.character(Yields$Year)
Yields<-merge(fipsyearlistpotatoes,Yields,by=c('FIPS','Year'),all.x=TRUE)
IDS<-Yields[,c('FIPS','Year')]
ToScale<-Yields[,-c(1,2)]
ToScale<-scale(ToScale)
Yields<-cbind(IDS,ToScale)
#Identify least overlapping... by counting nas
colSums(is.na(Yields))
# 51, 91, 
Yields[is.na(Yields)] <- 0

save(Yields,file='D:/CorntoSpecialty/Data/Prev20Yields.rda')
Yields1<-subset(Yields,select=-c(BaseMean.51, BaseVariance.51))
save(Yields1,file='D:/CorntoSpecialty/Data/Prev20Yields1.rda')
Yields2<-subset(Yields1,select=-c(BaseMean.91, BaseVariance.91))
save(Yields2,file='D:/CorntoSpecialty/Data/Prev20Yields2.rda')
# cumscale<- function(x) {
#   sapply( seq_along(x), function(i) scale(x[I(max(c(1,I(i-20)))):I(i-1)]))
# }


####potatoes loss ratio data####
df<-merge(fipsyearlistpotatoes,SOBSCCTPU,by=c('FIPS','Year'))
#dfold<-filter(SOBSCCTPU,FIPS %in% fipslistpotatoes)
df<-filter(df,Commodity.Code %in% PotatoesList)
dfx<-subset(df,Commodity.Code!=84)
dfx1<-filter(dfx,Commodity.Code %in% PotCListless1)
dfx2<-filter(dfx,Commodity.Code %in% PotCListless2)
dfx3<-filter(dfx,Commodity.Code %in% PotCListless3)


dfx<-aggregate(data=dfx, cbind(Insured.Acres,Liability.Amount,Total.Premium.Amount,Indemnity.Amount) ~ FIPS+Commodity.Code+Year, sum)
dfx$LossRatio<-dfx$Indemnity.Amount/dfx$Total.Premium.Amount
LossRatios<-dfx[,cbind('FIPS','Year','Commodity.Code','LossRatio')]
LossRatios$ZeroIndemnities<-if_else(LossRatios$LossRatio==0,1,0)
LossRatios<-LossRatios[order(LossRatios$FIPS,LossRatios$Commodity.Code,LossRatios$Year),]

LossRatios$LossRatio[LossRatios$LossRatio == 0] <- NA
LossRatios<-reshape(data=LossRatios, idvar=c('FIPS','Year'),v.names=c('LossRatio','ZeroIndemnities'),timevar="Commodity.Code", direction='wide')
col<-ncol(LossRatios)/2-1
for(i in 1:col){
  LossRatios[, i*2+1] <- log(LossRatios[,i*2+1])
  LossRatios[!is.na(LossRatios[,i*2+1]), i*2+1] <- (LossRatios[!is.na(LossRatios[,i*2+1]),i*2+1]-mean(LossRatios[,i*2+1], na.rm = TRUE))/sd(LossRatios[,i*2+1], na.rm = TRUE)
  LossRatios[is.na(LossRatios[,i*2+1]), i*2+1] <- 0
  LossRatios[is.na(LossRatios[,i*2+2]), i*2+2] <- 0
}
LossRatiosx<-LossRatios
save(LossRatiosx, file='D:/CorntoSpecialty/Data/LossRatiosx.rda')

dfx1<-aggregate(data=dfx1, cbind(Insured.Acres,Liability.Amount,Total.Premium.Amount,Indemnity.Amount) ~ FIPS+Commodity.Code+Year, sum)
dfx1$LossRatio<-dfx1$Indemnity.Amount/dfx1$Total.Premium.Amount
LossRatios<-dfx1[,cbind('FIPS','Year','Commodity.Code','LossRatio')]
LossRatios$ZeroIndemnities<-if_else(LossRatios$LossRatio==0,1,0)
LossRatios$LossRatio[LossRatios$LossRatio == 0] <- NA
LossRatios<-reshape(data=LossRatios, idvar=c('FIPS','Year'),v.names=c('LossRatio','ZeroIndemnities'),timevar="Commodity.Code", direction='wide')
col<-ncol(LossRatios)/2-1
for(i in 1:col){
  LossRatios[, i*2+1] <- log(LossRatios[,i*2+1])
  LossRatios[!is.na(LossRatios[,i*2+1]), i*2+1] <- (LossRatios[!is.na(LossRatios[,i*2+1]),i*2+1]-mean(LossRatios[,i*2+1], na.rm = TRUE))/sd(LossRatios[,i*2+1], na.rm = TRUE)
  LossRatios[is.na(LossRatios[,i*2+1]), i*2+1] <- 0
  LossRatios[is.na(LossRatios[,i*2+2]), i*2+2] <- 0
}
LossRatiosx1<-LossRatios
save(LossRatiosx1, file='D:/CorntoSpecialty/Data/LossRatiosx1.rda')

dfx2<-aggregate(data=dfx2, cbind(Insured.Acres,Liability.Amount,Total.Premium.Amount,Indemnity.Amount) ~ FIPS+Commodity.Code+Year, sum)
dfx2$LossRatio<-dfx2$Indemnity.Amount/dfx2$Total.Premium.Amount
LossRatios<-dfx2[,cbind('FIPS','Year','Commodity.Code','LossRatio')]
LossRatios$ZeroIndemnities<-if_else(LossRatios$LossRatio==0,1,0)
LossRatios$LossRatio[LossRatios$LossRatio == 0] <- NA
LossRatios<-reshape(data=LossRatios, idvar=c('FIPS','Year'),v.names=c('LossRatio','ZeroIndemnities'),timevar="Commodity.Code", direction='wide')
col<-ncol(LossRatios)/2-1
for(i in 1:col){
  LossRatios[, i*2+1] <- log(LossRatios[,i*2+1])
  LossRatios[!is.na(LossRatios[,i*2+1]), i*2+1] <- (LossRatios[!is.na(LossRatios[,i*2+1]),i*2+1]-mean(LossRatios[,i*2+1], na.rm = TRUE))/sd(LossRatios[,i*2+1], na.rm = TRUE)
  LossRatios[is.na(LossRatios[,i*2+1]), i*2+1] <- 0
  LossRatios[is.na(LossRatios[,i*2+2]), i*2+2] <- 0
}
LossRatiosx2<-LossRatios
save(LossRatiosx2, file='D:/CorntoSpecialty/Data/LossRatiosx2.rda')


dfx3<-aggregate(data=dfx3, cbind(Insured.Acres,Liability.Amount,Total.Premium.Amount,Indemnity.Amount) ~ FIPS+Commodity.Code+Year, sum)
dfx3$LossRatio<-dfx3$Indemnity.Amount/dfx3$Total.Premium.Amount
LossRatios<-dfx3[,cbind('FIPS','Year','Commodity.Code','LossRatio')]
LossRatios$ZeroIndemnities<-if_else(LossRatios$LossRatio==0,1,0)
LossRatios$LossRatio[LossRatios$LossRatio == 0] <- NA
LossRatios<-reshape(data=LossRatios, idvar=c('FIPS','Year'),v.names=c('LossRatio','ZeroIndemnities'),timevar="Commodity.Code", direction='wide')
col<-ncol(LossRatios)/2-1
for(i in 1:col){
  LossRatios[, i*2+1] <- log(LossRatios[,i*2+1])
  LossRatios[!is.na(LossRatios[,i*2+1]), i*2+1] <- (LossRatios[!is.na(LossRatios[,i*2+1]),i*2+1]-mean(LossRatios[,i*2+1], na.rm = TRUE))/sd(LossRatios[,i*2+1], na.rm = TRUE)
  LossRatios[is.na(LossRatios[,i*2+1]), i*2+1] <- 0
  LossRatios[is.na(LossRatios[,i*2+2]), i*2+2] <- 0
}
LossRatiosx3<-LossRatios
save(LossRatiosx3, file='D:/CorntoSpecialty/Data/LossRatiosx3.rda')

dfy<-subset(df,Commodity.Code==84)
dfy<-aggregate(data=dfy, cbind(Insured.Acres,Total.Premium.Amount,Indemnity.Amount) ~ FIPS+Year, sum)
dfy$LossRatio<-dfy$Indemnity.Amount/dfy$Total.Premium.Amount
LossRatios<-dfy[,cbind('FIPS','Year','LossRatio','Insured.Acres')]
LossRatios$ZeroIndemnities<-if_else(LossRatios$LossRatio==0,1,0)
LossRatios$LossRatio[LossRatios$LossRatio == 0] <- NA
  LossRatios[, 3] <- log(LossRatios[,3])
  LossRatios[!is.na(LossRatios[,3]), 3] <- (LossRatios[!is.na(LossRatios[,3]),3]-mean(LossRatios[,3], na.rm = TRUE))/sd(LossRatios[,3], na.rm = TRUE)
  LossRatios[is.na(LossRatios[,3]), 3] <- 0
LossRatiosy<-LossRatios
LossRatiosy <- na.omit(LossRatiosy)
save(LossRatiosy, file='D:/CorntoSpecialty/Data/LossRatiosy.rda')
#0s are unique information.... separate out as separate variable... then recode to 0
# meanwhile, otherwise, log gets close. otherwise the hundred page book offers fomrula , x-mean / std dev
#PotatoesList<-list(11,15,16,41,51,78,81,84,88,91)




####ByCauseDatasets####
df<-merge(fipsyearlistpotatoes,CauseOfLoss,by=c('FIPS','Year'))
df<-filter(df,Commodity.Code %in% PotatoesList)

dfx<-subset(df,Commodity.Code!=84)
dfx<-filter(dfx,Damage.Group %in% KeyDrivers)
dfx1<-filter(dfx,Commodity.Code %in% PotCListless1)
dfx2<-filter(dfx,Commodity.Code %in% PotCListless2)
dfx3<-filter(dfx,Commodity.Code %in% PotCListless3)

dfx<-aggregate(data=dfx, cbind(Determined.Acres) ~ FIPS+Commodity.Code+Year+Damage.Group, sum)
dfx<-merge(dfx,InsuredAcres,by=c('FIPS','Year','Commodity.Code'))
dfx$PercentLoss<-dfx$Determined.Acres/dfx$Insured.Acres

dfx1<-aggregate(data=dfx1, cbind(Determined.Acres) ~ FIPS+Commodity.Code+Year+Damage.Group, sum)
dfx1<-merge(dfx1,InsuredAcres,by=c('FIPS','Year','Commodity.Code'))
dfx1$PercentLoss<-dfx1$Determined.Acres/dfx1$Insured.Acres

dfx2<-aggregate(data=dfx2, cbind(Determined.Acres) ~ FIPS+Commodity.Code+Year+Damage.Group, sum)
dfx2<-merge(dfx2,InsuredAcres,by=c('FIPS','Year','Commodity.Code'))
dfx2$PercentLoss<-dfx2$Determined.Acres/dfx2$Insured.Acres

dfx3<-aggregate(data=dfx3, cbind(Determined.Acres) ~ FIPS+Commodity.Code+Year+Damage.Group, sum)
dfx3<-merge(dfx3,InsuredAcres,by=c('FIPS','Year','Commodity.Code'))
dfx3$PercentLoss<-dfx3$Determined.Acres/dfx3$Insured.Acres

LossPercentagex<-dfx[,cbind('FIPS','Year','Commodity.Code','Damage.Group','PercentLoss')]
LossPercentagex$ZeroIndemnities<-if_else(LossPercentagex$PercentLoss==0,1,0)
LossPercentagex$PercentLoss[LossPercentagex$PercentLoss == 0] <- NA
temp<-reshape(data=LossPercentagex, idvar=c('FIPS','Year','Damage.Group'),timevar="Commodity.Code", direction='wide')
tempy<-reshape(data=temp, idvar=c('FIPS','Year'),timevar='Damage.Group', direction='wide')
col<-ncol(tempy)/2-1
LossPercentagex<-tempy
for(i in 1:col){
  LossPercentagex[!is.na(LossPercentagex[,i*2+1]), i*2+1] <- (LossPercentagex[!is.na(LossPercentagex[,i*2+1]),i*2+1]-mean(LossPercentagex[,i*2+1], na.rm = TRUE))/sd(LossPercentagex[,i*2+1], na.rm = TRUE)
  LossPercentagex[is.na(LossPercentagex[,i*2+1]), i*2+1] <- 0
  LossPercentagex[is.na(LossPercentagex[,i*2+2]), i*2+2] <- 0
}
save(LossPercentagex, file='D:/CorntoSpecialty/Data/Causepercentx.rda')

LossPercentagex<-dfx1[,cbind('FIPS','Year','Commodity.Code','Damage.Group','PercentLoss')]
LossPercentagex$ZeroIndemnities<-if_else(LossPercentagex$PercentLoss==0,1,0)
LossPercentagex$PercentLoss[LossPercentagex$PercentLoss == 0] <- NA
temp<-reshape(data=LossPercentagex, idvar=c('FIPS','Year','Damage.Group'),timevar="Commodity.Code", direction='wide')
tempy<-reshape(data=temp, idvar=c('FIPS','Year'),timevar='Damage.Group', direction='wide')
col<-ncol(tempy)/2-1
LossPercentagex<-tempy
for(i in 1:col){
  LossPercentagex[!is.na(LossPercentagex[,i*2+1]), i*2+1] <- (LossPercentagex[!is.na(LossPercentagex[,i*2+1]),i*2+1]-mean(LossPercentagex[,i*2+1], na.rm = TRUE))/sd(LossPercentagex[,i*2+1], na.rm = TRUE)
  LossPercentagex[is.na(LossPercentagex[,i*2+1]), i*2+1] <- 0
  LossPercentagex[is.na(LossPercentagex[,i*2+2]), i*2+2] <- 0
}
LossPercentagex1<-LossPercentagex
save(LossPercentagex1, file='D:/CorntoSpecialty/Data/Causepercentx1.rda')

LossPercentagex<-dfx2[,cbind('FIPS','Year','Commodity.Code','Damage.Group','PercentLoss')]
LossPercentagex$ZeroIndemnities<-if_else(LossPercentagex$PercentLoss==0,1,0)
LossPercentagex$PercentLoss[LossPercentagex$PercentLoss == 0] <- NA
temp<-reshape(data=LossPercentagex, idvar=c('FIPS','Year','Damage.Group'),timevar="Commodity.Code", direction='wide')
tempy<-reshape(data=temp, idvar=c('FIPS','Year'),timevar='Damage.Group', direction='wide')
col<-ncol(tempy)/2-1
LossPercentagex<-tempy
for(i in 1:col){
  LossPercentagex[!is.na(LossPercentagex[,i*2+1]), i*2+1] <- (LossPercentagex[!is.na(LossPercentagex[,i*2+1]),i*2+1]-mean(LossPercentagex[,i*2+1], na.rm = TRUE))/sd(LossPercentagex[,i*2+1], na.rm = TRUE)
  LossPercentagex[is.na(LossPercentagex[,i*2+1]), i*2+1] <- 0
  LossPercentagex[is.na(LossPercentagex[,i*2+2]), i*2+2] <- 0
}
LossPercentagex2<-LossPercentagex
save(LossPercentagex2, file='D:/CorntoSpecialty/Data/Causepercentx2.rda')

LossPercentagex<-dfx3[,cbind('FIPS','Year','Commodity.Code','Damage.Group','PercentLoss')]
LossPercentagex$ZeroIndemnities<-if_else(LossPercentagex$PercentLoss==0,1,0)
LossPercentagex$PercentLoss[LossPercentagex$PercentLoss == 0] <- NA
temp<-reshape(data=LossPercentagex, idvar=c('FIPS','Year','Damage.Group'),timevar="Commodity.Code", direction='wide')
tempy<-reshape(data=temp, idvar=c('FIPS','Year'),timevar='Damage.Group', direction='wide')
col<-ncol(tempy)/2-1
LossPercentagex<-tempy
for(i in 1:col){
  LossPercentagex[!is.na(LossPercentagex[,i*2+1]), i*2+1] <- (LossPercentagex[!is.na(LossPercentagex[,i*2+1]),i*2+1]-mean(LossPercentagex[,i*2+1], na.rm = TRUE))/sd(LossPercentagex[,i*2+1], na.rm = TRUE)
  LossPercentagex[is.na(LossPercentagex[,i*2+1]), i*2+1] <- 0
  LossPercentagex[is.na(LossPercentagex[,i*2+2]), i*2+2] <- 0
}
LossPercentagex3<-LossPercentagex
save(LossPercentagex3, file='D:/CorntoSpecialty/Data/Causepercentx3.rda')

dfy<-subset(df,Commodity.Code==84)
potatoacres<-subset(InsuredAcres,Commodity.Code==84)
dfy<-aggregate(data=dfy, cbind(Determined.Acres) ~ FIPS+Commodity.Code+Year, sum,na.rm=FALSE)
dfy<-merge(dfy,potatoacres,by=c('FIPS','Year','Commodity.Code'),all.y=TRUE)
dfy$Determined.Acres[is.na(dfy$Determined.Acres)]<-0
dfy$Insured.Acres<-if_else(dfy$Insured.Acres==0,dfy$Determined.Acres,dfy$Insured.Acres)
dfy$PercentLoss<-dfy$Determined.Acres/dfy$Insured.Acres
dfy$NonZeroLoss<-if_else(dfy$PercentLoss>0,1,0)
dfy$PercentLoss<-if_else(dfy$PercentLoss>1,1,dfy$PercentLoss)
dfy <- na.omit(dfy)
dfy<-dfy[,c('FIPS','Year','PercentLoss','NonZeroLoss','Insured.Acres')]
save(dfy, file='D:/CorntoSpecialty/Data/Causey.rda')
#### Weather ####
Weather<-read.csv("D:/DegreeDays/SchlenkerWeather.csv")
Weather<-distinct(Weather)
Weather<-Weather%>%
  rename(
    FIPS=fips,
    Year=year
  )
Weather$FIPS<-as.character(Weather$FIPS)
Weather$FIPS<-str_pad(Weather$FIPS, 5, pad = "0")
Weather$Year<-as.character(Weather$Year)
Weather$Year <- trimws(Weather$Year)
Weather$FIPS <- trimws(Weather$FIPS)
Weather<-merge(fipsyearlistpotatoes,Weather,by=c('FIPS','Year'),all.x=TRUE)
Weather<-Weather[,c(1:5)]
Weather$prec2<-Weather$prec*Weather$prec
ToScale<-Weather[,c(3:6)]
ToScale<-scale(ToScale)
IDS<-Weather[,c(1:2)]
WeatherLag<-cbind(IDS,ToScale)
Weather[is.na(Weather)] <- 0
save(Weather,file='D:/CorntoSpecialty/Data/Weather.rda')


Weather<-Weather %>%
  group_by(FIPS) %>%
  mutate(cvarprec = cumvar(prec)) 
Weather<-Weather %>%
  group_by(FIPS) %>%
  mutate(cmeanprec = cummean(prec)) 
Weather<-Weather %>%
  group_by(FIPS) %>%
  mutate(cvarprec2 = cumvar(prec2)) 
Weather<-Weather %>%
  group_by(FIPS) %>%
  mutate(cmeanprec2 = cummean(prec2)) 
Weather<-Weather %>%
  group_by(FIPS) %>%
  mutate(cvardday10C = cumvar(dday10C)) 
Weather<-Weather %>%
  group_by(FIPS) %>%
  mutate(cmeandday10C = cummean(dday10C)) 
Weather<-Weather %>%
  group_by(FIPS) %>%
  mutate(cvardday30C = cumvar(dday30C)) 
Weather<-Weather %>%
  group_by(FIPS) %>%
  mutate(cmeandday30C = cummean(dday30C)) 
ToScale<-Weather[,c(7:14)]
ToScale<-scale(ToScale)
IDS<-Weather[,c(1,2)]
WeatherLag<-cbind(IDS,ToScale)
WeatherLag[is.na(WeatherLag)] <- 0
WeatherLag<-as.data.frame(WeatherLag)
WeatherLag<-WeatherLag%>%
  rename(
    PrecipVar=cvarprec,
    PrecipMean=cmeanprec,
    Precip2Var=cvarprec2,
    Precip2Mean=cmeanprec2,
    DDay10CMean=cmeandday10C,
    DDay10CVar=cvardday10C,
    DDay30CMean=cmeandday30C,
    DDay30CVar=cvardday30C
  )
save(WeatherLag,file='D:/CorntoSpecialty/Data/WeatherLag.rda')

#### Drought ####

# load(file='D:/DroughtEssay/Data/AllDroughtYears.Rda')
# GrainCornYearlyDrought$DroughtOccur<-ifelse((GrainCornYearlyDrought$D4+GrainCornYearlyDrought$D3+GrainCornYearlyDrought$D2)>1 ,1,0)
# GrainCornYearlyDrought<-GrainCornYearlyDrought[order(GrainCornYearlyDrought$FIPS,GrainCornYearlyDrought$Year),]
# GrainCornYearlyDrought<-GrainCornYearlyDrought %>%
#   group_by(FIPS)%>%
#   mutate(lagDroughtOccur = dplyr:: lag(DroughtOccur,n=1,default=NA))
# Drought<-GrainCornYearlyDrought[,c('FIPS','Year','DroughtOccur','lagDroughtOccur')]
# Drought<-merge(fipsyearlistpotatoes,Drought,by=c('FIPS','Year'))
# Drought$Year <- trimws(Drought$Year)
# Drought$FIPS <- trimws(Drought$FIPS)
# save(Drought,file='D:/CorntoSpecialty/Data/Drought.rda')


##### testing ####

# 
# p<-subset(CauseOfLoss,Commodity.Code==84)
# d<-aggregate(cbind(Determined.Acres)~FIPS+Year,data=p,sum,na.rm=FALSE)
# s<-subset(SOBSCCTPU,Commodity.Code==84)
# a<-aggregate(cbind(Insured.Acres,Liability.Amount,Indemnity.Amount)~FIPS+Year,data=s,sum,na.rm = FALSE)
# test<-merge(a,d,by=c('FIPS','Year'),all.x=TRUE)
# test$Determined.Acres[is.na(test$Determined.Acres)]<-0
# test$percentloss<-test$Determined.Acres/test$Insured.Acres
# test$percentlossdollars<-test$Indemnity.Amount/test$Liability.Amount
# plot(test$percentloss,test$percentlossdollars)
# 
# test<-merge(test,potcov,by=c('FIPS','Year'),all.x=TRUE)
# test$postdeduct<-test$weightCovLevel.84*test$Liability.Amount
# test$postdeductpld<-test$Indemnity.Amount/test$postdeduct
# plot(test$percentloss,test$postdeductpld)
