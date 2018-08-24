
rm(list=ls())

library(dplyr)
library(xlsx)
library(earth)
#########This is the combination of all results from teammates#################
############I will be more than happy to connect with them on Github##########
#######################To show as the project work#########################
####Reading all the datasets####

#set the directory

setwd("C:/Users/Deepam Jain/Downloads/Practicum/New data/")

assets <- read.csv("Asset_features_RCSD.csv",header = TRUE)

failures <- read.csv("Failures_RCSD.csv")
names(failures) <- c("AssetID","ReadDate","ProblemTypeName")

volume <- read.csv("Volume_RCSD.csv")
names(volume) <- c("AssetID","ReadDate","Volume")

####Transforming the data####

#Change data format
volume$modDate = as.Date(volume$ReadDate, format="%m/%d/%Y %I:%M:%S %p")

#aggregating for only one day and removing negative values
volume_final <- volume[volume$Volume>=0,]
volume <- aggregate(Volume ~ AssetID+modDate,data=volume_final,FUN=sum)
rm(volume_final)

#cleaning failure data also

failures$modDate = as.Date(failures$ReadDate, format="%B %d,%Y")

#Specify the specific failure type here

#failures <- failures[failures$ProblemTypeName=='Paper Jam',]

#keeping only one failure per day if there are more than one on any single day

failures <- failures[!is.na(failures$modDate),]
failures <- aggregate(ProblemTypeName~AssetID+modDate,data=failures,FUN=length)

assets$AssetNumericID <- NULL
failures$ReadDate <- NULL

#merging failure and volume data to get a final data to be used for further operation

asset_volume <- merge(assets,volume,by='AssetID')
asset_failure <- merge(assets,failures,by='AssetID')

final_data <- merge(asset_volume,asset_failure,by=c("AssetID", "ModelName", "ModelClass", "IsColor",
                                                    "PPM", "ColorPPM", "Utilization", "IsPrinter", "IsScanner", "IsCopier",
                                                    "IsFax", "ageInMonths","modDate"),all.x=TRUE,all.y=TRUE)


rm(asset_volume)
rm(asset_failure)


####follwing code filters out the data after the last failure####

maxDate <- aggregate(modDate ~ AssetID,data=failures,FUN=max)
newData <- final_data[c(0),]

for (i in 1:nrow(maxDate))
{
  interData = final_data[final_data$AssetID == maxDate[i,'AssetID'],]
  tempData = interData[interData$modDate <= maxDate[i,'modDate'],]
  newData = rbind(newData,tempData)
}
rm(maxDate)
rm(interData)
rm(tempData)
final_data <- newData
rm(newData)

head(final_data)


####getting the days to failure at each failure####

startDate <- aggregate(modDate ~ AssetID,data=final_data,FUN=min)
startDate$ProblemTypeName <- NA
startDate <- startDate[c("AssetID","ProblemTypeName","modDate")]

failureData = rbind(failures,startDate)
failureData = unique(failureData)
failureData = failureData[order(failureData$AssetID,failureData$modDate,failureData$ProblemTypeName,na.last = FALSE),]
failureData <- mutate(failureData,prevDate=lag(failureData$modDate))

failureData$daystofailure <- failureData$modDate - failureData$prevDate

failureData <- failureData[!is.na(failureData$ProblemTypeName),]
failureData <- failureData[failureData$daystofailure>=0,]

plot(failureData$daystofailure)

#making date a continuous column for all the assets so that no dates are missing for any asset

minDate <- aggregate(modDate ~ AssetID,data=final_data,FUN=min)
maxDate <- aggregate(modDate ~ AssetID,data=final_data,FUN=max)

newData <- minDate[c(0),]

for(i in 1:nrow(minDate))
{
  days = as.integer(maxDate[i,2]-minDate[i,2])
  AssetID = rep(maxDate[i,1],days+1)
  modDate = seq(minDate[i,2],maxDate[i,2],"days")
  tempData <- data.frame(AssetID,modDate) 
  newData <- rbind(newData,tempData)
}
rm(minDate)
rm(maxDate)
rm(tempData)
rm(AssetID)
rm(days)
rm(i)
rm(modDate)

final_data1 <- merge(newData,volume,by=c('AssetID','modDate'),all.x = TRUE)
final_data2 <- merge(final_data1,failures,by=c('AssetID','modDate'),all.x = TRUE)
final_data3 <- merge(final_data2,failureData,by=c('AssetID','ProblemTypeName','modDate'),all.x = TRUE)
final_data <- merge(assets,final_data3,by=c('AssetID'))
final_data$prevDate <- NULL
final_data$Volume <- ifelse(is.na(final_data$Volume),0,final_data$Volume)
final_data <- final_data[order(final_data$AssetID,final_data$modDate,na.last = FALSE),]
final_data$failure <- ifelse(is.na(final_data$ProblemTypeName),0,1)
final_data <- final_data[order(final_data$AssetID,final_data$modDate,final_data$ProblemTypeName,na.last = FALSE),]
final_data$ProblemTypeName <- NULL

rm(newData)
rm(final_data1)
rm(final_data2)
rm(final_data3)
rm(failureData)
rm(startDate)


####getting cumulative volume as well as cumulative days to failure####

final_data$cumVol <- 0
final_data[1,'cumVol'] = final_data[1,'Volume']

intermediateData <- final_data[!is.na(final_data$daystofailure),]
firstDay <- intermediateData[1,'daystofailure']
intermediateData <- mutate(intermediateData,prevDays=lead(daystofailure))

intermediateData <- intermediateData[,c("AssetID","modDate","prevDays")]
final_data <- merge(final_data,intermediateData,by=c("AssetID","modDate"),all.x=TRUE)
final_data$daystofail <- NA
final_data[1,'daystofail'] <- firstDay
rm(firstDay)
rm(i)

rm(intermediateData)

for (i in 2:nrow(final_data))
{
  if (final_data[i,'AssetID'] == final_data[i-1,'AssetID'])
  {
    if (final_data[i-1,'failure'] == 1)
    {
      final_data[i,'cumVol'] = final_data[i,'Volume']
      final_data[i,'daystofail'] = final_data[i-1,'prevDays']-1
    }
    else
    {
      final_data[i,'cumVol'] = final_data[i-1,'cumVol'] + final_data[i,'Volume']
      final_data[i,'daystofail'] = final_data[i-1,'daystofail']-1 
    }
  }
  else
  {
    final_data[i,'cumVol'] = final_data[i,'Volume']
    
    if (final_data[i,'failure'] == 1)
    {
      final_data[i+1,'daystofail'] = final_data[i,'prevDays']-1
      final_data[i,'daystofail'] = 0
    }
    else {final_data[i,'daystofail'] = final_data[i-1,'prevDays']}
  }
}


final_data$prevDays <- NULL
final_data$daystofailure <- NULL

#####adding age of all the devices####

final_data$Month_Yr <- format(final_data$modDate, "%Y-%m")

final_mod <- aggregate(modDate~AssetID+Month_Yr,data = final_data,FUN=max)

final_mod$ageInMths <- ave(final_mod$Month_Yr,final_mod$AssetID,FUN=seq_along)
final_mod$modDate<- NULL

final_data <- merge(final_data,final_mod,by = c("AssetID","Month_Yr"), all.x = TRUE)

final_data$Month_Yr <- NULL
final_data <- final_data[,c("AssetID","ModelName","ModelClass","IsColor","PPM","ColorPPM","Utilization","IsPrinter","IsScanner","IsCopier","IsFax","ageInMonths","modDate","Volume","ageInMths","cumVol","daystofail","failure")]
final_data$ageInMths <- as.numeric(final_data$ageInMths)
final_data$Utilization <- as.numeric(final_data$Utilization)
final_data <- final_data[final_data$Utilization!=0,]
final_data$ageInMths <- final_data$ageInMths+final_data$ageInMonths
rm(final_mod)

######adding a new feature - days since failure######

final_data$daysSinceFailure <- NA
final_data[,'daysSinceFailure'] <- ifelse(final_data[,'failure']==1,0,NA)
final_data[1,'daysSinceFailure']<-1

for (i in 2:nrow(final_data))
{
  if (final_data[i,'AssetID'] != final_data[i-1,'AssetID'])
  {
    final_data[i,'daysSinceFailure'] = 1
  }
  
  else
  {
    if (final_data[i-1,'failure']==1)
    {
      final_data[i,'daysSinceFailure'] = 1
    }
    else {final_data[i,'daysSinceFailure'] = final_data[i-1,'daysSinceFailure']+1}
  }  
}

###added the previous failures

final_data$prevfailures <- ave(final_data$failure, final_data$AssetID, FUN=cumsum)

###added the total volume

final_data$totvol <- ave(final_data$Volume, final_data$AssetID, FUN=cumsum)

write.csv(final_data,"final_data.csv")

#Ran 
#survival analysis 
install.packages('survival')
library(survival)
install.packages('rms')
install.packages('survminer')
install.packages('ggpubr')
install.packages('magrittr')
library(survminer)
library(ggpubr)
library(magrittr)

#uploaded the data that include everything
library(readr)
final_data<- read_csv("~/Desktop/final_data_that_has_everything.csv")
View(final_data)

failureasset<- final_data[final_data$failure=='1',]

#for just one asset 
Asset1 <- failureasset[failureasset$AssetID=='000C52FB-9DA1-E011-B723-0024E861B15C',]
Asset1

#survival analysis for asset 1 
model <- survreg(Surv(daysSinceFailure,failure) ~ageInMths+cumVol,data=Asset1,dist='w')
summary(model)

p1 <- predict(model,newdata = data.frame(ageInMths=111,cumVol=12407),type='response')
p1
library(lubridate)
ymd('20140825')+days(134)#2015-01-06

#we did the survival analysis for just one asset.
#The strength of one-asset model is that for the same asset, the only two features which will be changed is ageInMths and cumvolumn.
#Therefore, if we provide the information of ageInMths and cumvol, the prediction will be more accurate.
#The drawback of this classification is that we have more than 200 asset in our case, and it is difficult to include more than 200 models.


##################################
######## MSE for asset2 #########
#################################

Asset2 <- failureasset[failureasset$AssetID =='00EAD915-3CD5-E111-97B2-0025B500016E',]

train_asset2 <- Asset2[Asset2$modDate <='2015-01-01',]
test_asset2 <- Asset2[Asset2$modDate > '2015-01-01',]

model_asset2 <- survreg(Surv(daysSinceFailure,failure) ~ageInMths+cumVol,data=train_asset2,dist='w')
summary(model_asset2)

pre_asset2 <- predict(model_asset2,newdata = data.frame(
  ageInMths = test_asset2$ageInMths,
  cumVol=test_asset2$cumVol
),type='response')

pre_asset2
sqrt(mean((test_asset2$daysSinceFailure-pre_asset2)^2))

#now we check the MSE for asset2, and it is a little high.
#we were wondering that if there are more data point for one asset, the mse will decrease. 
#so we check which asset has the most data point.
#######################################################
#############check which 
install.packages('gsubfn')
library(gsubfn)
install.packages("proto")
library(proto)
install.packages('RSQLite')
library(RSQLite)
library(sqldf)



sqldf("SELECT AssetID, COUNT('AssetID') AS number FROM failureasset GROUP BY AssetID ORDER BY number DESC LIMIT 5 ")
#                               AssetID number
#1 D1D3852A-BCDA-E111-AD34-0025B500016E     62
#2 97FE0553-D5D1-E111-97B2-0025B500016E     56
#3 ECF6F316-3DE0-E111-AD34-0025B500016E     55
#4 7E1C98B4-56F3-E111-AEC9-0025B500016E     53
#5 CC3CEDA4-D92C-E211-A66B-0025B500017E     53

##############################################################
########MSE for Asset which have most data point##############
##############################################################


mostasset <- failureasset[failureasset$AssetID=='D1D3852A-BCDA-E111-AD34-0025B500016E',]
train_mostasset <- mostasset[mostasset$modDate <='2016-01-01',]
test_mostasset <- mostasset[mostasset$modDate > '2016-01-01',]


model_mostasset <- survreg(Surv(daysSinceFailure,failure) ~ageInMths+cumVol,data=train_mostasset,dist='w')

summary(model_mostasset)

pre_mostasset<- predict(model_mostasset,newdata = data.frame(
  ageInMths = test_mostasset$ageInMths,
  cumVol=test_mostasset$cumVol
),type='response')

pre_mostasset
sqrt(mean((test_mostasset$daysSinceFailure-pre_mostasset)^2))

#we found that when there are more data information for one asset, the mse will decrease. 
#the mse is 8.650411

##################################################
###########MSE for the asset2 ###################
#################################################
secondasset <- failureasset[failureasset$AssetID =='97FE0553-D5D1-E111-97B2-0025B500016E',]

train_secondasset <- secondasset[secondasset$modDate <= '2016-01-01',]
test_secondasset <- secondasset[secondasset$modDate > '2016-01-01',]

model_secondasset <-  survreg(Surv(daysSinceFailure,failure) ~ageInMths+cumVol,data=train_secondasset,dist='w')
summary(model_secondasset)

pre_secondasset <- predict(model_secondasset,newdata = data.frame(
  ageInMths = test_secondasset$ageInMths,
  cumVol=test_secondasset$cumVol
),type='response')

pre_secondasset
sqrt(mean((test_secondasset$daysSinceFailure-pre_secondasset)^2))

########

rm(list=ls())
library(caret)

setwd("~/Desktop/Project/submitted")
load("knnmodel.rda") # load the model, which is for cluster 1

predict_daystofail <- function(){
  
  h <- readline(prompt="IsColor:")
  h = as.integer(h)
  f <- readline(prompt="PPM:")
  f = as.integer(f)
  y <- readline(prompt="ColorPPM:")
  y = as.integer(y)
  t <- readline(prompt="IsPrinter:")
  t <- as.integer(t)
  v <- readline(prompt="IsFax:")
  v <- as.integer(v)
  a <- readline(prompt="ageInMonths:")
  a <- as.integer(a)
  d <- readline(prompt="cumVol:")
  d <- as.integer(d)
  b <- readline(prompt="prevfailures:")
  b <- as.integer(b)
  c <- readline(prompt="totvol:")
  c <- as.integer(c)
  e <- readline(prompt="AssetID:")
  e <- as.factor(e)
  
  df = data.frame(h,f,y,t,v,a,d,b,c,e)
  x <- c("IsColor","PPM","ColorPPM","IsPrinter","IsFax","ageInMonths","cumVol","prevfailures","totvol","AssetID" )
  colnames(df) <- x
  df
  
  return(predict(model,df))
}

# load the data
final_data = read.csv('final_data.csv')
# subset for cluster 1
data1 = final_data[final_data$classcluster==1,]

# split into train and test datasets
set.seed(1)
isTraining = runif(nrow(data1)) < .8
training1 = subset(data1,isTraining)
test1 = subset(data1,!isTraining)

# test the function
predict_daystofail()

#####################################################################
## Classification: whether the asset is expected to fail in 10 days##
#####################################################################

load("classificationcluster1.rda")
predict_daystofail()

#Ran 
#survival analysis 
install.packages('survival')
library(survival)

#uploaded the data that include everything
library(readr)
final_data<- read_csv("~/Desktop/final_data_that_has_everything.csv")
View(final_data)

#for failure = 1
failureasset <- final_data[final_data$failure==1,]
failureasset

period <- failureasset$daysSinceFailure
status <- failureasset$failure
cluster <- failureasset$Cluster


#plot the line of survival model
install.packages('survminer')
install.packages('ggpubr')
install.packages('magrittr')
library(survminer)
library(ggpubr)
library(magrittr)
plotfit <-survfit(Surv(period,status)~1,data=failureasset)
survplot(plotfit)

#summary the survival analysis 
nasur<-survfit(coxph(Surv(period,status) ~1),type='aalen')
summary(nasur)


model2 <-survfit(Surv(period,status)~cluster,data=failureasset)
ggsurvplot(model2,data=failureasset)

#sub-cluster part
#the number of sub-cluster in cluster1
(3110/(358+3110+526))*15 #12
#but unique(ppm) in cluster 1
unique(cluster1$PPM)  #10
#the number of sub-cluster in cluster2
(358/(358+3110+526))*15 #1
#the number of sub-cluster in cluster3
15-12-1 #2


#for cluster 1 
cluster1 <- as.data.frame(failureasset[failureasset$Cluster==1,])
cluster1
summary(cluster1$PPM)
cluster1$ppmcluster <- NA

ppmCluster <- as.data.frame(unique(cluster1[,'PPM']))

for (i in 1:nrow(ppmCluster))
{
  cluster1[cluster1$PPM==ppmCluster[i,],'ppmcluster'] <- i
}

time <- cluster1$daysSinceFailure
failure <- cluster1$failure
subgroup <- cluster1$ppmcluster

#plot for cluster 1
cluster1model <- survfit(Surv(time,failure)~subgroup,data=cluster1)
ggsurvplot(cluster1model,data=cluster1)

#
cluster1nasur<-survfit(coxph(Surv(time,failure) ~1),type='aalen')
summary(cluster1nasur)
#for cluster 2

cluster2 <- as.data.frame(failureasset[failureasset$Cluster==2,])
cluster2
summary(cluster2$PPM)
cluster2$ppmcluster <- 11


#for cluster 3
cluster3 <- as.data.frame(failureasset[failureasset$Cluster==3,])
cluster3
summary(cluster3$PPM)
unique(cluster3$PPM)
cluster3$ppmcluster <- NA

ppmCluster <- as.data.frame(unique(cluster3[,'PPM']))

for (i in 1:nrow(ppmCluster))
{
  cluster3[cluster1$PPM==ppmCluster[i,],'ppmcluster'] <- i
}

#survival analysis

install.packages('survival')
library(survival)

time <- final_data$daysSinceFailure
event <- final_data$failure
x <- cbind(final_data$IsColor,final_data$PPM,final_data$ColorPPM,final_data$IsPrinter,final_data$ageInMonths,final_data$cumVol)
group <- final_data$Cluster
#summary
summary(time)
summary(event)
#model 
kmsurvival <- survreg(Surv(time,event)~1)
summary(kmsurvival)

kmcurve <- survfit(Surv(time,event)~1)
summary(kmcurve)
plot(kmcurve)

predict(kmsurvival,final_data)
groupsurvival <- survfit(Surv(time,event)~group)
summary(groupsurvival)
qplot(groupsurvival)

nasurvival <-survfit(coxph(Surv(time,event) ~1),type='aalen')
summary(nasurvival)
plot(nasurvival)



