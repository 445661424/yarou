####################Q1###################
###Extract positive reviews content#######
#######For Airline reviews################
#######Using Sentiment Analysis##########


rm(list=ls())
library(tm)
library(DBI)
library(e1071)
library(maxent)
library(pROC) # install.packages("pROC")
library(DBI)
library(RMySQL)
library(tidytext)

myhost <- "localhost"
mydb <- "studb"
myacct <- "cis434"
mypwd <- "LLhtFPbdwiJans8F@S207"
driver <- dbDriver("MySQL")
conn <- dbConnect(driver, host=myhost, dbname=mydb,
                  myacct, mypwd)
data1 <- dbGetQuery(conn, "SELECT * FROM classification WHERE rtag LIKE 'vC}l9a33\\'Trs'")
dbDisconnect(conn)

#extract data set

library(magrittr)
library(janeaustenr)
library(dplyr)
library(tidyr)
library(stringr)
library(tidytext)
library(glue)
library("dplyr")
bing <- get_sentiments("bing")

#packages needed and lexicon

data1 <- data1 %>%unnest_tokens(word,tweet) 
data1 <- data1 %>%anti_join(get_stopwords())

#token data into words

text <- data1 %>%
  inner_join(bing) %>%#input bing as valuation method
  count(id, sentiment, sort = TRUE) %>% #group by id and tag words it contains into pos and neg
  spread(sentiment, n, fill = 0) %>%
  mutate(sentiment = positive - negative)%>%  #create new column named sentiment as bigger proportioned sentiment as sentiment of this tweet
  filter(sentiment>0) %>% #extract those tweets are not negative
  ungroup()

nrow(text)#356

m <- matrix(0, ncol = 2, nrow = 356)
m <- data.frame(m)#create a blank data frame as draft final csv
colnames(m) <- c("id","tweet")#name two columns as id and tweet
m$id<-text$id#input positive tweets id in to draft

myhost <- "localhost"
mydb <- "studb"
myacct <- "cis434"
mypwd <- "LLhtFPbdwiJans8F@S207"
driver <- dbDriver("MySQL")
conn <- dbConnect(driver, host=myhost, dbname=mydb,
                  myacct, mypwd)
data1 <- dbGetQuery(conn, "SELECT * FROM classification WHERE rtag LIKE 'vC}l9a33\\'Trs'")
dbDisconnect(conn)

#the above procedures are written to retrieve original sentences of tweet since the current data1 file we
#get is contained with words instead of sentences, because the previous one was overwritte for the same name


result <- cbind(m, data1[match(m$id, data1$id),c(1,5)])#if id matches in both dataframes, extract tweet content
result<-result[,c(3,4)] # put the content into the draft file 

write.csv(result,"yaruo_zhang_1.csv") # write out as csv


#############################################################
#######Q2 Identify food trend through word frequency##########
#following codes are written to count frequency of cauliflower
##############################################################

###First thing first, my each count function for each keyword 
###takes my one hour to get the trend plot###################
#################For your information########################

keywordcauli<-data.frame(matrix(nrow=60, ncol=3)) #Establish blank data frame for cauliflower accuracy
colnames(keywordcauli)<-c("year","month","count")#Name of columns in this data frame
keywordcauli$year<-rep(2011:2015, each = 12) #Each Year
keywordcauli$month<-rep(1:12, times=5) #Each Month
keywordcauli$count<-NA #Set the count as NA first

fileadress<-setwd("D:/CIS434/data")#list all files in text

for (year in keywordcauli$year){ #for each year 
  for(month in keywordcauli$month){ #for each month
    name<-paste("fpost-",year,"-",month,".csv",sep="")#list names of files
    con <- file(name, open='r')#open files
    text <- readLines(con)#read csv
    close(con)#close
    keywordcauli$count[keywordcauli$month==month&keywordcauli$year==year]<-length(grep("cauliflower rice",text,ignore.case=TRUE))
  }#last step:calculate the frequency of word and put frequency on the third column 
}

keywordcauli$date <-as.Date(paste(paste(keywordcauli$year, keywordcauli$month, sep = "/"),"/01",sep=""))
#change the class of time into as.date to plot them

plot(keywordcauli$date,keywordcauli$count,
     main="Trend for Cauliflower",xlab="Time",ylab="Frequency",type = "l") 

#plot the frequency of cauli flower as time goes on

#############################################################
#following codes are written to count frequency of pumpkin pie
##############################################################

keywordpp<-data.frame(matrix(nrow=60, ncol=3)) #Establish blank data frame for pumpkin pie accuracy
colnames(keywordpp)<-c("year","month","count")#Name of columns in this data frame
keywordpp$year<-rep(2011:2015, each = 12) #Each Year
keywordpp$month<-rep(1:12, times=5) #Each Month
keywordpp$count<-NA #Set the count as NA first

fileadress<-setwd("D:/CIS434/data")#set work directory

for (year in keywordpp$year){#for each year 
  for(month in keywordpp$month){#for each month
    name<-paste("fpost-",year,"-",month,".csv",sep="")#list names of files
    con <- file(name, open='r')#open files
    text <- readLines(con)#read csv
    close(con)#close
    keywordpp$count[keywordpp$month==month&keywordpp$year==year]<-length(grep("pumpkin pie",text,ignore.case=TRUE))
  }#last step:calculate the frequency of word and put frequency on the third column 
}

keywordpp$date <-as.Date(paste(paste(keywordpp$year, keywordpp$month, sep = "/"),"/01",sep=""))
#change the class of time into as.date to plot them
plot(keywordpp$date,keywordpp$count,
     main="Trend for pumpkin pie",xlab="Time",ylab="Frequency",type = "l")
#plot the frequency of pumpkin pie


#############################################################
#following codes are written to count frequency of vegetable noodles
##############################################################

keywordvn<-data.frame(matrix(nrow=60, ncol=3)) #Establish blank data frame for cauliflower accuracy
colnames(keywordvn)<-c("year","month","count")#Name of columns in this data frame
keywordvn$year<-rep(2011:2015, each = 12) #Each Year
keywordvn$month<-rep(1:12, times=5) #Each Month
keywordvn$count<-NA #Set the count as NA first

fileadress<-setwd("D:/CIS434/data")#set the work directory

toMatch<-c("vegetable noodle","zoodle","veggie spagehtti","veggie pasta","spralizer")
#set the vegetable noodle pattern

for (year in keywordvn$year){#for each year 
  for(month in keywordvn$month){#for each month
    name<-paste("fpost-",year,"-",month,".csv",sep="")#list names of files
    con <- file(name, open='r')#open and read files
    text <- readLines(con)#read text content
    close(con)#close
    keywordvn$count[keywordvn$month==month&keywordvn$year==year]<-length(grep(paste(toMatch,collapse="|"), 
                                                                              text, value=TRUE,ignore.case = TRUE))
    
  }#last step:calculate the frequency of word and put frequency on the third column 
}

keywordvn$date <-as.Date(paste(paste(keywordvn$year, keywordvn$month, sep = "/"),"/01",sep=""))
#change the class of time into as.date to plot them
plot(keywordvn$date,keywordvn$count,
     main="Trend for All Vegetable Noodle",xlab="Time",ylab="Frequency",type = "l")
#plot the frequency of vegetable noodle as time goes on

#############################################################
#calculate the number of tweets#############################
##############################################################

numberoftweet<-data.frame(matrix(nrow=60, ncol=3)) #Establish blank data frame for cauliflower accuracy
colnames(numberoftweet)<-c("year","month","count")#Name of columns in this data frame
numberoftweet$year<-rep(2011:2015, each = 12) #Each Year
numberoftweet$month<-rep(1:12, times=5) #Each Month
numberoftweet$count<-NA #Set the count as NA first

for (year in numberoftweet$year){#for each year 
  for(month in numberoftweet$month){#for each month
    name<-paste("fpost-",year,"-",month,".csv",sep="")#list names of files
    con <- file(name, open='r')#open files
    text <- read.csv(con,header = FALSE)#read text content
    numberoftweet$count[numberoftweet$month==month&numberoftweet$year==year]<-nrow(text)
    close(con)#close
  }
}
numberoftweet$date <-as.Date(paste(paste(numberoftweet$year, numberoftweet$month, sep = "/"),"/01",sep=""))
#change the class of time into as.date to plot them
plot(numberoftweet$date,numberoftweet$count,
     main="Trend for Tweet Volumn",xlab="Time",ylab="Frequency",type = "l")
###############################################################################

