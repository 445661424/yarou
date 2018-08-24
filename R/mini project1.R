members<-data.frame(matrix(nrow=5, ncol=8)) #Establish blank data frame for names
colnames(members)<-c("s1","s2","t1","t2","a1","a2","b1","b2")#name of columns in this data frame are parts of the choir
parts<-colnames(members)#set the parts
parts
members$rownames = rep(1:5)#for convenience in setting names, create a column for later use
members

for (part in colnames(members)){ #for each number represents the member 
  for(number in members$rownames){ #combine with the part he or she is in
    name<-paste(part,number)#is the name of each member
    members[number,part]<-name#combine the part and the number is the code of each member
  }#in real life, name are real names rather than codes, I use code here for convenience
}#what we do in practice, we can upload an excel file containing all names and with last two queries to finish this case

members$rownames <- NULL#delete the last column

# Try to create randomly sampled data frame

selection<-apply(members, 2, function(d) sample(d, 1))#randomly shuffle 
selection#show the result
