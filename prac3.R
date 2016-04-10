

setwd("c:\\ml\\kaggle\\comp3\\")

library(neuralnet)
library(dplyr)
library(kknn)
library(class)
library(FNN)

memory.limit(size=8048)
#funciton to get memory sizes of the biggest objects
getsizes <- function() {z <- sapply(ls(envir=globalenv()),
                                    function(x) object.size(get(x)))
(tmp <- as.matrix(rev(sort(z))[1:10]))}



train_file = read.csv('train.csv', header=T)
test_file  = read.csv('test.csv', header=T)
#soln_file  = read.csv('user_median.csv', header=T)
profiles_file = read.csv('profiles.csv', header=T)
artists_file = read.csv('artists.csv', header=T)


havepro = merge( train_file,  profiles_file, by='user', all=F)

df = havepro
df$sex = (as.numeric(df$sex))
df$country = (as.numeric(df$country))
df$user = (as.numeric(df$user))
df$artist = (as.numeric(df$artist))


haveprotest = merge( test_file,  profiles_file, by='user', all=F)

dftest = haveprotest
dftest$sex = (as.numeric(dftest$sex))
dftest$country = (as.numeric(dftest$country))
dftest$user = (as.numeric(dftest$user))
dftest$artist = (as.numeric(dftest$artist))


numtest= dftest[complete.cases(dftest),] [1:100,] 
numtrain =  df[complete.cases(df),] [1:100,] 


#kk = knn.reg(comptrain,comptest,comptrain$plays, k=20)


#nn = neuralnet(plays ~ user+artist + sex + age + country, df, hidden=20)

comptrain = havepro[complete.cases(havepro),] [1:100,]  
comptest  =haveprotest[complete.cases(haveprotest),] [1:100,]  
  
#try knn - this works on smaller datasets
f = as.formula(plays ~ user+artist + sex + age + country)
kn = kknn::kknn(f, train=numtrain, test=numtest)

#try on bigger set

numtest2= dftest[complete.cases(dftest),] [1:1000,] 
numtrain2 =  df[complete.cases(df),] [1:1000,] 

kn2 = kknn::kknn(f, train=numtrain2, test=numtest2)


#try on just training set, so we can compare results

numfktrain3= df[complete.cases(df),] [1001:2000,] 
numtrain3 =  df[complete.cases(df),] [1:1000,] 

kn3 = kknn::kknn(f, train=numtrain3, test=numfktrain3)

hist(kn3$fitted.values - numfktrain3$plays, xlim=c(-5000,5000), title="KNN Prediction error ")







#try kknn:kknn 

f = as.formula(plays ~ user+artist + sex + age + country)
kn = kknn::kknn(f, train=comptrain, test=comptest)



