#Predicting Success of Bank Telemarketing using CRISP-DM process
#Author: Kedar Kulkarni

#random sampling#
dat = read.csv('DC.csv', stringsAsFactors=T, head=T)
dat0 = dat[which(dat$y_yes == 0), ] #no of 0s i.e. no outcome values in y
dat1 = dat[which(dat$y_yes == 1), ]#no of 1s i.e. yes outcome values in y
set.seed(1)
ind = sample(1:nrow(dat0), nrow(dat1)) #taking sample by referring total no of 1s
dat00 = dat0[ind, ]#seperating the equal no of 0s with 1s
datnew = rbind.data.frame(dat00, dat1)#combining separated 0s and original no of 1s
write.csv(datnew, file='datanew.csv', row.names=F)

#### Data partition 
rm(list=ls());gc()
setwd('/Users/Owner/Desktop/Fall 2017/ISDS 574 Data Mining/project/')
dat = read.csv('SampleData1.0.csv', head=T, stringsAsFactors=F, na.strings='')

##1. Take 60% of data randomly as training##
set.seed(1) 
id.train = sample(1:nrow(dat), nrow(dat)*.6) # ncol() gives number of columns
id.test = setdiff(1:nrow(dat), id.train) # setdiff gives the set difference

##2. Prepare data for XLMiner that have same partition##
ind_XLMiner = rep(NA, nrow(dat))
ind_XLMiner[id.train] = 'T'
ind_XLMiner[id.test] = 'V'

dat1 = cbind(dat, ind_XLMiner)
write.csv(dat1, file = 'SampleData2.0.csv', row.names=F, na='')
