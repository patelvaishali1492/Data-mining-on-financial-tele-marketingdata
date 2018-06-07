#Predicting Success of Bank Telemarketing using CRISP-DM process
#Author: Kedar Kulkarni

#k-Nearest-Neighbors (kNN)

# remove all the variables stored in the environment
rm(list = ls())gc()
# Set the location
setwd('/Users/Owner/Desktop/Fall 2017/ISDS 574/Final Project')
# Read the file
dat = read.csv('SampleData1.csv', header = TRUE, stringsAsFactors = TRUE)

# normalize the data except the outcome variable (dat[1]) and 
# the partition indicator variable (dat[28])
normaldata = as.data.frame(scale(dat[2:27], center=TRUE, scale=TRUE))
# combine the normalized data with outcome variable and partition indicator
finnormaldata = as.data.frame(c(dat[1], normaldata, dat[28]))
# use the indicator variable to build the train data
train = finnormaldata[finnormaldata$ind_XLMiner=='T',]
# remove the indicator variable from the train data
train = subset(train, select = -c(ind_XLMiner))
# specify the outcome variable in y.train
y.train = train[,1]
# use the indicator variable to build the test data
test = finnormaldata[finnormaldata$ind_XLMiner=='V',]

# remove the indicator variable from the test data
test = subset(test, select = -c(ind_XLMiner))

# specify the outcome variable in y.test
y.test = test[,1]

require(class)
require(caret)

# build a dich function that will take in a probability vector and 
# use a cutoff to build the outcome as 1 if probability is above 
# the specified cutoff
dich = function(x, ct = .5) {
  out = rep(0, length(x))
  out[x > ct] = 1
  out
}
# build a function to find the best k that produces the least error
knn.bestK = function(train, test, y.train, y.test, k.max = 20, ctoff = 0.5) {
  k.grid = seq(1, k.max) 
  error = rep(NA, length(k.grid))
  fun.tmp = function(x) {
    y.prob = attributes(knn(train, test, y.train, y.test, k = x,prob=T))$prob
    y.hat = dich(y.prob, ctoff)
    return(sum(y.hat != y.test))
  }
  ## create a temporary function (fun.tmp) that we want to apply to each value in k.grid
  error = unlist(lapply(k.grid, fun.tmp)) / length(y.test)
  out = list(k.optimal = k.grid[which.min(error)], error.min = min(error))
  return(out)
}
# find out the best k by using knn.bestK function
knn.bestK(train[2:27], test[2:27], y.train, y.test, 20, 0.25)
