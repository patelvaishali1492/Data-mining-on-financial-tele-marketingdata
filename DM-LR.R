#Predicting Success of Bank Telemarketing using CRISP-DM process
#Author: Kedar Kulkarni

#Logistic Regression (LR)
rm(list=ls());gc()
setwd('/Users/Owner/Desktop/Fall 2017/ISDS 574 Data Mining/project/')
dat = read.csv('SampleData2.0.csv', head=T, stringsAsFactors=F, na.strings='')

##1. Building min.model and max.model##
dat.train = dat[id.train,]
dat.test = dat[id.test,]
min.model = glm(y_yes ~ 1, data = dat.train, family = 'binomial')
max.model = glm(y_yes ~ ., data = dat.train, family = 'binomial')
max.formula = formula(max.model)

##2. Building function for classification task##
dichotomize = function(x, cutoff=.5) {
  out = rep(0, length(x))
  out[x > cutoff] = 1
  out
}

##3. Forward selection##
objf = step(min.model, scope=list(lower=min.model, upper=max.model), direction='forward')
summary(objf)
yhatf = predict(objf, newdata = dat.test, type='response')
hist(yhatf)

yhatf.class1 = dichotomize(yhatf, .5)
sum(yhatf.class1 != dat.test$y_yes)/length(id.test) ##misclassification error rate for cutoff 0.5

yhatf.class2 = dichotomize1(yhatf, .25)
sum(yhatf.class2 != dat.test$y_yes)/length(id.test) ##misclassification error rate for cutoff 0.25

#Obtain confusion matrix, error rate, sensitivity, specitivity#
install.packages('caret')
require(caret)
confusionMatrix(yhatf.class2, dat.test$y_yes, positive= "1") ##confusion matrix

#Obtain ROC Curve#
install.packages('pROC')
require(pROC)
roc(dat.test$y_yes, yhatf)
plot.roc(dat.test$y_yes, yhatf)

##4. Backward elimination#
objb = step(max.model, scope=list(lower=min.model, upper=max.model), direction='backward')
summary(objb)

yhatb = predict(objb, newdata = dat.test, type='response')
hist(yhatb)

yhatb.class = dichotomize(yhatb, .25)
sum(yhatb.class != dat.test$y_yes)/length(id.test) ##misclassification error rate for cutoff 0.25

#Obtain confusion matrix, error rate, sensitivity, specitivity#
confusionMatrix(yhatb.class, dat.test$y_yes, positive= "1") ##confusion matrix

#Obtain ROC Curve#
roc(dat.test$y_yes, yhatb)
plot.roc(dat.test$y_yes, yhatb)

##5. Stepwise selection##
objsw = step(min.model, scope=list(lower=min.model, upper=max.model), direction='both')
summary(objsw)

yhatsw = predict(objsw, newdata = dat.test, type='response')
hist(yhatsw)

yhatsw.class = dichotomize(yhatsw, .25)
sum(yhatsw.class != dat.test$y_yes)/length(id.test) ##misclassification error rate for cutoff 0.25

#Obtain confusion matrix, error rate, sensitivity, specitivity#
confusionMatrix(yhatsw.class, dat.test$y_yes, positive= "1") ##confusion matrix

#Obtain ROC Curve#
roc(dat.test$y_yes, yhatsw)
plot.roc(dat.test$y_yes, yhatsw)
