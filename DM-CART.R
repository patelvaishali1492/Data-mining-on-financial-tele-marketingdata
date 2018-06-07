#Predicting Success of Bank Telemarketing using CRISP-DM process
#Author: Kedar Kulkarni

#Classification Trees (CART)
list=ls(); gc()
library(rpart)

dat = read.csv('SampleData1.0.csv', stringsAsFactors=T, head=T)
colnames(dat) 
head(dat) 

# Classification Tree with rpart
# grow tree
fit = rpart(y_yes ~ ., method="class", data=dat, cp = 1e-2, minsplit=21)  

printcp(fit) # display the results
plotcp(fit) # visualize cross-validation results
summary(fit) # detailed summary of splits

# plot tree
plot(fit, uniform=T, main="Classification Tree for Loan Acceptance data")
text(fit, use.n=T, all=TRUE, cex=.8)

# prune the tree
pfit = prune(fit, cp = fit$cptable[which.min(fit$cptable[,"xerror"]),"CP"])

# plot the pruned tree
plot(pfit, uniform=T, main="Pruned Classification Tree for Loan Acceptance Data")
text(pfit, use.n=T, all=T, cex=.8)
