# Step 1: Collecting data

credit <- read.csv("/Users/Albert/Desktop/credit_card.csv")
credit$default.payment.next.month <- as.factor(credit$default.payment.next.month)

# remove ID
credit <- credit[,-1]

# recode dummy variable SEX (SEX==1 if male, 0 if female)
credit$SEX[credit$SEX==2] <- 0


# Remove the observations with unknown meanings
credit<- credit[credit$EDUCATION != 0,]
credit<- credit[credit$EDUCATION != 5,]
credit<- credit[credit$EDUCATION != 6,]
credit<- credit[credit$MARRIAGE != 0,]
credit<- credit[credit$PAY_0 != -2,]
credit<- credit[credit$PAY_2 != -2,]
credit<- credit[credit$PAY_3 != -2,]
credit<- credit[credit$PAY_4 != -2,]
credit<- credit[credit$PAY_5 != -2,]
credit<- credit[credit$PAY_6 != -2,]

# Create a new variable: Balance = Bill - Pay
credit$BALANCE1 <- credit$BILL_AMT1 - credit$PAY_AMT1
credit$BALANCE2 <- credit$BILL_AMT2 - credit$PAY_AMT2
credit$BALANCE3 <- credit$BILL_AMT3 - credit$PAY_AMT3
credit$BALANCE4 <- credit$BILL_AMT4 - credit$PAY_AMT4
credit$BALANCE5 <- credit$BILL_AMT5 - credit$PAY_AMT5
credit$BALANCE6 <- credit$BILL_AMT6 - credit$PAY_AMT6
credit$BALANCE_SUM <- credit$BALANCE1 + credit$BALANCE2 + credit$BALANCE3 + credit$BALANCE4 + credit$BALANCE5 + credit$BALANCE6
credit$BALANCE_cleared[credit$BALANCE_SUM <= 0] <- 1
credit$BALANCE_cleared[credit$BALANCE_SUM > 0] <- 0
credit$BALANCE_AVG <- credit$BALANCE_SUM/6
credit$UTILIZATION_RATE <- credit$BALANCE_AVG/credit$LIMIT_BAL

summary(as.factor(credit$default.payment.next.month))
prop.table(table(as.factor(credit$default.payment.next.month)))
library(stats)
fit <- glm(default.payment.next.month ~ UTILIZATION_RATE, data=credit, family = "binomial")
summary(fit)

table(credit$BALANCE_cleared, credit$default.payment.next.month)
prop.table(table(credit$BALANCE_cleared, credit$default.payment.next.month),1)

table(credit$default.payment.next.month)
prop.table(table(credit$default.payment.next.month))

table(credit$BALANCE_cleared)
prop.table(table(credit$BALANCE_cleared))

# KNN Classifier

library(nnet)
credit1 <- credit
EDUCATION1 <- class.ind(credit1$EDUCATION)
head(EDUCATION1)
credit1$EDUCATION_1 <- EDUCATION1[,1]
credit1$EDUCATION_2 <- EDUCATION1[,2]
credit1$EDUCATION_3 <- EDUCATION1[,3]
credit1$EDUCATION_4 <- EDUCATION1[,4]


MARRIAGE <- class.ind(credit1$MARRIAGE)
head(MARRIAGE)
credit1$MARRIAGE_1<- MARRIAGE[,1]
credit1$MARRIAGE_2<- MARRIAGE[,2]
credit1$MARRIAGE_3<- MARRIAGE[,3]

credit1$MARRIAGE <- NULL
credit1$EDUCATION <- NULL

set.seed(123)
library(caret)
train_rows <- createDataPartition(credit1$default.payment.next.month, times=1, p=0.8, list=F)
train <- credit1[train_rows,]
test <- credit1[-train_rows,]

train_labels <- train[,22]
test_labels <- test[,22]

library(class)
pred_labels <- knn(train=train,test=test, cl=train_labels, k = 155)
table(test_labels,pred_labels)
mean(test_labels==pred_labels)

pred_labels1 <- knn(train=train,test=test, cl=train_labels, k = 193)
table(test_labels,pred_labels1)
mean(test_labels==pred_labels1)

pred_labels2 <- knn(train=train,test=test, cl=train_labels, k = 100)
table(test_labels,pred_labels2)
mean(test_labels==pred_labels2)

#Normalize and Scaling 
normalize <- function(x) {
  return ((x - min(x)) / (max(x) - min(x)))
}

credit_n <- credit1
credit_n$AGE <- normalize(credit_n$AGE)
credit_n$BILL_AMT1 <- normalize(credit_n$BILL_AMT1)
credit_n$BILL_AMT2 <- normalize(credit_n$BILL_AMT2)
credit_n$BILL_AMT3 <- normalize(credit_n$BILL_AMT3)
credit_n$BILL_AMT4 <- normalize(credit_n$BILL_AMT4)
credit_n$BILL_AMT5 <- normalize(credit_n$BILL_AMT5)
credit_n$BILL_AMT6 <- normalize(credit_n$BILL_AMT6)
credit_n$PAY_AMT1 <- normalize(credit_n$PAY_AMT1)
credit_n$PAY_AMT2 <- normalize(credit_n$PAY_AMT2)
credit_n$PAY_AMT3 <- normalize(credit_n$PAY_AMT3)
credit_n$PAY_AMT4 <- normalize(credit_n$PAY_AMT4)
credit_n$PAY_AMT5 <- normalize(credit_n$PAY_AMT5)
credit_n$PAY_AMT6 <- normalize(credit_n$PAY_AMT6)
credit_n$BALANCE1 <- normalize(credit_n$BALANCE1)
credit_n$BALANCE2 <- normalize(credit_n$BALANCE2)
credit_n$BALANCE3 <- normalize(credit_n$BALANCE3)
credit_n$BALANCE4 <- normalize(credit_n$BALANCE4)
credit_n$BALANCE5 <- normalize(credit_n$BALANCE5)
credit_n$BALANCE6 <- normalize(credit_n$BALANCE6)
credit_n$BALANCE_SUM <- normalize(credit_n$BALANCE_SUM)
credit_n$BALANCE_AVG <- normalize(credit_n$BALANCE_AVG)
credit_n$UTILIZATION_RATE <- normalize(credit_n$UTILIZATION_RATE)

train_n <- credit_n[train_rows,]
test_n <- credit_n[-train_rows,]

train_n_labels <- train_n[,22]
test_n_labels <- test_n[,22]

pred_labels_n <- knn(train=train_n,test=test_n, cl=train_n_labels, k = 155)
table(test_labels,pred_labels_n)
mean(test_labels==pred_labels_n)

pred_labels_n1 <- knn(train=train_n,test=test_n, cl=train_n_labels, k = 2)
table(test_labels,pred_labels_n1)
mean(test_labels==pred_labels_n1)

library(pROC)
knn.prob.pred <- knn(train,test,cl=train_labels,prob=T,k=155)
knn.prob.pred1 <- knn(train,test,cl=train_labels,prob=T,k=193)
pr <- attr(knn.prob.pred,"prob")
pr1 <- attr(knn.prob.pred1,"prob")
knn.ROC <- roc(test_labels,ifelse(pred_labels==1,pr,1-pr),
               levels = c(0,1))
knn.ROC1 <- roc(test_labels,ifelse(pred_labels1==1,pr,1-pr),
               levels = c(0,1))
plot(knn.ROC, legacy.axes = TRUE, main = "ROC curve of kNN classifier")
lines(knn.ROC1,col="red")

knn.prob.pred_n <- knn(train_n,test_n,cl=train_n_labels,prob=T,k=155)
knn.prob.pred_n1 <- knn(train_n,test_n,cl=train_n_labels,prob=T,k=2)
pr_n <- attr(knn.prob.pred_n,"prob")
pr_n1 <- attr(knn.prob.pred_n1,"prob")
knn.ROC_n <- roc(test_labels,ifelse(pred_labels==1,pr_n,1-pr_n),
               levels = c(0,1))
knn.ROC_n1 <- roc(test_labels,ifelse(pred_labels1==1,pr_n1,1-pr_n1),
                levels = c(0,1))
plot(knn.ROC_n, legacy.axes = TRUE, main = "ROC curve of kNN classifier")
lines(knn.ROC,col="red")
lines(knn.ROC_n1,col="blue")

# Naive Bayes
credit2 <- credit1
credit2$SEX <- as.factor(credit2$SEX)
credit2$EDUCATION_1 <- as.factor(credit2$EDUCATION_1)
credit2$EDUCATION_2 <- as.factor(credit2$EDUCATION_2)
credit2$EDUCATION_3 <- as.factor(credit2$EDUCATION_3)
credit2$EDUCATION_4 <- as.factor(credit2$EDUCATION_4)
credit2$MARRIAGE_1 <- as.factor(credit2$MARRIAGE_1)
credit2$MARRIAGE_2 <- as.factor(credit2$MARRIAGE_2)
credit2$MARRIAGE_3 <- as.factor(credit2$MARRIAGE_3)
credit2$PAY_0 <- as.factor(credit2$PAY_0)
credit2$PAY_2 <- as.factor(credit2$PAY_2)
credit2$PAY_3 <- as.factor(credit2$PAY_3)
credit2$PAY_4 <- as.factor(credit2$PAY_4)
credit2$PAY_5 <- as.factor(credit2$PAY_5)
credit2$PAY_6 <- as.factor(credit2$PAY_6)
credit2$BALANCE_cleared <- as.factor(credit2$BALANCE_cleared)


prop.table(table(credit1$default.payment.next.month,credit$EDUCATION),2)
barplot(prop.table(table(credit1$default.payment.next.month,credit$EDUCATION),2))
chisq.test(credit$default.payment.next.month,credit$EDUCATION)
chisq.test(credit$default.payment.next.month,credit$EDUCATION)$observed
# for actual cell counts
chisq.test(credit$default.payment.next.month,credit$EDUCATION)$expected
# for cell counts expected by chance
chisq.test(credit$default.payment.next.month,credit$EDUCATION)$residuals
# for Pearson residuals (z scores)

prop.table(table(credit1$default.payment.next.month,credit$MARRIAGE),2)
barplot(prop.table(table(credit1$default.payment.next.month,credit$MARRIAGE),2))

cor.test(credit$LIMIT_BAL,credit$UTILIZATION_RATE)

prop.table(table(credit1$default.payment.next.month,credit$MARRIAGE),2)
barplot(prop.table(table(credit1$default.payment.next.month,credit$EDUCATION),1))library(e1071)
train.X <- credit2[train_rows,-22]
test.X <- credit2[-train_rows,-22]

train.default.payment <- credit2$default.payment.next.month[train_rows]
test.default.payment <- credit2$default.payment.next.month[-train_rows]


nb.class <- naiveBayes(train.X, train.default.payment)
nb.pred <- predict(nb.class, test.X)

table(nb.pred, test.default.payment)
mean(nb.pred==test.default.payment)

# Laplace Smoothing
nb.class1 <- naiveBayes(train.X, train.default.payment,laplace=1)
nb.pred1 <- predict(nb.class1, test.X)

table(nb.pred1, test.default.payment)
mean(nb.pred1==test.default.payment)

nb.class2 <- naiveBayes(train.X, train.default.payment,laplace=500)
nb.pred2 <- predict(nb.class2, test.X)

table(nb.pred2, test.default.payment)
mean(nb.pred2==test.default.payment)

nb.class3 <- naiveBayes(train.X, train.default.payment,laplace=1000)
nb.pred3 <- predict(nb.class3, test.X)

table(nb.pred3, test.default.payment)
mean(nb.pred3==test.default.payment)


# nb.ROC <- roc(test$Insp, nb.prob.pred[,2], levels=c("fraud","ok","unkn"))
nb.prob.pred <- predict(nb.class,test,"raw")
head(nb.prob.pred)
nb.prob.pred1 <- predict(nb.class1,test,"raw")
head(nb.prob.pred1)
nb.prob.pred2 <- predict(nb.class2,test,"raw")
head(nb.prob.pred2)
head(test.default.payment)
nb.prob.pred3 <- predict(nb.class3,test,"raw")

nb.ROC <- roc(test.default.payment, nb.prob.pred[,2], levels=c(0,1))
nb.ROC1 <- roc(test.default.payment, nb.prob.pred1[,2], levels=c(0,1))
nb.ROC2 <- roc(test.default.payment, nb.prob.pred2[,2], levels=c(0,1))
nb.ROC3 <- roc(test.default.payment, nb.prob.pred3[,2], levels=c(0,1))
plot(nb.ROC, legacy.axes = TRUE, main="ROC curve of Naive Bayes with different laplace smoothing")
lines(nb.ROC1,col="red")
lines(nb.ROC2,col="blue")
lines(nb.ROC3,col="yellow")


## Classification Tree
library(rpart)
credit.tree <- rpart(default.payment.next.month ~., data=train)

library(rpart.plot)
library(RColorBrewer)
rpart.plot(credit.tree)
printcp(credit.tree)
plotcp(credit.tree)
credit.tree.pred <- predict(credit.tree, test, type="class")
table(credit.tree.pred, test[,22])
mean(credit.tree.pred==test[,22])

table(credit$PAY_0,credit$default.payment.next.month)
prop.table(table(credit$PAY_0,credit$default.payment.next.month))

#Pruning: find the CP with minimal xerror and use it to prune the tree
prune_cp <- credit.tree$cptable[which.min(credit.tree$cptable[,"xerror"]),"CP"]
credit.tree.pruned <- prune(credit.tree,cp=prune_cp)
rpart.plot(credit.tree.pruned)
printcp(credit.tree.pruned)
plotcp(credit.tree.pruned)
credit.tree.pruned.pred <- predict(credit.tree.pruned,test,type = "class")
table(credit.tree.pruned.pred,test[,22])
mean(credit.tree.pruned.pred==test[,22])

tree.prob.pred <- predict(credit.tree,test)
tree.prob.pruned.pred <- predict(credit.tree.pruned,test)
head(tree.prob.pred)
tree.ROC <- roc(test$default.payment.next.month, tree.prob.pred[,2],
                levels = (levels(test$default.payment.next.month)))
tree.ROC.pruned <- roc(test$default.payment.next.month, tree.prob.pruned.pred[,2],
                levels = (levels(test$default.payment.next.month)))
plot(tree.ROC, legacy.axes=TRUE, main="ROC curve of classification tree")
lines(tree.ROC.pruned,col="red")

# Random Forest
library(randomForest)
rf.credit <- randomForest(default.payment.next.month~., data=train, mtry= 5 , importance=T)
varImpPlot(rf.credit)
rf.credit1 <- randomForest(default.payment.next.month~., data=train, mtry= 14 , importance=T)
varImpPlot(rf.credit1)
rf.credit2 <- randomForest(default.payment.next.month~., data=train, ntree = 1000, mtry= 5 , importance=T)
varImpPlot(rf.credit2)
rf.credit3 <- randomForest(default.payment.next.month~., data=train, ntree = 1000, mtry= 14 , importance=T)
varImpPlot(rf.credit3)

rf.pred <- predict(rf.credit,test)
rf.pred1 <- predict(rf.credit1,test)
rf.pred2 <- predict(rf.credit2,test)
rf.pred3 <- predict(rf.credit3,test)
table(rf.pred,test$default.payment.next.month)
mean(rf.pred==test$default.payment.next.month)
mean(rf.pred1==test$default.payment.next.month)
mean(rf.pred2==test$default.payment.next.month)
mean(rf.pred3==test$default.payment.next.month)

rf.pred.prob <- predict(rf.credit,test, type = "prob")
rf.pred.prob1 <- predict(rf.credit1,test, type = "prob")
rf.pred.prob2 <- predict(rf.credit2,test, type = "prob")
rf.pred.prob3 <- predict(rf.credit3,test, type = "prob")
rf.ROC <- roc(test$default.payment.next.month,rf.pred.prob[,2], levels = (levels(test$default.payment.next.month)))
rf.ROC1 <- roc(test$default.payment.next.month,rf.pred.prob1[,2], levels = (levels(test$default.payment.next.month)))
rf.ROC2 <- roc(test$default.payment.next.month,rf.pred.prob2[,2], levels = (levels(test$default.payment.next.month)))
rf.ROC3 <- roc(test$default.payment.next.month,rf.pred.prob3[,2], levels = (levels(test$default.payment.next.month)))

plot(rf.ROC, legacy.axes=TRUE, main="ROC curve of Random Forest")
lines(rf.ROC1,col="red")
lines(rf.ROC2,col="blue")
lines(rf.ROC3,col="yellow")

# Logistic Regression

fit <- glm(default.payment.next.month ~ LIMIT_BAL+SEX+EDUCATION+MARRIAGE+AGE+BALANCE_AVG+UTILIZATION_RATE, data=credit, family = "binomial")
summary(fit)
fit1 <- glm(default.payment.next.month ~ LIMIT_BAL+SEX+EDUCATION_2+EDUCATION_3+EDUCATION_4+MARRIAGE_2+MARRIAGE_3+AGE+BALANCE_AVG+UTILIZATION_RATE, data=credit2, family = "binomial")
summary(fit1)
fit2 <- glm(default.payment.next.month ~ ., data=credit2, family = "binomial")
summary(fit2)

credit$PAY_0 <- NULL
credit$PAY_2 <- NULL
credit$PAY_3 <- NULL
credit$PAY_4 <- NULL
credit$PAY_5 <- NULL
credit$PAY_6 <- NULL

str(credit)
