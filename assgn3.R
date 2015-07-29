# Assignment #3
# Allan Bisnar
# 17385089
setwd("/home/abisnar/Documents//CPSC340/assignments//assgn3")

#---- Load the data ---------
train_missing <- read.csv("Assign3trainMissingValues.csv")
train_missing # inspect data frame for missing values

#-- Remove rows from train_missing that are missing
testData <- read.csv("Assign3Test.csv")

#remove rows with missing data
train_modified <- na.omit(train_missing)

# Training ommiting missing data rows
MClean <-glm(default10yr ~ income + age + loan, data = train_modified, family="binomial")

#Prediction on Test
predict1 <- predict(MClean, newdata = testData, type = "response")

#performance helper function to determine sensitivity and specificity
perf = function(cut, mod, y)
{
  yhat = (mod$fit>cut)
  w = which(y==1)
  sensitivity = mean( yhat[w] == 1 ) 
  specificity = mean( yhat[-w] == 0 ) 
  out = t(as.matrix(c(sensitivity, specificity)))
  colnames(out) = c("sensitivity", "specificity")
  return(out)
}

sensitivity=c()
specifity=c()

for(i in seq(0.15, 0.2, by=0.01)){
  sensitivity <- c(sensitivity,perf(i,MClean,train_modified$default10yr)[1])
  specifity <- c(specifity,perf(i,MClean,train_modified$default10yr)[2])
}

#find the cutoff
plot(seq(0.15, 0.2, by=0.01), sensitivity , type="l", lty=1, col="blue", xlab="cutoff points", ylab="sensitivity/specifity", lwd=2)
lines(seq(0.15, 0.2, by=0.01),specifity, type="l", lty=1, col="red", lwd=2)

#looks like optimal cutoff point is 0.192 from graph
cutoff_pt.clean <- 0.192


#prediction
predict1 <- ifelse(predict(MClean, newdata=testData,type="response")> cutoff_pt.clean, 1, 0)
confusionmatrix.clean <- table(testData$default10yr,predict1)
#accuracy of model over the test data:
MClean.acc <-sum(diag(confusionmatrix.clean))/sum(confusionmatrix.clean)

#sensitivity
clean_train.specificity <- confusionmatrix.clean[1]/(confusionmatrix.clean[1]+confusionmatrix.clean[3])
#specificity
clean_train.sensitivity <- confusionmatrix.clean[4]/(confusionmatrix.clean[4]+confusionmatrix.clean[2])

# ================= Part 2 ====================================

#Using subsets of the original data for linear regression
train_missing.train <- subset(train_missing, !is.na(train_missing$loan))
train_missing.test <- subset(train_missing, is.na(train_missing$loan))

# Load in the true loan values
true_values <- read.table("Assign3TrueValues.csv", sep=",", header=T)

#-----------------RIncome -----------------------------------------

#Linear regression model using income as the predictor
RIncome = lm(loan~income,data=train_missing.train)
RIncome
summary(RIncome)

#Predict loan values from income
income.predicted.loan <-
as.data.frame(predict(RIncome,interval="prediction",newdata=train_missing.test))

#Imputed loans with linear regression test dataset, and combine with filled values
imputed.loan.income <- as.data.frame(cbind(train_missing.test ,
imputed_loan=c(income.predicted.loan$fit)))

#Compute the squared error distance of expected from actual
Rincome.err <- sum((imputed.loan.income$imputed_loan - true_values$loan)^2)


#---------- RAge ------------------------------------------------------------------

#Linear regression model using age as the predictor
RAge = lm(loan~age,data=train_missing.train)
RAge
summary(RAge)

#Predict loan values from age
age.predicted.loan <-
  as.data.frame(predict(RAge,interval="prediction",newdata=train_missing.test))

#Imputed loans with linear regression test dataset, and combine with filled values
imputed.loan.age <- as.data.frame(cbind(train_missing.test ,
                                           imputed_loan=c(age.predicted.loan$fit)))

#Compute the squared error distance of expected from actual
RAge.err <-sum((imputed.loan.age$imputed_loan - true_values$loan)^2)

#---------------Combination of Both --------------------------------------------
#Linear regression model using both as the predictor
RBoth = lm(loan~age + income,data=train_missing.train)
RBoth
summary(RBoth)

#Predict loan values from both
both.predicted.loan <-
  as.data.frame(predict(RBoth,interval="prediction",newdata=train_missing.test))

#Imputed loans with linear regression test dataset, and combine with filled values
imputed.loan.both <- as.data.frame(cbind(train_missing.test ,
                                        imputed_loan=c(both.predicted.loan$fit)))

#Compute the squared error distance of expected from actual
RBoth.err <- sum((imputed.loan.both$imputed_loan - true_values$loan)^2)

#================Part 3 ========================================
#----------------------MIncome -----------------------------------

#Combine linear regression test and traning dataset into new traning
#set for logistic regression
train_imputed.income <- as.data.frame(
  rbind(
    cbind(train_missing.train, imputed_loan=c(train_missing.train$loan)),
    imputed.loan.income
  )
)
names(train_imputed.income)[4] <- "loan_old"
names(train_imputed.income)[6] <- "loan"

# Training ommiting missing data rows
MIncome <-glm(default10yr ~ income + age + loan, data = train_imputed.income, family="binomial")

sensitivity=c()
specifity=c()

for(i in seq(0.15, 0.2, by=0.01)){
  sensitivity <- c(sensitivity,perf(i,MIncome,train_imputed.income$default10yr)[1])
  specifity <- c(specifity,perf(i,MIncome,train_imputed.income$default10yr)[2])
}

#find the cutoff
plot(seq(0.15, 0.2, by=0.01), sensitivity , type="l", lty=1, col="blue", xlab="cutoff points", ylab="sensitivity/specifity", lwd=2)
lines(seq(0.15, 0.2, by=0.01),specifity, type="l", lty=1, col="red", lwd=2)

#Looks like cutoff is 0.177 (Intersection)
cutoff_pt.income <- 0.177

#prediction
predict.MIncome <- ifelse(predict(MIncome, newdata=testData,type="response")> cutoff_pt.income, 1, 0)
confusionmatrix.income <- table(testData$default10yr,predict.MIncome)
#accuracy of model over the test data:
MIncome.acc <- sum(diag(confusionmatrix.income))/sum(confusionmatrix.income)

#sensitivity
MIncome_train.specificity <- confusionmatrix.income[1]/(confusionmatrix.income[1]+confusionmatrix.income[3])
#specificity
MIncome_train.sensitivity <- confusionmatrix.income[4]/(confusionmatrix.income[4]+confusionmatrix.income[2])

#----------------------MAge------------------------------------------------------

#Combine linear regression test and traning dataset into new traning
#set for logistic regression
train_imputed.age <- as.data.frame(
  rbind(
    cbind(train_missing.train, imputed_loan=c(train_missing.train$loan)),
    imputed.loan.age
  )
)
names(train_imputed.age)[4] <- "loan_old"
names(train_imputed.age)[6] <- "loan"

# Training ommiting missing data rows
MAge <-glm(default10yr ~ income + age + loan, data = train_imputed.age, family="binomial")

#Prediction on Test
predict.MAge <- predict(MAge, newdata = testData, type = "response")

sensitivity=c()
specifity=c()

for(i in seq(0.15, 0.2, by=0.01)){
  sensitivity <- c(sensitivity,perf(i,MAge,train_imputed.age$default10yr)[1])
  specifity <- c(specifity,perf(i,MAge,train_imputed.age$default10yr)[2])
}

#find the cutoff
plot(seq(0.15, 0.2, by=0.01), sensitivity , type="l", lty=1, col="blue", xlab="cutoff points", ylab="sensitivity/specifity", lwd=2)
lines(seq(0.15, 0.2, by=0.01),specifity, type="l", lty=1, col="red", lwd=2)

#looks like intersection is at 0.172
cutoff_pt.age <- 0.172

#prediction
predict.MAge <- ifelse(predict(MAge, newdata=testData,type="response")> cutoff_pt.age, 1, 0)
confusionmatrix.age <- table(testData$default10yr,predict.MAge)
#accuracy of model over the test data:
MAge.acc <- sum(diag(confusionmatrix.age))/sum(confusionmatrix.age)

#sensitivity
MAge_train.specificity <- confusionmatrix.age[1]/(confusionmatrix.age[1]+confusionmatrix.age[3])
#specificity
MAge_train.sensitivity <- confusionmatrix.age[4]/(confusionmatrix.age[4]+confusionmatrix.age[2])


#----------------------MBoth------------------------------------------------------

#Combine linear regression test and traning dataset into new traning
#set for logistic regression
train_imputed.both <- as.data.frame(
  rbind(
    cbind(train_missing.train, imputed_loan=c(train_missing.train$loan)),imputed.loan.both)
)
names(train_imputed.both)[4] <- "loan_old"
names(train_imputed.both)[6] <- "loan"


# Training ommiting missing data rows
MBoth <-glm(default10yr ~ income + age + loan, data = train_imputed.both, family="binomial")

#Prediction on Test
predict.MBoth <- predict(MBoth, newdata = testData, type = "response")

sensitivity=c()
specifity=c()

for(i in seq(0.15, 0.2, by=0.01)){
  sensitivity <- c(sensitivity,perf(i,MBoth,train_imputed.both$default10yr)[1])
  specifity <- c(specifity,perf(i,MBoth,train_imputed.both$default10yr)[2])
}

#find the cutoff
plot(seq(0.15, 0.2, by=0.01), sensitivity , type="l", lty=1, col="blue", xlab="cutoff points", ylab="sensitivity/specifity", lwd=2)
lines(seq(0.15, 0.2, by=0.01),specifity, type="l", lty=1, col="red", lwd=2)

#cutoff pt looks like .175 from graph
cutoff_pt.both <- 0.175

#prediction
predict.MBoth <- ifelse(predict(MBoth, newdata=testData,type="response")> cutoff_pt.both, 1, 0)
confusionmatrix.both <- table(testData$default10yr,predict.MBoth)
#accuracy of model over the test data:
MBoth.acc <- sum(diag(confusionmatrix.both))/sum(confusionmatrix.both)

#sensitivity
MBoth_train.specificity <- confusionmatrix.both[1]/(confusionmatrix.both[1]+confusionmatrix.both[3])
#specificity
MBoth_train.sensitivity <- confusionmatrix.age[4]/(confusionmatrix.both[4]+confusionmatrix.both[2])

#================Results =======================================

#Sq Error Results of Imputation
sq_errs <-c(Rincome.err,RAge.err,RBoth.err)
sq_names <- c("RIncome","RAge","RBoth")
sq_errs <- cbind(sq_names,sq_errs)

#Results From Prediction Based on Training Manipulations
results<- matrix(c(
  MClean.acc,clean_train.sensitivity,clean_train.specificity,cutoff_pt.clean,
  MIncome.acc,MIncome_train.sensitivity,MIncome_train.specificity,cutoff_pt.income,
  MAge.acc,MAge_train.sensitivity,MAge_train.specificity,cutoff_pt.age,
  MBoth.acc,MBoth_train.sensitivity,MBoth_train.specificity,cutoff_pt.both),
  ncol=4,byrow=TRUE)

rownames(results) <- c("MClean","MIncome","MAge","MBoth")
colnames(results) <- c("Accuracy","Sensitivity","Specificity","CutoffVal")

print(sq_errs)
print(results)
