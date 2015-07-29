# Assignment #2
# Allan Bisnar
# 17385089

library(rpart)

#================================ Part 1: Load Full Training Data ==========================================
census.training <- read.csv("2014CensusTraining.csv",sep=",",header=T)
summary(census.training)

# Load full training data with 5 parameters
F5<- rpart(class ~ age + workclass +  fnlwgt + education + education.num, census.training)
print(F5)

# Load full training data with 10 parameters
F10<- rpart(class ~ age + workclass +  fnlwgt + education + education.num + marital.status + occupation + relationship + race + sex, census.training)
print(F10)

# Load full training data with all 14 parameters
F14<- rpart(class ~., census.training)
print(F14)

#=========================Part 2 : Load Half Training Data Set ============================
census.half <- read.csv("2014HalfCensusTraining.csv",sep=",",header=T)
summary(census.half)

# Load half traning data with 5 parameters
H5<- rpart(class ~ age + workclass +  fnlwgt + education + education.num, census.half)
print(H5)

# Load half training data with 10 parameters
H10<- rpart(class ~ age + workclass +  fnlwgt + education + education.num + marital.status + occupation + relationship + race + sex, census.half)
print(H10)

# Load half training data with all 14 parameters
H14<- rpart(class ~., census.half)
print(H14)

#=========================== Part 3: Load Census Test Data==================================
census.test <- read.csv("2014NewCensusTest.csv", sep=",", header=T)

# Predicting based on full training data with 5 parameters
prediction <- predict(F5, census.test,type="class")
f5.results <- table (census.test$class,prediction)

# Predicting based on full training with 10 parameters
prediction <- predict(F10, census.test,type="class")
f10.results <- table (census.test$class,prediction)

# Prediction based on full training with all parameters
prediction <- predict(F14, census.test,type="class")
f14.results <- table (census.test$class,prediction)

#==============Prediction Results based on half training======

# Prediction based on half training with 5 parameters
prediction <- predict(H5, census.test,type="class")
h5.results<- table (census.test$class,prediction)

# Prediction based on half training with 10 parameters
prediction <- predict(H10, census.test,type="class")
h10.results<- table (census.test$class,prediction)

# Prediction based on half training with 14 parameters
prediction <- predict(H14, census.test,type="class")
h14.results<- table (census.test$class,prediction)

# === Calculating Sensitivity, Specificity and Accuracy =====
# Sensitivity = TP / (TP + FN)
# Specificity = TN / (TN + FP)
# Accuracy = (TP + TN) / (TN + TP + FN + FP)

## Comparing 5 parameters

#---- Looking at full training------------

f5.sensitivity <- f5.results[1]/(f5.results[1]+f5.results[3])
f5.specificity <- f5.results[4]/(f5.results[4]+f5.results[2])
f5.accuracy <- (f5.results[1]+f5.results[4])/(sum(f5.results))

#------Compare with half training------------
h5.sensitivity <- h5.results[1]/(h5.results[1]+h5.results[3])
h5.specificity <- h5.results[4]/(h5.results[4]+h5.results[2])
h5.accuracy <- (h5.results[1]+h5.results[4])/(sum(h5.results))

## Compare with 10 parameters

#---- Looking at full training------------

f10.sensitivity <- f10.results[1]/(f10.results[1]+f10.results[3])
f10.specificity <- f10.results[4]/(f10.results[4]+f10.results[2])
f10.accuracy <- (f10.results[1]+f10.results[4])/(sum(f10.results))

#------Compare with half training------------
h10.sensitivity <- h10.results[1]/(h10.results[1]+h10.results[3])
h10.specificity <- h10.results[4]/(h10.results[4]+h10.results[2])
h10.accuracy <- (h10.results[1]+h10.results[4])/(sum(h10.results))

## Compare with 14 parameters

#---- Looking at full training------------

f14.sensitivity <- f14.results[1]/(f14.results[1]+f14.results[3])
f14.specificity <- f14.results[4]/(f14.results[4]+f14.results[2])
f14.accuracy <- (f14.results[1]+f14.results[4])/(sum(f14.results))

#------Compare with half training------------
h14.sensitivity <- h14.results[1]/(h14.results[1]+h14.results[3])
h14.specificity <- h14.results[4]/(h14.results[4]+h14.results[2])
h14.accuracy <- (h14.results[1]+h14.results[4])/(sum(h14.results))

#================================================================
# Put results into vectors
f5.analysis <- c(f5.sensitivity,f5.specificity,f5.accuracy)
f10.analysis <- c(f10.sensitivity,f10.specificity,f10.accuracy)
f14.analysis <- c(f14.sensitivity,f14.specificity,f14.accuracy)

h5.analysis <- c(h5.sensitivity,h5.specificity,h5.accuracy)
h10.analysis <- c(h10.sensitivity,h10.specificity,h10.accuracy)
h14.analysis <- c(h14.sensitivity,h14.specificity,h14.accuracy)

sens.results <- c(f5.sensitivity, h5.sensitivity, f10.sensitivity,h10.sensitivity, f14.sensitivity, h14.sensitivity)
spec.results <- c(f5.specificity,h5.specificity, f10.specificity,h10.specificity,f14.specificity,h14.specificity)
acc.results <- c(f5.accuracy, h5.accuracy, f10.accuracy, h10.accuracy,f14.accuracy,h14.accuracy)


analysis.results <- cbind(f5.analysis,f10.analysis,f14.analysis,h5.analysis,h10.analysis,h14.analysis)
titles<- c("Sensitivity","Specificity","Accuracy")
analysis.results <-cbind(titles,analysis.results)
analysis.results

print(t(analysis.results))

