# Assignment #4
# Allan Bisnar 17385089
set.seed(340)
library(tm)
library(cvTools)
library(SnowballC)
library(e1071)

#==== Helper Functions for Preprocessing============

preProcess <- function(corpus){
  #removeXML <- function(x) gsub("<.*>", "", x)
  removeAuthor <- function(x) gsub("(F|f)rom:.*", "", x)
  tmp <- tm_map(corpus, content_transformer(PlainTextDocument))
  tmp <- tm_map(tmp, content_transformer(removeAuthor))
  tmp <- tm_map(tmp, content_transformer(removeWords), c(stopwords('english')))
  tmp <- tm_map(tmp, content_transformer(stripWhitespace))
  tmp <- tm_map(tmp, content_transformer(tolower))
  tmp <- tm_map(tmp, content_transformer(removePunctuation))
  tmp <- tm_map(tmp, content_transformer(removeNumbers))
  return(tmp)
}

convertToDTM <- function(corpus){
  tmp2 <- DocumentTermMatrix(corpus,control=list(stemming = TRUE, weighting = weightTfIdf, wordLengths=c(5,Inf)))
  return(tmp2)
}

dataFrame <- function(x){
  tmp3 <- as.data.frame(inspect( x ))
  return(tmp3)
}

#====================================================

setwd("E://Users/Allan/DropBox/assgn4/")
#setwd("/home//abisnar/Dropbox/assgn4/")
#load the Data Set

train.corpus.comp1 <-VCorpus(DirSource("20news-bydate-train/comp.graphics/", encoding="UTF-8"))
train.corpus.comp2 <-VCorpus(DirSource("20news-bydate-train/comp.os.ms-windows.misc/", encoding="UTF-8"))
train.corpus.comp3 <-VCorpus(DirSource("20news-bydate-train/comp.sys.ibm.pc.hardware/", encoding="UTF-8"))
train.corpus.comp4 <-VCorpus(DirSource("20news-bydate-train/comp.sys.mac.hardware/", encoding="UTF-8"))
train.corpus.comp5 <-VCorpus(DirSource("20news-bydate-train/comp.windows.x/", encoding="UTF-8"))

train.corpus.talk1 <-VCorpus(DirSource("20news-bydate-train/talk.politics.guns/", encoding="UTF-8"))
train.corpus.talk2 <-VCorpus(DirSource("20news-bydate-train/talk.politics.mideast/", encoding="UTF-8"))
train.corpus.talk3 <-VCorpus(DirSource("20news-bydate-train/talk.politics.misc/", encoding="UTF-8"))
train.corpus.talk4 <-VCorpus(DirSource("20news-bydate-train/talk.religion.misc/", encoding="UTF-8"))

#==================================================================================================

test.corpus.comp1 <-VCorpus(DirSource("20news-bydate-test/comp.graphics/", encoding="UTF-8"))
test.corpus.comp2 <-VCorpus(DirSource("20news-bydate-test/comp.os.ms-windows.misc/", encoding="UTF-8"))
test.corpus.comp3 <-VCorpus(DirSource("20news-bydate-test/comp.sys.ibm.pc.hardware/", encoding="UTF-8"))
test.corpus.comp4 <-VCorpus(DirSource("20news-bydate-test/comp.sys.mac.hardware/", encoding="UTF-8"))
test.corpus.comp5 <-VCorpus(DirSource("20news-bydate-test/comp.windows.x/", encoding="UTF-8"))

test.corpus.talk1 <-VCorpus(DirSource("20news-bydate-test/talk.politics.guns/", encoding="UTF-8"))
test.corpus.talk2 <-VCorpus(DirSource("20news-bydate-test/talk.politics.mideast/", encoding="UTF-8"))
test.corpus.talk3 <-VCorpus(DirSource("20news-bydate-test/talk.politics.misc/", encoding="UTF-8"))
test.corpus.talk4 <-VCorpus(DirSource("20news-bydate-test/talk.religion.misc/", encoding="UTF-8"))

#=========Combining Each Sources into  Individual Corpuses=================================

#total unprocessed corpus from comp
train.corpus.comp <- c(train.corpus.comp1,train.corpus.comp2,train.corpus.comp3,train.corpus.comp4,train.corpus.comp5)
total_train_class.1 <- length(train.corpus.comp)

train.corpus.talk <- c(train.corpus.talk1,train.corpus.talk2,train.corpus.talk3,train.corpus.talk4)
total_train_class.0 <- length(train.corpus.talk)

test.corpus.comp <- c(test.corpus.comp1,test.corpus.comp2,test.corpus.comp3,test.corpus.comp4,test.corpus.comp5)
total_test_class.1 <- length(test.corpus.comp)

test.corpus.talk <- c(test.corpus.talk1,test.corpus.talk2,test.corpus.talk3,test.corpus.talk4)
total_test.class.0 <- length(test.corpus.talk)

train.rows <- total_train_class.0 + total_train_class.1
test.rows <- total_test.class.0 + total_test_class.1

#====================== Preprocess each Corpus ==================
train.corpus.comp <- preProcess(train.corpus.comp)
train.corpus.talk <- preProcess(train.corpus.talk)
test.corpus.comp <- preProcess(test.corpus.comp)
test.corpus.talk <- preProcess(test.corpus.talk)

#====Convert each preprocessed Corpus into DTMs at least 5 chars long =========
train.dtm.comp <- convertToDTM(train.corpus.comp)
train.dtm.talk <- convertToDTM(train.corpus.talk)
test.dtm.comp <- convertToDTM(test.corpus.comp)
test.dtm.talk <- convertToDTM(test.corpus.talk)

#========= Create Classes Vector =======================

#order : train.comp, train.talk, test.comp, test.talk
classes <- c(rep(1,total_train_class.1),rep(0,total_train_class.0),rep(1,total_test_class.1),rep(0,total_test.class.0))

train_test_dtm <- c(train.dtm.comp, train.dtm.talk, test.dtm.comp,test.dtm.talk, recursive = TRUE)
train_test_dtm <- removeSparseTerms(train_test_dtm, sparse=0.96)

train_test_df <- dataFrame(train_test_dtm)

#=====================SVM Analysis ===========================================================================
train_withoutclass <- head(train_test_df,train.rows)
test_withoutclass <- tail(train_test_df,test.rows)
train_classes <- factor(head (classes,train.rows))
test_classes <- factor(tail(classes,test.rows))

SVM <- svm(train_withoutclass,train_classes,kernel="linear")
summary(SVM)
PredictionSVM <- predict(SVM,test_withoutclass)

c_matrix <- table(PredictionSVM,test_classes)
#       test_classes
# PrSVM    0    1
# 0      1157  172
# 1       144 1783

spec <- c_matrix[1]/(c_matrix[1] + c_matrix[3])
# > spec
# [1] 0.8705794
sens <- c_matrix[4]/(c_matrix[2] + c_matrix[4])
# > sens
# [1] 0.9252724
accuracy <- sum(diag(c_matrix))/sum(c_matrix)
# > accuracy
# [1] 0.9029484

prop.table(table(test_classes==PredictionSVM))
# FALSE      TRUE 
# 0.0970516 0.9029484 
# > summary(SVM)


#===========Part 2: 5-Fold CV=======================================
#===================================================================
#===================================================================

#Create stratified DFrame

train_test.wclass <- cbind(train_test_df,classes)

#divide comp into 5 categories of approximately equal size
train_test.comp <- subset(train_test.wclass,train_test.wclass$classes == 1)

index.comp <- floor((total_train_class.1 + total_test_class.1)/5)

comp.fold1 <- train_test.comp[1:979,]
comp.fold2 <- train_test.comp[980:1957,]
comp.fold3 <- train_test.comp[1958:2935,]
comp.fold4 <- train_test.comp[2936:3913,]
comp.fold5 <- train_test.comp[3914:4891,]


#divide talk into 5 categories of approx. equal size
train_test.talk <- subset(train_test.wclass,train_test.wclass$classes == 0)
index.talk <- floor((total_train_class.0 + total_test.class.0)/5)

talk.fold1 <- train_test.talk[1:651,]
talk.fold2 <- train_test.talk[652:1302,]
talk.fold3 <- train_test.talk[1303:1953,]
talk.fold4 <- train_test.talk[1954:2603,]
talk.fold5 <- train_test.talk[2604:3253,]

#=======Create the 5 stratified folds ================================================================

cv_fold1 <- rbind(comp.fold1, talk.fold1)
cv_fold2 <- rbind(comp.fold2, talk.fold2)
cv_fold3 <- rbind(comp.fold3, talk.fold3)
cv_fold4 <- rbind(comp.fold4, talk.fold4)
cv_fold5 <- rbind(comp.fold5, talk.fold5)

#======Combine into a single DataFrame=========================================================
cv.df <-rbind(cv_fold1,cv_fold2,cv_fold3,cv_fold4, cv_fold5)
# get rid of classes column
cv.df.noclasses <- subset(cv.df, select =-c(classes))
cv.df.classes <- c(cv.df$classes)

#------CV # 1 using fold 1, 2, 3, 4 as Training, test on Fold 5-------------------------
#-------------------------------------------------------------------------------------
train.cv1.rows = nrow(cv.df) - nrow(cv_fold5)
test.cv1.rows = nrow(cv_fold5)

train.cv1.noclass <- head(cv.df.noclasses,train.cv1.rows)
test.cv1.noclass <- tail(cv.df.noclasses,test.cv1.rows)


train.cv1.class <- factor(head (cv.df.classes,train.cv1.rows))
test.cv1.class <- factor(tail(cv.df.classes,test.cv1.rows))

#-------------------SVM ---------------------------------------------------

SVM.cv1 <- svm(train.cv1.noclass,train.cv1.class,kernel="linear")
summary(SVM.cv1)


PredSVM.cv1 <- predict(SVM.cv1, test.cv1.noclass)


c_matrix.cv1 <- table(PredSVM.cv1,test.cv1.class)
# > c_matrix.cv1
# test.cv1.class
# PredSVM.cv1   0   1
# 0 580  83
# 1  70 895

spec.cv1 <- c_matrix.cv1[1]/(c_matrix.cv1[1] + c_matrix.cv1[3])
# [1] 0.8748115

sens.cv1 <- c_matrix.cv1[4]/(c_matrix.cv1[2] + c_matrix.cv1[4])
# [1] 0.9274611

acc.cv1 <- sum(diag(c_matrix.cv1))/sum(c_matrix.cv1)
# [1] 0.9060197

prop.table(table(test.cv1.class==PredSVM.cv1))
# > prop.table(table(test.cv1.class==PredSVM.cv1))
# 
# FALSE       TRUE 
# 0.09398034 0.90601966

#------CV # 2 using fold 1, 2, 3, 5 as Training, test on Fold 4-------------------------
#---------------------------------------------------------------------------------------
cv.df <-rbind(cv_fold1,cv_fold2,cv_fold3,cv_fold5, cv_fold4)
# get rid of classes column
cv.df.noclasses <- subset(cv.df, select =-c(classes))
cv.df.classes <- c(cv.df$classes)

train.cv2.rows = nrow(cv.df) - nrow(cv_fold4)
test.cv2.rows = nrow(cv_fold4)

train.cv2.noclass <- head(cv.df.noclasses,train.cv2.rows)
test.cv2.noclass <- tail(cv.df.noclasses,test.cv2.rows)

train.cv2.class <- factor(head (cv.df.classes,train.cv2.rows))
test.cv2.class <- factor(tail(cv.df.classes,test.cv2.rows))

#---------------------SVM ----------------------------------------------------------

SVM.cv2 <- svm(train.cv2.noclass,train.cv2.class,kernel="linear")
summary(SVM.cv2)

PredSVM.cv2 <- predict(SVM.cv2, test.cv2.noclass)


c_matrix.cv2 <- table(PredSVM.cv2,test.cv2.class)
#         test.cv2.class
# PredSVM.cv2   0   1
# 0 582  75
# 1  68 903

spec.cv2 <- c_matrix.cv2[1]/(c_matrix.cv2[1] + c_matrix.cv2[3])
# [1] 0.8858447

sens.cv2 <- c_matrix.cv2[4]/(c_matrix.cv2[2] + c_matrix.cv2[4])
# [1] 0.9299691

acc.cv2 <- sum(diag(c_matrix.cv2))/sum(c_matrix.cv2)
# [1] 0.9121622

prop.table(table(test.cv2.class==PredSVM.cv2))
# > prop.table(table(test.cv1.class==PredSVM.cv2))
# 
# FALSE       TRUE 
# 0.08783784 0.91216216

#------CV # 3 using fold 1, 2, 4, 5 as Training, test on Fold 3-------------------------
#---------------------------------------------------------------------------------------
cv.df <-rbind(cv_fold1,cv_fold2,cv_fold4, cv_fold5,cv_fold3)
# get rid of classes column
cv.df.noclasses <- subset(cv.df, select =-c(classes))
cv.df.classes <- c(cv.df$classes)

train.cv3.rows = nrow(cv.df) - nrow(cv_fold3)
test.cv3.rows = nrow(cv_fold3)

train.cv3.noclass <- head(cv.df.noclasses,train.cv3.rows)
test.cv3.noclass <- tail(cv.df.noclasses,test.cv3.rows)

train.cv3.class <- factor(head (cv.df.classes,train.cv3.rows))
test.cv3.class <- factor(tail(cv.df.classes,test.cv3.rows))

#---------------------SVM ----------------------------------------------------------

SVM.cv3 <- svm(train.cv3.noclass,train.cv3.class,kernel="linear")
summary(SVM.cv3)

PredSVM.cv3 <- predict(SVM.cv3, test.cv3.noclass)

c_matrix.cv3 <- table(PredSVM.cv3,test.cv3.class)
#           test.cv3.class
# PredSVM.cv3   0   1
# 0 590 106
# 1  61 872

spec.cv3 <- c_matrix.cv3[1]/(c_matrix.cv3[1] + c_matrix.cv3[3])
# [1] 0.8477011

sens.cv3 <- c_matrix.cv3[4]/(c_matrix.cv3[2] + c_matrix.cv3[4])
# [1] 0.9346195

acc.cv3 <- sum(diag(c_matrix.cv3))/sum(c_matrix.cv3)
# [1] 0.8974831

prop.table(table(test.cv3.class==PredSVM.cv3))
# > prop.table(table(test.cv3.class==PredSVM.cv3))
# 
# FALSE      TRUE 
# 0.1025169 0.8974831 

#------CV # 4 using fold 1, 3, 4, 5 as Training, test on Fold 2-------------------------
#---------------------------------------------------------------------------------------
cv.df <-rbind(cv_fold1,cv_fold3,cv_fold4, cv_fold5,cv_fold2)
# get rid of classes column
cv.df.noclasses <- subset(cv.df, select =-c(classes))
cv.df.classes <- c(cv.df$classes)

train.cv4.rows = nrow(cv.df) - nrow(cv_fold2)
test.cv4.rows = nrow(cv_fold2)

train.cv4.noclass <- head(cv.df.noclasses,train.cv4.rows)
test.cv4.noclass <- tail(cv.df.noclasses,test.cv4.rows)

train.cv4.class <- factor(head (cv.df.classes,train.cv4.rows))
test.cv4.class <- factor(tail(cv.df.classes,test.cv4.rows))

#---------------------SVM ----------------------------------------------------------

SVM.cv4 <- svm(train.cv4.noclass,train.cv4.class,kernel="linear")
summary(SVM.cv4)

PredSVM.cv4 <- predict(SVM.cv4, test.cv4.noclass)

c_matrix.cv4 <- table(PredSVM.cv4,test.cv4.class)
#         test.cv4.class
# PredSVM.cv4   0   1
# 0           581  94
# 1           70 884

spec.cv4 <- c_matrix.cv4[1]/(c_matrix.cv4[1] + c_matrix.cv4[3])
# [1] 0.8607407

sens.cv4 <- c_matrix.cv4[4]/(c_matrix.cv4[2] + c_matrix.cv4[4])
# [1] 0.9266247

acc.cv4 <- sum(diag(c_matrix.cv4))/sum(c_matrix.cv4)
# [1] 0.8993247

prop.table(table(test.cv4.class==PredSVM.cv4))
# > prop.table(table(test.cv4.class==PredSVM.cv4))
# 
# FALSE      TRUE 
# 0.1006753 0.8993247 

#------CV # 5 using fold 2, 3, 4, 5 as Training, test on Fold 1-------------------------
#---------------------------------------------------------------------------------------
cv.df <-rbind(cv_fold2,cv_fold3,cv_fold4, cv_fold5,cv_fold1)
# get rid of classes column
cv.df.noclasses <- subset(cv.df, select =-c(classes))
cv.df.classes <- c(cv.df$classes)

train.cv5.rows = nrow(cv.df) - nrow(cv_fold1)
test.cv5.rows = nrow(cv_fold1)

train.cv5.noclass <- head(cv.df.noclasses,train.cv5.rows)
test.cv5.noclass <- tail(cv.df.noclasses,test.cv5.rows)

train.cv5.class <- factor(head (cv.df.classes,train.cv5.rows))
test.cv5.class <- factor(tail(cv.df.classes,test.cv5.rows))

#---------------------SVM ----------------------------------------------------------

SVM.cv5 <- svm(train.cv5.noclass,train.cv5.class,kernel="linear")
summary(SVM.cv5)

PredSVM.cv5 <- predict(SVM.cv5, test.cv5.noclass)

c_matrix.cv5 <- table(PredSVM.cv5,test.cv5.class)
#         test.cv5.class
# PredSVM.cv5   0   1
# 0           587  89
# 1           64 890

spec.cv5 <- c_matrix.cv5[1]/(c_matrix.cv5[1] + c_matrix.cv5[3])
# [1] 0.8683432

sens.cv5 <- c_matrix.cv5[4]/(c_matrix.cv5[2] + c_matrix.cv5[4])
# [1] 0.932914

acc.cv5 <- sum(diag(c_matrix.cv5))/sum(c_matrix.cv5)
# [1] 0.906135

prop.table(table(test.cv5.class==PredSVM.cv5))
# > prop.table(table(test.cv5.class==PredSVM.cv5))
# 
# FALSE       TRUE 
# 0.09386503 0.90613497  

#=============5 FOLD CV SVM Results==========================
# Sum all the confusion matrices to determine the 5 fold cv
cv.results <- as.table(matrix(0,nrow=2,ncol=2,dimnames=list(c("pred.0","pred.1"),c("true.0","true.1"))))
cv.results[1] <- c_matrix.cv1[1] + c_matrix.cv2[1] + c_matrix.cv3[1] + c_matrix.cv4[1] + c_matrix.cv5[1]
cv.results[2] <- c_matrix.cv1[2] + c_matrix.cv2[2] + c_matrix.cv3[2] + c_matrix.cv4[2] + c_matrix.cv5[2]
cv.results[3] <- c_matrix.cv1[3] + c_matrix.cv2[3] + c_matrix.cv3[3] + c_matrix.cv4[3] + c_matrix.cv5[3]
cv.results[4] <- c_matrix.cv1[4] + c_matrix.cv2[4] + c_matrix.cv3[4] + c_matrix.cv4[4] + c_matrix.cv5[4]

cv.spec.results <- cv.results[1]/(cv.results[1] + cv.results[3])
# [1] 0.8672409

cv.sens.results <- cv.results[4]/(cv.results[2] + cv.results[4])
# [1] 0.930291

cv.acc.results <- sum(diag(cv.results))/sum(cv.results)
# [1] 0.904224