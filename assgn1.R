### ====================== CS 340 - Assignment #1 ========================
## Allan Bisnar
## 17385089


setwd("/home/abisnar/Documents/CPSC340/r/LabSep15newsgroup/")
library(tm)
library(rpart)
library(proxy)

corpus.newsgroup <- VCorpus(DirSource("Assign1newsgroup/", encoding="UTF-8", recursive=TRUE))

#cleanup data
corpus.newsgroup <- tm_map(corpus.newsgroup, content_transformer(tolower))
myStopwords <- c(stopwords('english')) # removed stopwords before punctuation
corpus.newsgroup <- tm_map(corpus.newsgroup, content_transformer(removeWords), myStopwords)
corpus.newsgroup <- tm_map(corpus.newsgroup, content_transformer(removePunctuation))
corpus.newsgroup <- tm_map(corpus.newsgroup, content_transformer(removeNumbers))
corpus.newsgroup <- tm_map(corpus.newsgroup, content_transformer(stripWhitespace))
removeURL <- function(x) gsub("http[[:alnum:]]*", "", x)
corpus.newsgroup <- tm_map(corpus.newsgroup, content_transformer(removeURL))

# create term frequency vector for each document with word 
dtm1 <- DocumentTermMatrix(corpus.newsgroup, control=list(wordLengths=c(1,Inf)))

#removing sparse terms
dtm2 <- removeSparseTerms(dtm1, sparse=0.96)

#conversion to matrix:
m2 <- as.matrix(dtm2)

# Setting Seed to reproduce results
set.seed(340)

distmatrix <- dist(scale(m2))
hclustResults <- hclust(distmatrix,method="ward.D")

#====================================================================================

#starting plotting through with k=2
plot(hclustResults,cex=0.5,hang=-1,main="Hclust dendrogram of Newsgroups")
rect.hclust(hclustResults,k=2)
cluster.k2 <- cutree(hclustResults, k=2)

table.k2 <- table(cluster.k2)

#convert to table
table.hardware <- table(cluster.k2[1:100])
table.os <- table(cluster.k2[101:200])
table.sci <- table(cluster.k2[201:300])

#convert counts of documents within clusters to vectors
vector.count <- as.vector(table.k2)
vector.hardware <- as.vector(table.hardware)
vector.os <- as.vector(table.os)
vector.sci <- as.vector(table.sci)

##calculate probabilities of each document within each cluster
prob1.hardware <- (vector.hardware[1]/vector.count [1]) # prob of cluster
prob2.hardware <- (vector.hardware[2]/vector.count [2])

prob1.os <- (vector.os[1]/vector.count [1])
prob2.os <- (vector.os[2]/vector.count [2])

prob1.sci <- (vector.sci[1]/vector.count [1])
prob2.sci <- (vector.sci[2]/vector.count [2])

pt1.entropy <- -1 * (prob1.hardware * (log2(prob1.hardware)) + prob1.os * (log2(prob1.os)) + prob1.sci*(log2(prob1.sci)))
pt2.entropy <- -1 * (prob2.hardware * (log2(prob2.hardware)) + prob2.os * (log2(prob2.os)) + prob2.sci*(log2(prob2.sci)))

## length(corpus.newsgroup) = 300
entropy.sum <- (vector.count[1]/300)*pt1.entropy + (vector.count[2]/300)*pt2.entropy

k2.entropy <-entropy.sum
print(k2.entropy)

#============= Repeat for k = 3 ===========================================

plot(hclustResults,cex=0.5,hang=-1,main="Hclust dendrogram of Newsgroups")
rect.hclust(hclustResults,k=3)
cluster.k3 <-cutree(hclustResults, k=3)

table.k3 <- table(cluster.k3)

#convert to table
table.hardware <- table(cluster.k3[1:100])
table.os <- table(cluster.k3[101:200])
table.sci <- table(cluster.k3[201:300])

#convert to vector
vector.count <- as.vector(table.k3)
vector.hardware <- as.vector(table.hardware)
vector.os <- as.vector(table.os) 
vector.sci <- as.vector(table.sci)

#calculate probabilities of each documet type in each cluster
prob1.hardware <- (vector.hardware[1]/vector.count[1])
prob2.hardware <- (vector.hardware[2]/vector.count[2])
# no hardware docs in cluster 3

prob1.os <- (vector.os[1]/vector.count[1])
prob2.os <- (vector.os[2]/vector.count[2])
# no os docs in cluster 3

prob1.sci <- (vector.sci[1]/vector.count[1])
prob2.sci <- (vector.sci[2]/vector.count[2])
prob3.sci <- (vector.sci[3]/vector.count[3])

#calculate entropies
pt1.entropy <- -1 * (prob1.hardware * (log2(prob1.hardware)) + prob1.os * (log2(prob1.os)) + prob1.sci*(log2(prob1.sci)))
pt2.entropy <- -1 * (prob2.hardware * (log2(prob2.hardware)) + prob2.os * (log2(prob2.os)) + prob2.sci*(log2(prob2.sci)))
pt3.entropy <- -1 * (prob3.sci * log2(prob3.sci))

## length(corpus.newsgroup) = 300
entropy.sum <- (vector.count[1]/300)*pt1.entropy + (vector.count[2]/300)*pt2.entropy + (vector.count[3]/300)*pt3.entropy

k3.entropy <- entropy.sum
print(k3.entropy)

#============= Repeat for k = 4 ===========================================

plot(hclustResults,cex=0.5,hang=-1,main="Hclust dendrogram of Newsgroups")
rect.hclust(hclustResults,k=4)
cluster.k4 <-cutree(hclustResults, k=4)

table.k4 <- table(cluster.k4)

#convert to table
table.hardware <- table(cluster.k4[1:100])
table.os <- table(cluster.k4[101:200])
table.sci <- table(cluster.k4[201:300])

#convert to vector
vector.count <- as.vector(table.k4)
vector.hardware <- as.vector(table.hardware)
vector.os <- as.vector(table.os) 
vector.sci <- as.vector(table.sci)

#calculate the probabilities of each document type in each cluster
prob1.hardware <- (vector.hardware[1]/vector.count[1])
prob2.hardware <- (vector.hardware[2]/vector.count[2])
#no hardware docs in cluster 3
#no hardware docs in cluster 4

prob1.os <- (vector.os[1]/vector.count[1])
prob2.os <- (vector.os[2]/vector.count[2])
#no os docs in cluster 3
#no os docs in cluster 4

prob1.sci <- (vector.sci[1]/vector.count[1])
prob2.sci <- (vector.sci[2]/vector.count[2])
prob3.sci <- (vector.sci[3]/vector.count[3])
prob4.sci <- (vector.sci[4]/vector.count[4])

#calculate entropies
pt1.entropy <- -1 * (prob1.hardware * (log2(prob1.hardware)) + prob1.os * (log2(prob1.os)) + prob1.sci*(log2(prob1.sci)))
pt2.entropy <- -1 * (prob2.hardware * (log2(prob2.hardware)) + prob2.os * (log2(prob2.os)) + prob2.sci*(log2(prob2.sci)))
pt3.entropy <- -1 * (prob3.sci*(log2(prob3.sci)))
pt4.entropy <- -1 * (prob4.sci*(log2(prob4.sci)))

## length(corpus.newsgroup) = 300
entropy.sum <- (vector.count[1]/300)*pt1.entropy + (vector.count[2]/300)*pt2.entropy + 
  (vector.count[3]/300)*pt3.entropy + (vector.count[4]/300)*pt4.entropy

k4.entropy <- entropy.sum
print(k4.entropy)

#=============================== Repeat for k = 5 =======================================================


plot(hclustResults,cex=0.5,hang=-1,main="Hclust dendrogram of Newsgroups")
rect.hclust(hclustResults,k=5)
cluster.k5 <-cutree(hclustResults, k=5)

table.k5 <- table(cluster.k5)

#convert to table
table.hardware <- table(cluster.k5 [1:100])
table.os <- table(cluster.k5 [101:200])
table.sci <- table(cluster.k5 [201:300])

#convert to vector
vector.count <- as.vector(table.k5)
vector.hardware <- as.vector(table.hardware)
vector.os <- as.vector(table.os)
vector.sci <- as.vector(table.sci)

#calculate probabilities of each doctype in each cluster
prob1.hardware <- (vector.hardware[1]/vector.count[1])
prob2.hardware <- (vector.hardware[2]/vector.count[2])
prob3.hardware <- (vector.hardware[3]/vector.count[3])
## There are no hardware docs in clusters 4 and 5

prob1.os <- (vector.os[1]/vector.count[1])
prob2.os <- (vector.os[2]/vector.count[2])
prob3.os <- (vector.os[3]/vector.count[3])
## There are no os docs in clusters 4 and 5

prob1.sci <- (vector.sci[1]/vector.count[1])
prob2.sci <- (vector.sci[2]/vector.count[2])
prob3.sci <- (vector.sci[3]/vector.count[3])
prob4.sci <- (vector.sci[4]/vector.count[4])
prob5.sci <- (vector.sci[5]/vector.count[5])

# calculate entropy
pt1.entropy <- -1 * (prob1.hardware * (log2(prob1.hardware)) + prob1.os * (log2(prob1.os)) + prob1.sci*(log2(prob1.sci)))
pt2.entropy <- -1 * (prob2.hardware * (log2(prob2.hardware)) + prob2.os * (log2(prob2.os)) + prob2.sci*(log2(prob2.sci)))
pt3.entropy <- -1 * (prob3.hardware * (log2(prob3.hardware)) + prob3.os * (log2(prob3.os)) + prob3.sci*(log2(prob3.sci)))
pt4.entropy <- -1 * (prob4.sci*(log2(prob4.sci)))
pt5.entropy <- -1 * (prob5.sci*(log2(prob5.sci)))

## length(corpus.newsgroup) = 300
entropy.sum <- (vector.count[1]/300)*pt1.entropy + (vector.count[2]/300)*pt2.entropy + (vector.count[3]/300)*pt3.entropy + 
  (vector.count[4]/300)*pt4.entropy + (vector.count[5]/300)*pt5.entropy

k5.entropy <-entropy.sum
print(k5.entropy)

##============================== Repeat for k = 6 ==============================================================


plot(hclustResults,cex=0.5,hang=-1,main="Hclust dendrogram of NewsGroup")
rect.hclust(hclustResults,k=6)
cluster.k6 <-cutree(hclustResults, k=6)

table.k6 <- table(cluster.k6)

#convert to table
table.hardware <- table(cluster.k6[1:100])
table.os <- table(cluster.k6[101:200])
table.sci <- table(cluster.k6[201:300])

#convert to vector
vector.count <- as.vector(table.k6)
vector.hardware <- as.vector(table.hardware) ## no hardware in clusters 4, 5 , 6
vector.os <- as.vector(table.os) ## no os in 5 and 6
vector.sci <- as.vector(table.sci)

prob1.hardware <- (vector.hardware[1]/vector.count[1])
prob2.hardware <- (vector.hardware[2]/vector.count[2])
prob3.hardware <- (vector.hardware[3]/vector.count[3])
prob4.hardware <- (vector.hardware[4]/vector.count[4])
## no hardware in clusters 5 , 6

prob1.os <- (vector.os[1]/vector.count[1])
prob2.os <- (vector.os[2]/vector.count[2])
prob3.os <- (vector.os[3]/vector.count[3])
prob4.os <- (vector.os[4]/vector.count[4])
## no os in 5 and 6

prob1.sci <- (vector.sci[1]/vector.count[1])
prob2.sci <- (vector.sci[2]/vector.count[2])
prob3.sci <- (vector.sci[3]/vector.count[3])
prob4.sci <- (vector.sci[4]/vector.count[4])
prob5.sci <- (vector.sci[5]/vector.count[5])
prob6.sci <- (vector.sci[6]/vector.count[6])

pt1.entropy <- -1 * (prob1.hardware * (log2(prob1.hardware)) + prob1.os * (log2(prob1.os)) + prob1.sci*(log2(prob1.sci)))
pt2.entropy <- -1 * (prob2.hardware * (log2(prob2.hardware)) + prob2.os * (log2(prob2.os)) + prob2.sci*(log2(prob2.sci)))
pt3.entropy <- -1 * (prob3.hardware * (log2(prob3.hardware)) + prob3.os * (log2(prob3.os)) + prob3.sci*(log2(prob3.sci)))
pt4.entropy <- -1 * (prob4.hardware * (log2(prob4.hardware)) + prob4.os * (log2(prob4.os)) + prob4.sci*(log2(prob4.sci)))
pt5.entropy <- -1 * (prob5.sci*(log2(prob5.sci)))
pt6.entropy <- -1 * (prob6.sci*(log2(prob6.sci)))

## length(corpus.newsgroup) = 300
entropy.sum <- (vector.count[1]/300)*pt1.entropy + (vector.count[2]/300)*pt2.entropy + (vector.count[3]/300)*pt3.entropy + 
  (vector.count[4]/300)*pt4.entropy + (vector.count[5]/300)*pt5.entropy + (vector.count[6]/300)*pt6.entropy

k6.entropy <-entropy.sum
print(k6.entropy)

q1.entropies= c(k2.entropy,k3.entropy,k4.entropy,k5.entropy,k6.entropy)
print("The entropy results without TF-IDF are")
print(q1.entropies)


# With removing stop words before punctuation
#> print(q1.entropies)
#[1] 1.554638 1.548276 1.548276 1.546074 1.525987

#===========Question 4: Extending Feature to TF-IDF =========================================
#============================================================================================
#============================================================================================

#========================Processing ====================================
dtm.tfidf <- DocumentTermMatrix(corpus.newsgroup, control=list(wordLengths=c(1,Inf), weighting = weightTfIdf))

#removing sparse terms
dtm.tfidf.clean <- removeSparseTerms(dtm.tfidf, sparse=0.96)

#conversion to matrix:
m3 <- as.matrix(dtm.tfidf.clean)

# Setting Seed to reproduce results
set.seed(340)

distmatrix <- dist(scale(m3))
hclustResults <- hclust(distmatrix,method="ward.D")

#----------------- Start with k = 2 ----------------------------------------------

plot(hclustResults,cex=0.5,hang=-1,main="Hclust dendrogram of NewsGroups")
rect.hclust(hclustResults,k=2)
cluster.k2.tfidf <- cutree(hclustResults, k=2)

table.k2.tfidf <- table(cluster.k2.tfidf)

#convert to table
table.hardware <- table(cluster.k2.tfidf[1:100])
table.os <- table(cluster.k2.tfidf[101:200])
table.sci <- table(cluster.k2.tfidf[201:300])

#convert to vector
vector.count <- as.vector(table.k2.tfidf)
vector.hardware <- as.vector(table.hardware)
vector.os <- as.vector(table.os)
vector.sci <- as.vector(table.sci)

#calculate probabilites of each document type in each cluster
prob1.hardware <- (vector.hardware[1]/vector.count[1])
prob2.hardware <- (vector.hardware[2]/vector.count[2])

prob1.os <- (vector.os[1]/vector.count[1])
prob2.os <- (vector.os[2]/vector.count[2])

prob1.sci <- (vector.sci[1]/vector.count[1])
prob2.sci <- (vector.sci[2]/vector.count[2])

#calculate entropies for each cluster
pt1.entropy <- -1 * (prob1.hardware * (log2(prob1.hardware)) + prob1.os * (log2(prob1.os)) + prob1.sci*(log2(prob1.sci)))
pt2.entropy <- -1 * (prob2.hardware * (log2(prob2.hardware)) + prob2.os * (log2(prob2.os)) + prob2.sci*(log2(prob2.sci)))

entropy.sum <- (vector.count[1]/300)*pt1.entropy + (vector.count[2]/300)*pt2.entropy

k2.entropy.tfidf <-entropy.sum
print(k2.entropy.tfidf)


#----------------------- Repeat for k = 3 ---------------------------------------


plot(hclustResults,cex=0.5,hang=-1,main="Hclust dendrogram of NewsGroup")
rect.hclust(hclustResults,k=3)
cluster.k3.tfidf <-cutree(hclustResults, k=3)

table.k3.tfidf <- table(cluster.k3.tfidf)

#convert to table
table.hardware <- table(cluster.k3.tfidf[1:100])
table.os <- table(cluster.k3.tfidf[101:200])
table.sci <- table(cluster.k3.tfidf[201:300])

#convert to vector
vector.count <- as.vector(table.k3.tfidf)
vector.hardware <- as.vector(table.hardware)
vector.os <- as.vector(table.os)
vector.sci <- as.vector(table.sci)

#calculate probabilities of each document type in each cluster
prob1.hardware <- (vector.hardware[1]/vector.count[1])
prob2.hardware <- (vector.hardware[2]/vector.count[2])
prob3.hardware <- (vector.hardware[3]/vector.count[3])

prob1.os <- (vector.os[1]/vector.count[1])
prob2.os <- (vector.os[2]/vector.count[2])
## there are no os doc is cluster 3

prob1.sci <- (vector.sci[1]/vector.count[1])
prob2.sci <- (vector.sci[2]/vector.count[2])
## there are no sci doc in cluster 3

pt1.entropy <- -1 * (prob1.hardware * (log2(prob1.hardware)) + prob1.os * (log2(prob1.os)) + prob1.sci*(log2(prob1.sci)))
pt2.entropy <- -1 * (prob2.hardware * (log2(prob2.hardware)) + prob2.os * (log2(prob2.os)) + prob2.sci*(log2(prob2.sci)))
pt3.entropy <- -1 * (prob3.hardware * (log2(prob3.hardware)))

entropy.sum <- (vector.count[1]/300)*pt1.entropy + (vector.count[2]/300)*pt2.entropy + (vector.count[3]/300)*pt3.entropy

k3.entropy.tfidf <-entropy.sum
print(k3.entropy.tfidf)

#------------------------- Repeat with k = 4 ------------------------------------

plot(hclustResults,cex=0.5,hang=-1,main="Hclust dendrogram of NewsGroup")
rect.hclust(hclustResults,k=4)
k4.cluster.tfidf <-cutree(hclustResults, k=4)

table.k4.tfidf <- table(k4.cluster.tfidf)

#convert to table
table.hardware <- table(k4.cluster.tfidf[1:100])
table.os <- table(k4.cluster.tfidf[101:200])
table.sci <- table(k4.cluster.tfidf[201:300])

#convert to vector
vector.count <- as.vector(table.k4.tfidf)
vector.hardware <- as.vector(table.hardware)
vector.os <- as.vector(table.os)
vector.sci <- as.vector(table.sci)

#calculate probabilities of document types in each cluster
prob1.hardware <- (vector.hardware[1]/vector.count[1])
prob2.hardware <- (vector.hardware[2]/vector.count[2])
prob3.hardware <- (vector.hardware[3]/vector.count[3])
prob4.hardware <- (vector.hardware[4]/vector.count[4])

prob1.os <- (vector.os[1]/vector.count[1])
prob2.os <- (vector.os[2]/vector.count[2])
prob3.os <- (vector.os[3]/vector.count[3])
# there are no os docs in cluster 4

prob1.sci <- (vector.sci[1]/vector.count[1])
prob2.sci <- (vector.sci[2]/vector.count[2])
prob3.sci <- (vector.sci[3]/vector.count[3])
#there are no sci docs in cluster 4

pt1.entropy <- -1 * (prob1.hardware * (log2(prob1.hardware)) + prob1.os * (log2(prob1.os)) + prob1.sci*(log2(prob1.sci)))
pt2.entropy <- -1 * (prob2.hardware * (log2(prob2.hardware)) + prob2.os * (log2(prob2.os)) + prob2.sci*(log2(prob2.sci)))
pt3.entropy <- -1 * (prob3.hardware * (log2(prob3.hardware)) + prob3.os * (log2(prob3.os)) + prob3.sci*(log2(prob3.sci)))
pt4.entropy <- -1 * (prob4.hardware * (log2(prob4.hardware)))

entropy.sum <- (vector.count[1]/300)*pt1.entropy + (vector.count[2]/300)*pt2.entropy + 
  (vector.count[3]/300)*pt3.entropy + (vector.count[4]/300)*pt4.entropy

k4.entropy.tfidf <- entropy.sum
print(k4.entropy.tfidf)


#---------------------------- Repeat for k = 5 ----------------------------------------------------------
plot(hclustResults,cex=0.5,hang=-1,main="Hclust dendogram of NewsGroup")
rect.hclust(hclustResults,k=5)
k5.cluster.tfidf <-cutree(hclustResults, k=5)

table.k5.tfidf <- table(k5.cluster.tfidf)

#convert to table
table.hardware <- table(k5.cluster.tfidf[1:100])
table.os <- table(k5.cluster.tfidf[101:200])
table.sci <- table(k5.cluster.tfidf[201:300])

#convert to vector
vector.count <- as.vector(table.k5.tfidf)
vector.hardware <- as.vector(table.hardware)
vector.os <- as.vector(table.os)
vector.sci <- as.vector(table.sci)

prob1.hardware <- (vector.hardware[1]/vector.count[1])
prob2.hardware <- (vector.hardware[2]/vector.count[2])
prob3.hardware <- (vector.hardware[3]/vector.count[3])
prob4.hardware <- (vector.hardware[4]/vector.count[4])
prob5.hardware <- (vector.hardware[5]/vector.count[5])

prob1.os <- (vector.os[1]/vector.count[1])
prob2.os <- (vector.os[2]/vector.count[2])
prob3.os <- (vector.os[3]/vector.count[3])
# there are no os docs for cluster 4
prob5.os <- (vector.os[4]/vector.count[5]) # probability of os doc in cluster 5



prob1.sci <- (vector.sci[1]/vector.count[1])
prob2.sci <- (vector.sci[2]/vector.count[2])
prob3.sci <- (vector.sci[3]/vector.count[3])
#there are no sci docs for cluster 4
prob5.sci <- (vector.sci[4]/vector.count[5]) # prob sci doc in cluster 5

pt1.entropy <- -1 * (prob1.hardware * (log2(prob1.hardware)) + prob1.os * (log2(prob1.os)) + prob1.sci*(log2(prob1.sci)))
pt2.entropy <- -1 * (prob2.hardware * (log2(prob2.hardware)) + prob2.os * (log2(prob2.os)) + prob2.sci*(log2(prob2.sci)))
pt3.entropy <- -1 * (prob3.hardware * (log2(prob3.hardware)) + prob3.os * (log2(prob3.os)) + prob3.sci*(log2(prob3.sci)))
pt4.entropy <- -1 * (prob4.hardware * (log2(prob4.hardware)))
pt5.entropy <- -1 * (prob5.hardware * (log2(prob5.hardware)) + prob5.os * (log2(prob5.os)) + prob5.sci*(log2(prob5.sci)))

entropy.sum <- (vector.count[1]/300)*pt1.entropy + (vector.count[2]/300)*pt2.entropy + (vector.count[3]/300)*pt3.entropy + 
  (vector.count[4]/300)*pt4.entropy + (vector.count[5]/300)*pt5.entropy

k5.entropy.tfidf <- entropy.sum
print(k5.entropy.tfidf)

#---------------------------------------Repeat for k = 6 ---------------------------------------------------------

plot(hclustResults,cex=0.5,hang=-1,main="Hclust dendogram of NewsGroup")
rect.hclust(hclustResults,k=6)
k6.cluster.tfidf <-cutree(hclustResults, k=6)

table.k6.tfidf <- table(k6.cluster.tfidf)

#convert to table
table.hardware <- table(k6.cluster.tfidf[1:100])
table.os <- table(k6.cluster.tfidf[101:200])
table.sci <- table(k6.cluster.tfidf[201:300])

#convert to vector
vector.count <- as.vector(table.k6.tfidf)
vector.hardware <- as.vector(table.hardware)
vector.os <- as.vector(table.os)
vector.sci <- as.vector(table.sci)

#calculate probabilities of document type in each cluster
prob1.hardware <- (vector.hardware[1]/vector.count[1])
prob2.hardware <- (vector.hardware[2]/vector.count[2])
prob3.hardware <- (vector.hardware[3]/vector.count[3])
prob4.hardware <- (vector.hardware[4]/vector.count[4])
prob5.hardware <- (vector.hardware[5]/vector.count[5])
## there are no hardware docs in cluster 6

prob1.os <- (vector.os[1]/vector.count[1])
prob2.os <- (vector.os[2]/vector.count[2])
prob3.os <- (vector.os[3]/vector.count[3])
#there are no os docs in cluster 4
prob5.os <- (vector.os[4]/vector.count[5]) #this is cluster 5
#there are no os docs in cluster 6


prob1.sci <- (vector.sci[1]/vector.count[1])
prob2.sci <- (vector.sci[2]/vector.count[2])
prob3.sci <- (vector.sci[3]/vector.count[3])
#there are no sci docs in cluster 4
prob5.sci <- (vector.sci[4]/vector.count[5]) # This is cluster 5
prob6.sci <- (vector.sci[5]/vector.count[6]) #6 This is cluster 6

pt1.entropy <- -1 * (prob1.hardware * (log2(prob1.hardware)) + prob1.os * (log2(prob1.os)) + prob1.sci*(log2(prob1.sci)))
pt2.entropy <- -1 * (prob2.hardware * (log2(prob2.hardware)) + prob2.os * (log2(prob2.os)) + prob2.sci*(log2(prob2.sci)))
pt3.entropy <- -1 * (prob3.hardware * (log2(prob3.hardware)) + prob3.os * (log2(prob3.os)) + prob3.sci*(log2(prob3.sci)))
pt4.entropy <- -1 * (prob4.hardware * (log2(prob4.hardware)))
pt5.entropy <- -1 * (prob5.hardware * (log2(prob5.hardware)) + prob5.os * (log2(prob5.os)) + prob5.sci*(log2(prob5.sci)))
pt6.entropy <- -1 * (prob6.sci*(log2(prob6.sci)))

entropy.sum <- (vector.count[1]/300)*pt1.entropy + (vector.count[2]/300)*pt2.entropy + (vector.count[3]/300)*pt3.entropy + 
  (vector.count[4]/300)*pt4.entropy + (vector.count[5]/300)*pt5.entropy + (vector.count[6]/300)*pt6.entropy

k6.entropy.tfidf<- entropy.sum
print(k6.entropy.tfidf)

entropies.tfidf<- c(k2.entropy.tfidf,k3.entropy.tfidf,k4.entropy.tfidf,k5.entropy.tfidf,k6.entropy.tfidf)
print(entropies.tfidf)
print(q1.entropies)


# Based on the entropies k= 6 with TF-IDF yielded the smallest entropy values, however the biggest drop
# in entropy values occur from k=3 to k= 4, thus 
#
#> entropies.tfidf<- c(k2.entropy.tfidf,k3.entropy.tfidf,k4.entropy.tfidf,k5.entropy.tfidf,k6.entropy.tfidf)
#> print(entropies.tfidf) <- based on removing stop words before punctuation
#[1] 1.547270 1.533631 1.465439 1.433771 1.429695
# ========================================================================================
# ========================================================================================
# ================ Repeat with TF-IDF and Using Cosine Distance ==========================
# ========================================================================================
# ========================================================================================
dtm4 <- DocumentTermMatrix(corpus.newsgroup, control=list(wordLengths=c(1,Inf), weighting = weightTfIdf))

#removing sparse terms
dtm5 <- removeSparseTerms(dtm4, sparse=0.96)

#conversion to matrix:
m4 <- as.matrix(dtm5)

# Setting Seed to reproduce results
set.seed(340)

#using cosine as distance method
distmatrix <- dist(m4, method = 'cosine')
hclustResults <- hclust(distmatrix,method="ward.D")

## ===Based on the results from Question 4 I will do Cosine analysis on k=6 with TF-IDF ========

plot(hclustResults,cex=0.5,hang=-1,main="Hclust dendrogram of messages")
rect.hclust(hclustResults,k=6)
cluster.k6.tfidf.cosine <-cutree(hclustResults, k=6)

k6.tfidf.cosine.table <- table(cluster.k6.tfidf.cosine)

#convert to table
table.hardware <- table(cluster.k6.tfidf.cosine[1:100])
table.os <- table(cluster.k6.tfidf.cosine[101:200])
table.sci <- table(cluster.k6.tfidf.cosine[201:300])

#convert to vector
vector.count <- as.vector(k6.tfidf.cosine.table)
vector.hardware <- as.vector(table.hardware)
vector.os <- as.vector(table.os)
vector.sci <- as.vector(table.sci)

#calculate probabilities of k = 6 for each document type
prob1.hardware <- (vector.hardware[1]/vector.count[1]) 
prob2.hardware <- (vector.hardware[2]/vector.count[2])
prob3.hardware <- (vector.hardware[3]/vector.count[3])
prob4.hardware <- (vector.hardware[4]/vector.count[4])
prob5.hardware <- (vector.hardware[5]/vector.count[5])
#no hardware documents in cluster 6

prob1.os <- (vector.os[1]/vector.count[1])
prob2.os <- (vector.os[2]/vector.count[2])
prob3.os <- (vector.os[3]/vector.count[3])
prob4.os <- (vector.os[4]/vector.count[4])
prob5.os <- (vector.os[5]/vector.count[5])
prob6.os <- (vector.os[6]/vector.count[6])

prob1.sci <- (vector.sci[1]/vector.count[1])
# no sci document in cluster 2
prob3.sci <- (vector.sci[2]/vector.count[3]) # of sci docs in cluster 3
# no sci documents in cluster 4
# no sci documents in cluster 5
# no sci documents in cluster 6

#calculate entropy
pt1.entropy <- -1 * (prob1.hardware * (log2(prob1.hardware)) + prob1.os * (log2(prob1.os)) + prob1.sci*(log2(prob1.sci)))
pt2.entropy <- -1 * (prob2.hardware * (log2(prob2.hardware)) + prob2.os * (log2(prob2.os)))
pt3.entropy <- -1 * (prob3.hardware * (log2(prob3.hardware)) + prob3.os * (log2(prob3.os)) + prob3.sci*(log2(prob3.sci)))
pt4.entropy <- -1 * (prob4.hardware * (log2(prob4.hardware)) + prob4.os * (log2(prob4.os)))
pt5.entropy <- -1 * (prob5.hardware * (log2(prob5.hardware)) + prob5.os * (log2(prob5.os)))
pt6.entropy <- -1 * (prob6.os * (log2(prob6.os)))

entropy.sum <- (vector.count[1]/300)*pt1.entropy + (vector.count[2]/300)*pt2.entropy + (vector.count[3]/300)*pt3.entropy + 
  (vector.count[4]/300)*pt4.entropy + (vector.count[5]/300)*pt5.entropy + (vector.count[6]/300)*pt6.entropy

k6.entropy.cosine<- entropy.sum
print(k6.entropy.cosine)

#
#k6.entropy.cosine<- entropy.sum
#> k6.entropy.cosine<- entropy.sum
#> print(k6.entropy.cosine)
#[1] 0.9255395
#--------------------- Retry with k = 5 ---------------------------------------------------------------------
plot(hclustResults,cex=0.5,hang=-1,main="Hclust dendrogram of messages")
rect.hclust(hclustResults,k=5)
cluster.k5.tfidf.cosine <-cutree(hclustResults, k=5)

k5.tfidf.cosine.table <- table(cluster.k5.tfidf.cosine)

#convert to table
table.hardware <- table(cluster.k5.tfidf.cosine[1:100])
table.os <- table(cluster.k5.tfidf.cosine[101:200])
table.sci <- table(cluster.k5.tfidf.cosine[201:300])

#convert to vector
vector.count <- as.vector(k5.tfidf.cosine.table)
vector.hardware <- as.vector(table.hardware)
vector.os <- as.vector(table.os)
vector.sci <- as.vector(table.sci)

#calculate probabilites of each document type in each cluster
prob1.hardware <- (vector.hardware[1]/vector.count[1]) 
prob2.hardware <- (vector.hardware[2]/vector.count[2])
prob3.hardware <- (vector.hardware[3]/vector.count[3])
prob4.hardware <- (vector.hardware[4]/vector.count[4])
#no hardware docs in cluster 5

prob1.os <- (vector.os[1]/vector.count[1])
prob2.os <- (vector.os[2]/vector.count[2])
prob3.os <- (vector.os[3]/vector.count[3])
prob4.os <- (vector.os[4]/vector.count[4])
prob5.os <- (vector.os[5]/vector.count[5])

prob1.sci <- (vector.sci[1]/vector.count[1])
#no sci docs in cluster 2
prob3.sci <- (vector.sci[2]/vector.count[3])
# No sci documents in cluster 3, 4 ,5 


#calculate entropies
pt1.entropy <- -1 * (prob1.hardware * (log2(prob1.hardware)) + prob1.os * (log2(prob1.os)) + prob1.sci*(log2(prob1.sci)))
pt2.entropy <- -1 * (prob2.hardware * (log2(prob2.hardware)) + prob2.os * (log2(prob2.os)))
pt3.entropy <- -1 * (prob3.hardware * (log2(prob3.hardware)) + prob3.os * (log2(prob3.os)) + prob3.sci*(log2(prob3.sci)))
pt4.entropy <- -1 * (prob4.hardware * (log2(prob4.hardware)) + prob4.os * (log2(prob4.os)))
pt5.entropy <- -1 * (prob5.os * (log2(prob5.os)))

entropy.sum <- (vector.count[1]/300)*pt1.entropy + (vector.count[2]/300)*pt2.entropy + (vector.count[3]/300)*pt3.entropy + 
  (vector.count[4]/300)*pt4.entropy + (vector.count[5]/300)*pt5.entropy

k5.entropy.cosine<- entropy.sum
print(k5.entropy.cosine)

#
# k5.entropy.cosine<- entropy.sum
#print(k5.entropy.cosine)
#[1] 0.9286528
#
#
# --------------------- Repeat k= 4 ------------------------------------------
plot(hclustResults,cex=0.5,hang=-1,main="Hclust dendrogram of messages")
rect.hclust(hclustResults,k=4)
cluster.k4.tfidf.cosine <-cutree(hclustResults, k=4)

k4.tfidf.cosine.table <- table(cluster.k4.tfidf.cosine)

#convert to table
table.hardware <- table(cluster.k4.tfidf.cosine[1:100])
table.os <- table(cluster.k4.tfidf.cosine[101:200])
table.sci <- table(cluster.k4.tfidf.cosine[201:300])

#convert to vector
vector.count <- as.vector(k4.tfidf.cosine.table)
vector.hardware <- as.vector(table.hardware)
vector.os <- as.vector(table.os)
vector.sci <- as.vector(table.sci)

#calculate probabilites of each document type in each cluster
prob1.hardware <- (vector.hardware[1]/vector.count[1]) 
prob2.hardware <- (vector.hardware[2]/vector.count[2])
prob3.hardware <- (vector.hardware[3]/vector.count[3])
#no hardware docs in cluster 4

prob1.os <- (vector.os[1]/vector.count[1])
prob2.os <- (vector.os[2]/vector.count[2])
prob3.os <- (vector.os[3]/vector.count[3])
prob4.os <- (vector.os[4]/vector.count[4])


prob1.sci <- (vector.sci[1]/vector.count[1])
prob2.sci <- (vector.sci[2]/vector.count[2])
# No sci documents in cluster 3, 4

#calculate entropies
pt1.entropy <- -1 * (prob1.hardware * (log2(prob1.hardware)) + prob1.os * (log2(prob1.os)) + prob1.sci*(log2(prob1.sci)))
pt2.entropy <- -1 * (prob2.hardware * (log2(prob2.hardware)) + prob2.os * (log2(prob2.os)) + prob2.sci*(log2(prob2.sci)))
pt3.entropy <- -1 * (prob3.hardware * (log2(prob3.hardware)) + prob3.os * (log2(prob3.os)))
pt4.entropy <- -1 * (prob4.os * (log2(prob4.os)))

entropy.sum <- (vector.count[1]/300)*pt1.entropy + (vector.count[2]/300)*pt2.entropy + (vector.count[3]/300)*pt3.entropy + 
  (vector.count[4]/300)*pt4.entropy

k4.entropy.cosine<- entropy.sum
print(k4.entropy.cosine)

#
#> k4.entropy.cosine<- entropy.sum
#> print(k4.entropy.cosine)
#[1] 0.9644349
#

#-------------------------k = 3 ---------------------------------------------------------
plot(hclustResults,cex=0.5,hang=-1,main="Hclust dendrogram of messages")
rect.hclust(hclustResults,k=3)
cluster.k3.tfidf.cosine <-cutree(hclustResults, k=3)

k3.tfidf.cosine.table <- table(cluster.k3.tfidf.cosine)

#convert to table
table.hardware <- table(cluster.k3.tfidf.cosine[1:100])
table.os <- table(cluster.k3.tfidf.cosine[101:200])
table.sci <- table(cluster.k3.tfidf.cosine[201:300])

#convert to vector
vector.count <- as.vector(k3.tfidf.cosine.table)
vector.hardware <- as.vector(table.hardware)
vector.os <- as.vector(table.os)
vector.sci <- as.vector(table.sci)

#calculate probabilites of each document type in each cluster
prob1.hardware <- (vector.hardware[1]/vector.count[1]) 
prob2.hardware <- (vector.hardware[2]/vector.count[2])
prob3.hardware <- (vector.hardware[3]/vector.count[3])

prob1.os <- (vector.os[1]/vector.count[1])
prob2.os <- (vector.os[2]/vector.count[2])
prob3.os <- (vector.os[3]/vector.count[3])

prob1.sci <- (vector.sci[1]/vector.count[1])
prob2.sci <- (vector.sci[2]/vector.count[2])
# No sci documents in cluster 3

#calculate entropies
pt1.entropy <- -1 * (prob1.hardware * (log2(prob1.hardware)) + prob1.os * (log2(prob1.os)) + prob1.sci*(log2(prob1.sci)))
pt2.entropy <- -1 * (prob2.hardware * (log2(prob2.hardware)) + prob2.os * (log2(prob2.os)) + prob2.sci*(log2(prob2.sci)))
pt3.entropy <- -1 * (prob3.hardware * (log2(prob3.hardware)) + prob3.os * (log2(prob3.os))))

entropy.sum <- (vector.count[1]/300)*pt1.entropy + (vector.count[2]/300)*pt2.entropy + (vector.count[3]/300)*pt3.entropy

k3.entropy.cosine<- entropy.sum
print(k3.entropy.cosine)

#
#> k3.entropy.cosine<- entropy.sum
#> print(k3.entropy.cosine)
#[1] 0.9835688
#

# --------------------------- Repeat k = 2 --------------------------------------------------
plot(hclustResults,cex=0.5,hang=-1,main="Hclust dendrogram of messages")
rect.hclust(hclustResults,k=2)
cluster.k2.tfidf.cosine <-cutree(hclustResults, k=2)

k2.tfidf.cosine.table <- table(cluster.k2.tfidf.cosine)

#convert to table
table.hardware <- table(cluster.k2.tfidf.cosine[1:100])
table.os <- table(cluster.k2.tfidf.cosine[101:200])
table.sci <- table(cluster.k2.tfidf.cosine[201:300])

#convert to vector
vector.count <- as.vector(k2.tfidf.cosine.table)
vector.hardware <- as.vector(table.hardware)
vector.os <- as.vector(table.os)
vector.sci <- as.vector(table.sci)

#calculate probabilites of each document type in each cluster
prob1.hardware <- (vector.hardware[1]/vector.count[1]) 
prob2.hardware <- (vector.hardware[2]/vector.count[2])

prob1.os <- (vector.os[1]/vector.count[1])
prob2.os <- (vector.os[2]/vector.count[2])

prob1.sci <- (vector.sci[1]/vector.count[1])
prob2.sci <- (vector.sci[2]/vector.count[2])


#calculate entropies
pt1.entropy <- -1 * (prob1.hardware * (log2(prob1.hardware)) + prob1.os * (log2(prob1.os)) + prob1.sci*(log2(prob1.sci)))
pt2.entropy <- -1 * (prob2.hardware * (log2(prob2.hardware)) + prob2.os * (log2(prob2.os)) + prob2.sci*(log2(prob2.sci)))

entropy.sum <- (vector.count[1]/300)*pt1.entropy + (vector.count[2]/300)*pt2.entropy 

k2.entropy.cosine<- entropy.sum
print(k2.entropy.cosine)

cosine.entropies <- c(k2.entropy.cosine,k3.entropy.cosine,k4.entropy.cosine,k5.entropy.cosine,k6.entropy.cosine)

print(cosine.entropies)

#
#> print(cosine.entropies)
#[1] 1.0977008 0.9835688 0.9644349 0.9286528 0.9255395
#

