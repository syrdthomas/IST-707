#Homeworks 4 & 5
#Packages and Libraries
install.packages("RColorBrewer")
install.packages("NLP")
install.packages("tm")
install.packages("proxy")
install.packages("dplyr")
install.packages("mclust")
install.packages("factoextra")
install.packages("tibble")
install.packages("fancyRpartPlot")
install.packages("rpart")
install.packages("rplot")
install.packages("rpart.plot")
install.packages("rattle")
library(RColorBrewer)
library(wordcloud)
library(slam)
library(quanteda)
library(SnowballC)
library(arules)
library(cluster)
library(stringi)
library(Matrix)
library(tidytext)
library(dplyr)
library(ggplot2)
library(mclust)
library(proxy)
library(tibble)
library(rpart)
library(rpart.plot)
library(rattle)

#Load Federal Papers Corpus file
FedPapersCorpus<- Corpus(DirSource("C:\\Users\\deand\\OneDrive\\Documents\\SyrU ADS\\IST707\\Week 4\\FedPapersCorpus"))
(numberFedPapers<- length(FedPapersCorpus))

#The following will show you that all documents were read into R
summary(FedPapersCorpus)

(meta(FedPapersCorpus[[1]]))

(meta(FedPapersCorpus[[1]],5))

#Data Cleaning
#Here we investigate the data and vectorize it using Document Term Matrix. We 
#will ignore very infrequent words and very frequent words during the 
#vectorization. Note: The Document Term Matrix method will perform much data 
#cleaning for us. 

#Data Prep and Transformation on Fed Papers
#Removing punctuation, numbers, and spaces
(getTransformations())
(nFedPapersCorpus<- length(FedPapersCorpus))

#Ignore extremely rare words such as those appearing less than 1% in documents
(minTermFreq<- nFedPapersCorpus * 0.0001)

#Ignore extremely common words such as those appearing more than 50% in docs
(maxTermFreq<- nFedPapersCorpus * 1)

(MyStopWords<- c("will", "one", "two", "may", "less", "well", "might", "withou", "small", "single", "several", "but", "very", "can", "must", "also", "any", "and", "are", "however", "into", "almost", "can", "for", "add"))

#stopwords
(STOPS<- stopwords('english'))

Papers_DTM<- DocumentTermMatrix(FedPapersCorpus,
                                control = list(
                                  stopwords = TRUE,
                                  wordLengths = c(3, 15),
                                  removePunctuation = T,
                                  removeNumbers = T,
                                  tolower = T,
                                  stemming = T,
                                  remove_separators = T, 
                                  stopwords = MyStopWords,
                              #removeWords(STOPS), #use the "buil-in" STOP words
                                  bounds = list(global = c(minTermFreq, maxTermFreq))
                                ))                                  
                            
#inspect FedPapers Document Term Matrix (DTM)
DTM<- as.matrix(Papers_DTM)
(DTM[1:11, 1:10])

#Inspect Initial Cleaning Results
#Investigate the initial results of data cleaning. Depedning on the results, we 
#decide to go back and "rec-lean" the data by providing more stop words. Let's 
#inspect the word frequencies. 

#Look at word frequencies
WordFreq<- colSums(as.matrix(Papers_DTM))
(head(WordFreq))
(length(WordFreq))
ord<- order(WordFreq)
(WordFreq[head(ord)])
(WordFreq[tail(ord)])

#Row Sums per Fed Papers
(Row_Sum_Per_doc<- rowSums((as.matrix(Papers_DTM))))

#Normalization
#In text processing, it is often benefitial to normalize the word vectors 
#before applying standard analysis techniques. 

#Create a normalized version of Papers_DTM
Papers_M<- as.matrix(Papers_DTM)
Papers_M_N1<- apply(Papers_M, 1, function(i) round(i/sum(i),3))
Papers_Matrix_Norm<- t(Papers_M_N1)

#Have a look at the orignal and the norm to make sure
(Papers_M[c(1:11), c(1000:1010)])

(Papers_Matrix_Norm[c(1:11), c(1000:1010)])

#From the line of code
# '(Row_Sum_Per_doc<- rowSums((as.matrix(FedPapersDTM))))
# above, we can see that dispt_fed_text_53/txt has a row sum of 1035
#. So, we can confirm correctness. For word "curious" we should have
# 1/1034 = 0.001 rounded, which is what we have. 

#Data Structures
#Depending on the subsequent analysis, we may need to restructure the data. Here 
#is an example

#Convert to matrix view
Papers_dtm_matrix = as.matrix(Papers_DTM)
str(Papers_dtm_matrix)
(Papers_dtm_matrix[c(1:11), c(2:10)])

#Also convert to DF
Papers_DF<- as.data.frame(as.matrix(Papers_DTM))
str(Papers_DF)
(Papers_DF$abolit)
(nrow(Papers_DF)) #Each row is Paper

#Example of Word Cloud
#Note: this word cloud package requires our data to be in DTM format
#Wordcloud Visualization Hamilton, Madison, and Disputed Papers
DisputedPapersWC<- wordcloud(colnames(Papers_dtm_matrix), Papers_dtm_matrix[11, ])
(head(sort(as.matrix(Papers_DTM)[11,], decreasing = TRUE), n=50))
HamiltonPapersWC<- wordcloud(colnames(Papers_dtm_matrix), Papers_dtm_matrix[12:62, ])
MadisonPapersWC<- wordcloud(colnames(Papers_dtm_matrix), Papers_dtm_matrix[63:77, ])

#Analysis
# Now that the data has been vectorized, analysis can take place. We will apply
#some distance metrics and see how the data clusters. 
#Note: Cosine distance usually works well with high dimensional data

#Distance Metrics
#Below we compute a variety of distance metrics to determine which seems to work 
#the best.

#Distance Measure
m<- Papers_dtm_matrix
m_norm<- Papers_Matrix_Norm
#m<- [1:2, 1:3]
distMatrix_E<- dist(m, method =  "euclidean")
print(distMatrix_E)
distMatrix_M<- dist(m, method = "manhattan")
print(distMatrix_M)
distMatrix_C<- dist(m, method = "cosine")
print(distMatrix_C)
distMatrix_C_norm<- dist(m_norm, method = "cosine")
print(distMatrix_C_norm)
#Cosine similarity works the best. Norm and not norm is about the same becayse 
#the size of the Papers are significantly different. 

#Clustering
#Below are some HAC result. Which does best? Why?

#Clustering methods:
#HAC: Hierarchical Algorithm Clustering Method

#Euclidean
groups_E<- hclust(distMatrix_E, method = "ward.D")
plot(groups_E, cex = 0.5, font = 22, hang = -1, main = "HAC Cluster Dendrogram with Euclidean Simlarity")
rect.hclust(groups_E, k=2)


#Manhattan
groups_M<- hclust(distMatrix_M, method = "ward.D")
plot(groups_M, cex = 0.5, font = 22, hang = -1, main = "HAC Cluster Dendrogram with Manhattan Simlarity")
rect.hclust(groups_M, k=2)

#Cosine Similarity
groups_C<- hclust(distMatrix_C, method = "ward.D")
plot(groups_C, cex = 0.5, font = 22, hang = -1, main = "HAC Cluster Dendrogram with Cosine Simlarity")
rect.hclust(groups_C, k=2)

#Cosine Similarity for Normalized Matrix
groups_C_n<- hclust(distMatrix_C_norm, method = "ward.D")
plot(groups_C_n, cex = 0.5, font = 22, hang = -1, main = "HAC Cluster Dendrogram with Cosine Simlarity Normalized Matrix")
rect.hclust(groups_C_n, k=2)

#Below are some k-means results. 
#K means clustering methods
X<- m_norm
k2<- kmeans(X, centers = 2, nstart = 100, iter.max = 50)
str(k2)
k3<- kmeans(X, centers = 7, nstart = 50, iter.max = 50)
str(k3)

#k means visulization results
distance1<- get_dist(X, method = "manhattan")
fviz_dist(distance1, gradient = list(low = "#00AFBB", mid = "white", high = "#FC4E07"))

distance2<- get_dist(X, method = "euclidean")
fviz_dist(distance2, gradient = list(low = "#00AFBB", mid = "white", high = "#FC4E07"))

distance3<- get_dist(X, method = "spearman")
fviz_dist(distance3, gradient = list(low = "#00AFBB", mid = "white", high = "#FC4E07"))

str(X)

#K-means
kmeansFIT_1<- kmeans(X, centers = 4)
kmeansFIT_1
summary(kmeansFIT_1)
(kmeansFIT_1$cluster)


kmeansFIT_2<- kmeans(X, centers = 3)
kmeansFIT_2
summary(kmeansFIT_2)
(kmeansFIT_2$cluster)

#_______________________________________________________________________________

#Homework 5

#Label the Data

#Below we label the data, prepare for modeling, and create some wordclouds.  
#Convert to DF
Papers_DF1<- Papers_DF%>%rownames_to_column()
names(Papers_DF1)[1]<- "Author"
Papers_DF1[1:11,1]="dispt"
Papers_DF1[12:62,1]="hamil"
Papers_DF1[63:85,1]="madis"
head(Papers_DF1)

#Wordcloud Visualization Hamilton, Madison, and Disputed Papers
DisputedPapersWC<- wordcloud(colnames(Papers_dtm_matrix), Papers_dtm_matrix)
head(sort(as.matrix(Papers_dtm_matrix)[11,], decreasing = TRUE, n = 50))
HamiltonPapersWC<- wordcloud(colnames(Papers_dtm_matrix), Papers_dtm_matrix[12:62,])
MadisonPapersWC<- wordcloud(colnames(Papers_dtm_matrix), Papers_dtm_matrix[63:77,])

#Experimental Design

#Now that the data is labeled, its time to design an experiment. Below we 
#randomly select a train and test a set for validation using function: 
#sample.int().

#Make Train and Test sets
numDisputed = 11
numTotalPapers = nrow(Papers_DF1)
trainRatio<- .60
set.seed(11) #Set seed so that same sample can be reproduced in future also
sample<- sample.int(n = numTotalPapers-numDisputed, size = floor(trainRatio+numTotalPapers), replace = TRUE)
newSample = sample + numDisputed
train<- Papers_DF1[newSample, ]
test<- Papers_DF1[-newSample, ]
#train / test ratio
length(newSample)/nrow(Papers_DF1)

#Classification
#We are now ready to train and test using classifiers. Below we use a few
#different decision tree models. Try different parameters and prunings to get
#varied results
#Use FacyRpartPlot to visualize the learned tree models. What do these diagrams 
#display?

#Decision Tree Models
#Train Tree Model 1
train_tree1<- rpart(Author ~ ., data = train, method = "class", control = rpart.control(cp=0))
summary(train_tree1)

#Predict the test dataset using the model for train tree No. 1
predicted1= predict(train_tree1, test, type = "class")
#plot number of splits
rsq.rpart(train_tree1)
plotcp(train_tree1)

#Plot the decision tree
rpart.plot(train_tree1)
fancyRpartPlot(train_tree1)

#confusion matrix to find correct and incorrect predictions
table(Authorship = predicted1, true = test$Author)

#Train Tree Model 2
train_tree2<- rpart(Author ~ upon + may + will + one, data = train, method = "class", control = rpart.control(cp = 0, minsplit = 2, maxdepth = 3))
summary(train_tree2)

#predict the test dataset using the model for train tree No.1
predicted2= predict(train_tree2, test, type = "class")
#plot number of splits
rsq.rpart(train_tree2)
plotcp(train_tree2)

#plot the decision tree
fancyRpartPlot(train_tree2)

#confusion matrix to find correct and incorrect predictions
table(Authorship = predicted2, trus = test$Author)