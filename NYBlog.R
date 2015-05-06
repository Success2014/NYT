NewsTrain = read.csv("NYTimesBlogTrain.csv",stringsAsFactors = FALSE)
NewsTest = read.csv("NYtimesBlogTest.csv",stringsAsFactors = FALSE)

#exploratory analysis
summary(NewsTrain)
summary(NewsTest)
str(NewsTrain)
str(NewsTest)
table(NewsTrain$NewsDesk, NewsTrain$SectionName)
table(NewsTest$NewsDesk, NewsTest$SectionName)

hist(NewsTrain$WordCount,breaks=80,xlim=c(0,4000))
hist(log(NewsTrain$WordCount+1),breaks=80,xlim=c(0,10))

#preprocessing data
#change 0, 1 to "unpop" and "pop", otherwise gbm model will report error
#since 0, 1 cannot be variable names, check the following posts:
#http://stackoverflow.com/questions/24081246/predicting-probabilities-for-gbm-with-caret-library
#http://stackoverflow.com/questions/18402016/error-when-i-try-to-predict-class-probabilities-in-r-caret
NewsTrain$Containhelp = NA # see if the headline, abstract contain the word "help"
NewsTest$Containhelp = NA
NewsTrain$ContainChina = NA
NewsTest$ContainChina = NA
NewsTrain$ContainApple = NA
NewsTest$ContainApple = NA
for (i in 1:nrow(NewsTrain)){
        NewsTrain$Popular[i] = ifelse(NewsTrain$Popular[i] == 1, "pop","unpop")
        NewsTrain$Containhelp[i] = ifelse(grepl("help",NewsTrain$Headline[i],fixed=TRUE)
                                          |grepl("help",NewsTrain$Abstract[i],fixed=TRUE),1,0)
        NewsTrain$ContainChina[i] = ifelse(grepl("China",NewsTrain$Headline[i],fixed=TRUE)
                                          |grepl("China",NewsTrain$Abstract[i],fixed=TRUE),1,0)
        NewsTrain$ContainApple[i] = ifelse(grepl("Apple",NewsTrain$Headline[i],fixed=TRUE)
                                          |grepl("Apple",NewsTrain$Abstract[i],fixed=TRUE),1,0)
}
for (i in 1:nrow(NewsTest)){
        NewsTest$Containhelp[i] = ifelse(grepl("help",NewsTest$Headline[i],fixed=TRUE)
                                          |grepl("help",NewsTest$Abstract[i],fixed=TRUE),1,0)
        NewsTest$ContainChina[i] = ifelse(grepl("China",NewsTest$Headline[i],fixed=TRUE)
                                           |grepl("China",NewsTest$Abstract[i],fixed=TRUE),1,0)
        NewsTest$ContainApple[i] = ifelse(grepl("Apple",NewsTest$Headline[i],fixed=TRUE)
                                           |grepl("Apple",NewsTest$Abstract[i],fixed=TRUE),1,0)
}
NewsTrain$Containhelp = as.factor(NewsTrain$Containhelp)
NewsTest$Containhelp = as.factor(NewsTest$Containhelp)
NewsTrain$ContainChina = as.factor(NewsTrain$ContainChina)
NewsTest$ContainChina = as.factor(NewsTest$ContainChina)
NewsTrain$ContainApple = as.factor(NewsTrain$ContainApple)
NewsTest$ContainApple = as.factor(NewsTest$ContainApple)


NewsTrain$Popular = as.factor(NewsTrain$Popular)





NewsTrain$PubDate = strptime(NewsTrain$PubDate, "%Y-%m-%d %H:%M:%S")
NewsTest$PubDate = strptime(NewsTest$PubDate, "%Y-%m-%d %H:%M:%S")
NewsTrain$Weekday = NewsTrain$PubDate$wday
NewsTest$Weekday = NewsTest$PubDate$wday

NewsTrain$Hour = NewsTrain$PubDate$h
NewsTest$Hour = NewsTest$PubDate$h

#fill in missing values 
#fill in the missing values seems worsen the score
unique(NewsTrain$NewsDesk) # see how many newsdesk factors, totally 13
#library(mice)
#Train_missing = NewsTrain[c('NewsDesk','SectionName','SubsectionName')]
#Train_missing[Train_missing == ""] = NA # change empty space to NA
#Test_missing = NewsTest[c('NewsDesk','SectionName','SubsectionName')]
#Test_missing[Test_missing == ""] = NA # change empty space to NA
#Train_imputed = complete(mice(Train_missing))
#Test_imputed = complete(mice(Test_missing))

#for (i in 1:nrow(NewsTrain)) {  
#        NewsTrain$SubsectionName[i] <- ifelse(nchar(NewsTrain$SubsectionName[i])==0, NewsTrain$SectionName[i], 
#                                              NewsTrain$SubsectionName[i])
#}
#for (i in 1:nrow(NewsTrain)) {  
#        NewsTrain$SubsectionName[i] <- ifelse(nchar(NewsTrain$SubsectionName[i])==0, NewsTrain$NewsDesk[i], 
#                                              NewsTrain$SubsectionName[i])
#}
#table(NewsTrain$SubsectionName)## default Test subsection name from sectionName |NewsDesk
#table(NewsTest$SubsectionName)
#for (i in 1:nrow(NewsTest)) {  
#        NewsTest$SubsectionName[i] <- ifelse(nchar(NewsTest$SubsectionName[i])==0, NewsTest$SectionName[i], 
#                                             NewsTest$SubsectionName[i])
#}
#for (i in 1:nrow(NewsTest)) {  
#        NewsTest$SubsectionName[i] <- ifelse(nchar(NewsTest$SubsectionName[i])==0, NewsTest$NewsDesk[i], 
#                                             NewsTest$SubsectionName[i])
#}
#table(NewsTest$SubsectionName)







NewsTrain$NewsDesk = as.factor(NewsTrain$NewsDesk)
NewsTrain$NewsDesk = relevel(NewsTrain$NewsDesk,"Business")
NewsTest$NewsDesk = factor(NewsTest$NewsDesk, levels = levels(NewsTrain$NewsDesk))

NewsTrain$SectionName = as.factor(NewsTrain$SectionName)
NewsTrain$SectionName = relevel(NewsTrain$SectionName,"Business Day")
NewsTest$SectionName = factor(NewsTest$SectionName, levels = levels(NewsTrain$SectionName))


NewsTrain$SubsectionName = as.factor(NewsTrain$SubsectionName)
NewsTrain$SubsectionName = relevel(NewsTrain$SubsectionName,"Dealbook")
NewsTest$SubsectionName = factor(NewsTest$SubsectionName, levels = levels(NewsTrain$SubsectionName))


#---method 1 simple logistic model
SimpleMod = glm(Popular ~ WordCount, data=NewsTrain, family=binomial)
PredTest = predict(SimpleMod, newdata=NewsTest, type="response")


#---method 2
LogiMod2 = glm(Popular ~ WordCount + NewsDesk + Weekday + SectionName + SubsectionName, data = NewsTrain, family = binomial)
PredTest2 = predict(LogiMod2, newdata = NewsTest, type = "response")

#build text corpus for training set
library(tm)
NewsTrain$Headline = paste(NewsTrain$Headline, NewsTrain$Abstract, sep = " ")
NewsTest$Headline = paste(NewsTest$Headline, NewsTest$Abstract, sep = " ") #Snippet not included, since it is contained in Abstract
corpus = Corpus(VectorSource(c(NewsTrain$Headline,NewsTest$Headline)))
corpus = tm_map(corpus, tolower)
corpus = tm_map(corpus, PlainTextDocument)
corpus = tm_map(corpus, removePunctuation)
corpus = tm_map(corpus, removeWords, stopwords("english"))
corpus = tm_map(corpus, stemDocument)
frequencies = DocumentTermMatrix(corpus)
sparse = removeSparseTerms(frequencies, 0.98)
NewsSparse = as.data.frame(as.matrix(sparse))
colnames(NewsSparse) = make.names(colnames(NewsSparse))

Train = head(NewsSparse, nrow(NewsTrain))
Test = tail(NewsSparse, nrow(NewsTest))

Train$Popular = NewsTrain$Popular # random forests only accept factors
Train$WordCount = log(NewsTrain$WordCount+1)
Test$WordCount = log(NewsTest$WordCount+1)
Train$Weekday = NewsTrain$Weekday
Test$Weekday = NewsTest$Weekday
Train$Hour = NewsTrain$Hour
Test$Hour = NewsTest$Hour
Train$NewsDesk = NewsTrain$NewsDesk
Test$NewsDesk = NewsTest$NewsDesk
Train$SectionName = NewsTrain$SectionName
Test$SectionName = NewsTest$SectionName
Train$SubsectionName = NewsTrain$SubsectionName
Test$SubsectionName = NewsTest$SubsectionName

Train$Containhelp = NewsTrain$Containhelp
Test$Containhelp = NewsTest$Containhelp
TrainContainChina = NewsTrain$ContainChina
Test$ContainChina = NewsTest$ContainChina
Train$ContainApple = NewsTrain$ContainApple
Test$ContainApple = NewsTest$ContainApple



hist(Train$WordCount,breaks=80,xlim=c(0,10))
hist(Test$WordCount,breaks=80,xlim=c(0,10))

#---build tree model
library(rpart)
library(rpart.plot)
NewsCART = rpart(Popular~., data = Train, method = "class")
prp(NewsCART)
PredCART = predict(NewsCART, newdata = Test)[,2]

#---build logistic model
LogiMod3 = glm(Popular ~ ., data = Train, family = binomial)
PredTest3 = predict(LogiMod3, newdata = Test, type = "response")

#---build random forest model
library(randomForest)
Train$Popular = as.factor(Train$Popular)
NewsRF = randomForest(Popular ~., data = Train, ntree = 1000)
predictRF = predict(NewsRF, newdata = Test,type="prob")[,2]

#---build neural networks model
#library("neuralnet")
#nnm <- neuralnet(Popular~.,data=Train, hidden=10, threshold=0.01) # must type in independent variables one by one
library(nnet) 
# try corpus with less words and increase hidden layer
NewsNN = nnet(Popular~.,data=Train,size = 5,Hess=FALSE,maxit=2000)
predictNN = predict(NewsNN, newdata = Test)

#---build random forest model using cross validation
library(caret)
modelLookup('rf') # check which parameters could be tuned by caret
library(randomForest)
library(ggplot2)

Yvals = Train$Popular
IndependentVars = Train
IndependentVars$Popular = NULL # Drop the Dependent variable column

fitControl <- trainControl(method="repeatedcv", number=10, repeats = 5, classProbs = TRUE, summaryFunction = twoClassSummary)
tr = train(IndependentVars, Yvals, method="rf", nodesize=5, ntree=500, metric="ROC", trControl=fitControl)
#tr = train(Popular~., data=Train, method="rf", nodesize=5, ntree=500, metric="ROC", trControl=fitControl)
predRFCV = predict(tr$finalModel,newdata=Test,type="prob")[,2]

#---build GBM(gradient boosting machine) model using cross validation
library(caret)
modelLookup('gbm')

Yvals = Train$Popular
IndependentVars = Train
IndependentVars$Popular = NULL # Drop the Dependent variable column

fitControl <- trainControl(method="repeatedcv", number=10, repeats = 3, classProbs = TRUE, summaryFunction = twoClassSummary)
gbmGrid = expand.grid(interaction.depth = c(1,5,9), n.trees = c(500,1000,1500), shrinkage = 0.1)
gbmFit = train(IndependentVars, Yvals, method = "gbm", metric="ROC", trControl = fitControl,verbose=FALSE,tuneGrid=gbmGrid)
predGBMCV = predict(gbmFit,newdata=Test,type="prob")[,1]


#===================#
#===write results===#
#===================#
MySubmission = data.frame(UniqueID = NewsTest$UniqueID, Probability1 = predGBMCV)
write.csv(MySubmission, "SubmissionGBM1.csv", row.names=FALSE)




#Average over all predictions
pd1 = read.csv("SubmissionCART.csv")
pd2 = read.csv("SubmissionHeadlineLog.csv")
pd3 = read.csv("SubmissionLogi2.csv")
pd4 = read.csv("SubmissionLogi3.csv")
pd5 = read.csv("SubmissionNN.csv")
pd6 = read.csv("SubmissionNN2.csv")
pd7 = read.csv("SubmissionRF.csv")
pd8 = read.csv("SubmissionRF2.csv")
pd9 = read.csv("SubmissionRF3.csv")
pd10 = read.csv("SubmissionRF4.csv")
pd11 = read.csv("SubmissionRFCV1.csv")
pd12 = read.csv("SubmissionSimpleLog.csv")
pd13 = read.csv("SubmissionGBM1.csv")
avgpred = (pd1$Probability1+pd3$Probability1+pd4$Probability1+pd5$Probability1+pd6$Probability1+pd7$Probability1+pd8$Probability1+pd9$Probability1+pd10$Probability1+pd11$Probability1+pd13$Probability1)/11
