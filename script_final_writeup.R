############################
## Devoir not? par les pairs
###########################

library(ggplot2)
library(caret)
library(randomForest)
couleurs <- c('#d7191c','#fdae61','#ffffbf','#abdda4','#2b83ba')
set.seed(4658)

#The goal of your project is to predict the manner in which they did the exercise. 
#This is the "classe" variable in the training set. 
#You may use any of the other variables to predict with. 
#You should create a report describing how you built your model, how you used cross validation, 
#what you think the expected out of sample error is, and why you made the choices you did. 
#You will also use your prediction model to predict 20 different test cases.gf
dataset <- read.table("F:/mooc/Practical_machine_learning_John_Hopkins/final_writeup/pml-training.csv", header=T, sep=",")
partition <- createDataPartition(dataset$classe, p=0.75, list=F)
training <- dataset[partition,-c(1,3:5)]
testing <- dataset[-partition,-c(1,3:5)]

##
summary(training$classe)
table(training$user_name, training$classe)

#Missing values
nalength <- function(col){length(which(is.na(col)==T))}
missv <- data.frame(name=colnames(training), missval=apply(training,2,nalength))
training <- training[,which(missv$missval == 0)]
testing <- testing[,which(missv$missval == 0)]

nothinglength <- function(col){length(which(col==""))}
nothings <- data.frame(name=colnames(training), nothingval=apply(training,2,nothinglength))
training <- training[,which(nothings$nothingval == 0)]
testing <- testing[,which(nothings$nothingval == 0)]

nsv <- nearZeroVar(training, saveMetrics = T)
summary(training[,which(nsv$nzv == T)])

ggplot(training, aes(roll_belt, fill=classe)) + geom_density(alpha=0.5) + scale_fill_manual(values=couleurs)
ggplot(training, aes(pitch_forearm, fill=classe)) + geom_density(alpha=0.5) + scale_fill_manual(values=couleurs)


## Creation of 10 folds to  test different method 
mes10folds <- createFolds(y=training$classe, k=10, list=F, returnTrain = F)

#Performance of a simple tree, testing on fold 1
modFittree <- train(classe~., method="rpart", data=training[which(mes10folds != 1),])
confusionMatrix(predict(modFittree,training[which(mes10folds == 1),]),training$classe[which(mes10folds == 1)] )
    #accuracy = 0.4925

#Performance on random forest, testing on fold 2
modFitrf <- randomForest(classe~. , data=training[which(mes10folds != 2),], ntree = 15)
confusionMatrix(predict(modFitrf,training[which(mes10folds == 2),]),training$classe[which(mes10folds == 2)])
    #accuracy = 1
    
modFitrf <- randomForest(classe~., data=training, ntree=10)    
pred <- predict(modFitrf, testing)

varImpPlot(modFitrf)

#Performance on glm, testing on fold 3
modFitglm <- train(classe~., method="glm", data=training[which(mes10folds != 3),])
confusionMatrix(predict(modFitglm,training[which(mes10folds == 3),]),training$classe[which(mes10folds == 3)])
  #accuracy = 0.
  #very long


