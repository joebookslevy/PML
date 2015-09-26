##The goal of your project is to predict the manner in which they did the exercise. 
##This is the "classe" variable in the training set. You may use any of the other 
##variables to predict with. You should create a report describing how you built 
##your model, how you used cross validation, what you think the expected out of 
##sample error is, and why you made the choices you did. You will also use your 
##prediction model to predict 20 different test cases.

library(caret)

##Set working directory
setwd("C:/Users/jdlevy/Documents/datasciencecoursera/08_PracticalMachineLearning")

##Download training dataset 
if(!file.exists("./training")){dir.create("./training")}
fileURL<-"https://d396qusza40orc.cloudfront.net/predmachlearn/pml-training.csv"
destfile<-"training.csv"
download.file(fileURL, destfile=paste("training", destfile, sep="/"))
dateDownloaded<-date()
dateDownloaded

##Adjust working directory
setwd("C:/Users/jdlevy/Documents/datasciencecoursera/08_PracticalMachineLearning/training")

##Load/read activity data file
training<-read.csv("training.csv", na.strings=c("NA", "#DIV/0!", ""))
##Remove NAs
training<-training[,colSums(is.na(training)) == 0]
##Explore dataset
str(training) ##19622 observations of 60 variables

##Plot to see classe distribution to get an idea for cross validation
plot(training$classe)

##Getting 60% of training set to prepare for cross-validation
smp_size <- floor(0.6 * nrow(training))

##Set the seed to make your partition reproductible
set.seed(123)
##Separating training data for for cross validation purposes with this data set
divtrain <- sample(seq_len(nrow(training)), size = smp_size)
ttrain <- training[divtrain, ]
ttest <- training[-divtrain, ]

##Try rpart model
modelrpart<-train(classe~., data=ttrain, method="rpart")
##Do prediction and confusion matrix to see accuracy
pred<-predict(modelrpart, ttest)
cmrpart<-confusionMatrix(pred, ttest$classe)
##Accuracy is only 65.74%. Out of sample error is 1-accuracy (.3426) for predicitons
##made against the cross-validation set (ttest).

##Setting up limited verison of RandomForest method so computer can handle processing
fitControl<-trainControl(method = "none")
tgrid<- expand.grid(mtry=c(6)) 
test_rf<- train(classe ~ ., preProcess=c("center", "scale"), data = ttrain, method = "rf", trControl = fitControl, tuneGrid = tgrid)

##Do prediction and confusion matrix to see accuracy
predtry<-predict(test_rf, ttest)
cmrftry<-confusionMatrix(predtry, ttest$classe)
##100% accuracy! Out of sample error is 1-accuracy (.0000). However, this likely
##isn't accurate. There is probably some overfitting going on, so there will be
##some form of error with the test/new data.

##Download testing dataset 
if(!file.exists("./testing")){dir.create("./testing")}
fileURL<-"https://d396qusza40orc.cloudfront.net/predmachlearn/pml-testing.csv"
destfile<-"testing.csv"
download.file(fileURL, destfile=paste("testing", destfile, sep="/"))
dateDownloaded<-date()
dateDownloaded

##Adjust working directory
setwd("C:/Users/jdlevy/Documents/datasciencecoursera/08_PracticalMachineLearning/training/testing")

##Load/read activity data file
testing<-read.csv("testing.csv", na.strings=c("NA", "#DIV/0!", ""))
##Remove NAs
testing<-testing[,colSums(is.na(testing)) == 0]
##Explore dataset
summary(testing)
str(testing) ##19622 observations of 60 variables

##Using random forest model to predict classe values for test data
finalprediction<-predict(test_rf, testing)

##Write code to create files for each problem submission for assignment
pml_write_files = function(x){
  n = length(x)
  for(i in 1:n){
    filename = paste0("problem_id_",i,".txt")
    write.table(x[i],file=filename,quote=FALSE,row.names=FALSE,col.names=FALSE)
  }
}
pml_write_files(finalprediction)


library(knitr)
knitr::knit2html('PML Course Project.Rmd')


Help combined from:
  To start: https://rstudio-pubs-static.s3.amazonaws.com/29426_041c5ccb9a6a4bedb204e33144bb0ad4.html
Pick up here for prediction: http://rstudio-pubs-static.s3.amazonaws.com/25253_8c3f53920ddd4cfd93f03bf17d810a17.html
Extensive/many iterations: http://rstudio-pubs-static.s3.amazonaws.com/20380_01d51a675de747c0a60ab5967cdedade.html
