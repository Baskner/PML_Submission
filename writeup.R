library(caret)
train <- read.csv(url("https://d396qusza40orc.cloudfront.net/predmachlearn/pml-training.csv"))
test <- read.csv(url("https://d396qusza40orc.cloudfront.net/predmachlearn/pml-testing.csv"))

n <- names(Filter(function(x)!all(is.na(x)), test))
n <- Filter(function(x)!(x ==  "X"), n)
n <- Filter(function(x)!(x ==  "user_name"), n)
n <- Filter(function(x)!(x ==  "raw_timestamp_part_1"), n)
n <- Filter(function(x)!(x ==  "raw_timestamp_part_2"), n)
n <- Filter(function(x)!(x ==  "cvtd_timestamp"), n)
n <- Filter(function(x)!(x ==  "new_window"), n)
n <- Filter(function(x)!(x ==  "num_window"), n)
n <- Filter(function(x)!(x ==  "problem_id"), n)

train$classe <- as.factor(trainD$classe)

form <- paste("classe ~ ", paste(n, collapse="+"))

trainD <- train[, c("classe", n)]
testD <- test[, n]

folds <- createFolds(trainD$classe, k = 3, list = TRUE, returnTrain = FALSE)

r <- folds$Fold3
rf <-  train(trainD[-r,n], trainD[-r,"classe"], method="rf", preProcess=c("center", "scale"))
confusionMatrix(predict(rf, trainD[r,n]), trainD[r,"classe"])

samples <- 0
erros <- 0

for(r in folds) 
{
  rf <- train(trainD[-r,n], trainD[-r,"classe"], method="rf", preProcess=c("center", "scale"))
  folds$r.confM <- confusionMatrix(predict(rf, trainD[r,n]), trainD[r,"classe"])

  m <- predict(rf, trainD[r,n]) == trainD[r,"classe"]
  samples <- samples + length(m)
  errors <- errors + length(m) + sum(m)
}

out_of_sample_error <- errors / samples

rf <- train(trainD[, n], trainD[, "classe"], method="rf", preProcess=c("center", "scale"))
confM <- confusionMatrix(predict(rf, trainD[, n]), trainD[, "classe"])

rf$final

answers <- predict(rf, testD)

pml_write_files = function(x){
  n = length(x)
  for(i in 1:n){
    filename = paste0("problem_id_",i,".txt")
    write.table(x[i],file=filename,quote=FALSE,row.names=FALSE,col.names=FALSE)
  }
}

pml_write_files(answers)