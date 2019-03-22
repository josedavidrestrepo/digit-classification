library(readr)
train<- read_csv("train.csv")
test <- read_csv("test.csv")

train_orig_labels <- train[, 1]
train_orig_labels <- as.factor(train_orig_labels$`5`)
summary(train_orig_labels)

library(randomForest)
numTrees <- 25
w<-train[-1]
startTime <- proc.time()

#Modelo
rf <- randomForest(train[-1], train_orig_labels, xtest=test[-1], 
                   ntree=numTrees)
proc.time() - startTime

rf$predicted
plot(rf, main="Número de arboles Vs error")
varImpPlot(rf)

