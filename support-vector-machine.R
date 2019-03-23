require(ggplot2)
require(readr)
library(caret) 
library(RColorBrewer)
require(e1071)

#CARGA DE DATOS
setwd("~/ANGIE/Estadistica/TAE/trabajo 4")
train <- read.csv("train.csv") #entrenamiento
test <- read.csv("test.csv") #validacion
train <- as.data.frame(train)

dim(train) ; dim(test) #dimension de los datos
train[, 1] <- as.factor(train$label) #pasar las etiquetas a factor
head(sapply(train[1,], class)) #verificando la clase

#eliminando las columnas que contienen cero en todas las observaciones
train_orig <- train
test_orig <- test
nzv.data <- nearZeroVar(train, saveMetrics = TRUE) #col con varianza cercana a cero
drop.cols <- rownames(nzv.data)[nzv.data$nzv == TRUE] #eliminando col con valores == 0
train <- train[,!names(train) %in% drop.cols]
test <- test[,!names(test) %in% drop.cols]

## ANALISIS EXPLORATORIO


BNW <- c("white", "black")
CUSTOM_BNW <- colorRampPalette(colors = BNW)

par(mfrow = c(4, 3), pty = "s", mar = c(1, 1, 1, 1), xaxt = "n", yaxt = "n")
images_digits_0_9 <- array(dim = c(10, 28 * 28))
for (digit in 0:9) {
  images_digits_0_9[digit + 1, ] <- apply(train_orig[train_orig[, 1] == digit, -1], 2, sum)
  images_digits_0_9[digit + 1, ] <- images_digits_0_9[digit + 1, ]/max(images_digits_0_9[digit + 1, ]) * 255
  z <- array(images_digits_0_9[digit + 1, ], dim = c(28, 28))
  z <- z[, 28:1]
  image(1:28, 1:28, z, main = digit, col = CUSTOM_BNW(256))
}

#se observa que los numeros 9, 4 y 1 son muy borrosos
#lo cual puede llevar a malos pronosticos

CUSTOM_BNW_PLOT <- colorRampPalette(brewer.pal(10, "Set3"))
LabTable <- table(train_orig$label)
par(mfrow = c(1, 1))
percentage <- round(LabTable/sum(LabTable) * 100)
labels <- paste0(row.names(LabTable), " (", percentage, "%) ")
b<-barplot(LabTable,ylim=c(0,6000), col = CUSTOM_BNW_PLOT(10), main = "Porcentaje de digitos (Entrenamiento)")
text(x=b, y=LabTable, pos=3, cex=0.8, col="black",
     label=labels)
#se obserba que todos los digitos contribuyen de igual forma al conjunto
#de datos de entrenamiento


#partiendo para entrenar 
set.seed(43210)
trainIndex <- createDataPartition(train$label, p = 0.1, list = FALSE, times = 1)
allindices <- c(1:42000)
training <- train[trainIndex,]
validating <- train[-trainIndex,]
vali0_index <- allindices[! allindices %in% trainIndex]
validIndex <- createDataPartition(validating$label, p = 0.11, list = FALSE, times = 1)
validating <- validating[validIndex,]
original_validindex <- vali0_index[validIndex]


########## MODELO
modelo_svm <- svm(label~., data = training, kernel = "linear", cost = 10, 
                  scale = FALSE)
summary(modelo_svm)

#para ver que tan bien clasifica
SVMRadial_predict1 <- as.numeric(predict(modelo_svm,newdata = validating))-1
as.factor(SVMRadial_predict1)
confusionMatrix(as.factor(SVMRadial_predict1), validating$label)
