library(keras)
setwd("C:/Users/helsa/Downloads/MNIST")
mnist <- dataset_mnist()
str(mnist)

# Lectura
trainx <- mnist$train$x
trainy <- mnist$train$y
testx <- mnist$test$x
testy <- mnist$test$y

table(mnist$train$y, mnist$train$y)
table(mnist$test$y, mnist$test$y)

# Reescalar y combinar
trainx <- array_reshape(trainx, c(nrow(trainx), 784))
testx <- array_reshape(testx, c(nrow(testx), 784))
trainx <- trainx / 255
testx <- testx /255
hist(trainx[1,])

trainy <- to_categorical(trainy, 10)
testy <- to_categorical(testy, 10)
head(trainy)

# Modelo
model <- keras_model_sequential()
model %>% 
  layer_dense(units = 512, activation = 'relu', input_shape = c(784)) %>% 
  layer_dropout(rate = 0.4) %>% 
  layer_dense(units= 256, activation = 'relu') %>% 
  layer_dropout(rate = 0.3) %>% 
  layer_dense(units = 10, activation = 'softmax')
summary(model)

# Compilar
model %>% 
  compile(loss = 'categorical_crossentropy',
          optimizer = optimizer_rmsprop(),
          metrics = 'accuracy')

# Evaluación del modelo

model %>% evaluate(trainx, trainy)
pred <- model %>% predict_classes(trainx)
a1<-table(Predicted = pred, Actual = mnist$train$y)





#Adaptado de Bharatendra Rai. Image Recognition with MNIST Data in R 
