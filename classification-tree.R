library(RColorBrewer)
library(Rtsne)
library(rpart)
library(rpart.plot)
library(maptree)

### Leer Datos
digit<- read.csv("/Users/santiago/Downloads/mnist-in-csv/mnist_train.csv")
nrow(digit)


### Organizar Datos
digit <- digit[1:20000,]

# Dividir datos en una proporcion de 80:20 - entrenamiento:prueba
samp_size <-floor(0.80* nrow(digit))
entrenamiento_ind <-sample(seq_len(nrow(digit)), size = samp_size)

# Datos Entrenamiento
DATASET.entrenamiento <- as.data.frame(digit[entrenamiento_ind,])

# Datos Prueba
DATASET.prueba <-  as.data.frame(digit[-entrenamiento_ind,])


### Ver Datos
flip <- function(matrix){
  apply(matrix, 2, rev)
}

par(mfrow=c(3,3))
for (i in 1:27){
  dit <- flip(matrix(rev(as.numeric(DATASET.entrenamiento[i,-c(1, 786)])), nrow = 28)) #ver un digito
  image(dit, col = grey.colors(255))
}


### Exploración de Datos
barplot(table(DATASET.entrenamiento$label), main="Numero Total de Digitos (Conjunto de Entrenamiento)", col=brewer.pal(10,"Set1"),
        xlab="Numbers", ylab = "Frequencia de Numeros")

barplot(table(DATASET.prueba$label), main="Numero Total de Digitos (Conjunto de Prueba)", col=brewer.pal(10,"Set1"),
        xlab="Numbers", ylab = "Frequencia de Numeros")

tsne <- Rtsne(DATASET.entrenamiento[1:300,-1], dims = 2, perplexity=20, verbose=TRUE, max_iter = 500)

colors = rainbow(length(unique(DATASET.entrenamiento$label)))
names(colors) = unique(DATASET.entrenamiento$label)
plot(tsne$Y, t='n', main="tsne")
text(tsne$Y, labels=DATASET.entrenamiento$label, col=colors[DATASET.entrenamiento$label])


### Principal Component Analysis
caracteristicas<-digit[,-1]
pca<-princomp(caracteristicas)
std_dev <- pca[1:260]$sdev
pr_var <- std_dev^2
prop_varex <- pr_var/sum(pr_var)

plot(cumsum(prop_varex[1:260]), xlab = "Componente Principal",
     ylab = "Proporcioón Acumulativo de Varianza Explicado",
     type = "b")

nuevo_digito<-data.frame(number = digit[, "label"], pca$scores)
nuevo_digito<- nuevo_digito[,1:260]
samp_size <-floor(0.80* nrow(nuevo_digito))
entrenamiento_ind <-sample(seq_len(nrow(nuevo_digito)), size = samp_size)
entrenamiento_set <- nuevo_digito[entrenamiento_ind,]
prueba_set  <-nuevo_digito[-entrenamiento_ind,]

# RPart
pc <- proc.time()
model.rpart <- rpart(entrenamiento_set$number ~ .,method = "class", data = entrenamiento_set)
proc.time() - pc

printcp(model.rpart)


### Precisión - RPart
prediction.rpart <- predict(model.rpart, newdata = prueba_set, type = "class")
table(`Clase Real` = prueba_set$number, `Clase Predicha` = prediction.rpart)

error.rate.rpart <- sum(prueba_set$number != prediction.rpart)/nrow(prueba_set)
precision <- round((1 - error.rate.rpart) *100,2)
precision


### Visualizaciones de Arboles
heat.tree <- function(tree, low.is.green=FALSE, ...) { # dots args passed to prp
  y <- model.rpart$frame$yval
  if(low.is.green)
    y <- -y
  max <- max(y)
  min <- min(y)
  cols <- rainbow(99, end=.36)[
    ifelse(y > y[1], (y-y[1]) * (99-50) / (max-y[1]) + 50,
           (y-min) * (50-1) / (y[1]-min) + 1)]
  prp(model.rpart, branch.col=brewer.pal(10,"Set3"), box.col=brewer.pal(10,"Set3"), ...)
}

heat.tree(model.rpart, type=4, varlen=0, faclen=0, fallen.leaves=TRUE)

draw.tree(model.rpart, cex = 0.5, nodeinfo = TRUE, col = gray(0:8/8))

prp(model.rpart, extra=6, main="Clasificacion (RPART). Arbol de Reconocimiento de Digitos",
    box.col=brewer.pal(10,"Set3")[model.rpart$frame$yval])