library(readr)
train <- read_csv("train.csv")
test <- read_csv("test.csv")

niveles <- train[, 1]
niveles <- as.factor(niveles$`5`)
summary(niveles)

#MODELO
library(nnet)
modelo<-multinom(niveles~., family="multinomial", data=train,
                 MaxNWts =10000000, maxit=50)

matriz.conf<-table(predict(modelo),train$`5`)
matriz.conf
sum(diag(matriz.conf))/sum(matriz.conf)
1-sum(diag(matriz.conf))/sum(matriz.conf)

PseudoR2(modelo,which="McFadden")
PseudoR2(modelo,which="CoxSnell")
PseudoR2(modelo,which="Nagelkerke")

#curva ROC

pred_link<-predict(modelo)
pred_ord<-sort(pred_link,index.return=T,type="response")
pred<-as.numeric(pred_ord)
orden<-sort(train$`5`)
ord<-as.numeric(orden)
modelo_auc<-multiclass.roc(ord~pred)
print(auc(modelo_auc))
rs<-modelo_auc$rocs
plot.roc(rs[[1]])
sapply(2:length(rs),function(i)lines.roc(rs[[i]],col=i))
