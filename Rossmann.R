
library(ggplot2)
library(lubridate)
library(gridExtra)
library(utils)
library(plyr)
library(dplyr)
library(tree)
library(randomForest)
#load("~/Pap/RossMan/RM.RData")

#Datos descriptivos de Ventas.
summary(RossMan$Sales)

#Histograma de Ventas
png("Histograma de Ventas(1).png")
qplot(RossMan$Sales, fill = ..count..) +
  theme_minimal() +
  xlab("Ventas") +
  ylab("Volumen d?a cualquiera") +
  theme(legend.position = "none") +
  scale_fill_gradient("Count", low = "green", high = "red") +
  xlim(0, 23500)
dev.off()

#Volumen de ventas en un dia cualquiera
png("Histograma de Ventas(2).png")
ggplot(RossMan, aes(Sales, fill=..count..))+ 
  geom_histogram(binwidth = 1) +
  xlab("Ventas") +
  ylab("Volumen d?a cualquiera") +
  scale_fill_gradient("Count", low = "green", high = "red") + 
  theme_minimal()+
  xlim(0,20000)
dev.off()

##Volumen de Ventas por tipo de tienda.
png("Histograma de Ventas por Tipo de Tienda.png")
ggplot(RossMan, aes(Sales,fill=StoreType)) +
  geom_histogram(binwidth = 1) +
  theme_minimal() +
  xlab("Ventas") +
  ylab("Volumen d?a cualquiera") +
  theme_minimal()+
  xlim(0,20000)
dev.off()


#Analisis Descriptivo por Tipo de Tienda
sapply(split(RossMan$Sales, RossMan$StoreType), summary)

#Analisis Descriptivo por Tipo de Venta Boxplot
png("Boxplot Tipo de Venta.png")
ggplot(data = RossMan,aes(x = StoreType, y = Sales, fill = StoreType)) +
  geom_boxplot() + xlab("Tipo de Tienda") +
  ylab("Ventas") +
  theme(legend.position = "none") 
dev.off()


#Analisis Descriptivo por Numero de Clientes
summary(RossMan$Customers)

png("Nivel de Ventas por A?os.png")
qplot(as.Date(Date), Customers, data=RossMan, geom=c("smooth"))+
  xlab("Fecha") + ylab("Ventas")
dev.off()

#Analisis Descriptivo por Numero de Clientes
png("Boxplot por Tipo de Tienda.png")
ggplot(data = RossMan, aes(x = StoreType,y = Sales,
                          fill = month(RossMan$Date, label = TRUE))) +
  geom_boxplot() +
  theme_minimal() +
  xlab("Tipo de Tienda") +
  ylab("Ventas") +
  theme(legend.position = "none") 
dev.off()

#Mes por Compras promediales
RossMan$Mes <- month(RossMan$Date, label = TRUE)
ventas.Mth  <- ddply(.data = RossMan, .variables = c("Mes"), summarise, 
                    Mean = mean(Sales))
names(ventas.Mth) <- c("Month","Mean")

##Grafica. Ventas por Mes
png("Ventas por Mes.png")
qplot(Month,Mean, data=ventas.Mth, geom=c("point"), method="lm")
dev.off()


#Competition Distance
Comp.Ventas <- ddply(.data = RossMan,
                    .variables = c("CompetitionDistance"),
                    summarise,
                    Average = mean(Sales))
names(Comp.Ventas) <- c("CompetitionDistance","Mean")
Comp.Ventas <- na.omit(Comp.Ventas)

png("Nivel de Ventas por Distancia del Competidor.png")
qplot(CompetitionDistance,Mean, data=Comp.Ventas, geom = c("point", "step"))
dev.off()


#####Prediccion############

load("~/Pap/Rossman/RM.RData")
set.seed(1)
RMSPE<- c(1:4)
names(RMSPE) <- c("lm", "tree", "random forest", "boosting tree")
RossMan <- select(RossMan, DayOfWeek, Sales, Promo, Promo2, StateHoliday, SchoolHoliday,
                  Assortment, CompetitionDistance, Mes, StoreType)
RossMan <- na.omit(RossMan)
#RossMan$ddSales <- scale(RossMan$Sales)

#Conjuntos de entrenaiento
#######Selecci?on del mejor modelo############
#lm
train <- sample(1:nrow(RossMan), nrow(RossMan)*0.8)
RossMan.train <- RossMan[train, ]
RossMan.test <- RossMan[-train, ]
lm.RossMan <- lm(Sales~.,RossMan.train)
lm.pred <- predict(lm.RossMan,RossMan.test)
RMSPE[1] <- sqrt(mean((RossMan.test$Sales - lm.pred)^2))
names(RMSPE)[1] = 'lm'

#Arbol 
tree.RossMan <- tree(Sales~., RossMan.train)
Pred.tree <- predict(tree.RossMan,RossMan.test)
RMSPE[2] <- sqrt(mean((RossMan.test$Sales - Pred.tree)^2))
names(RMSPE)[2] = 'tree'

#Random Forest
#Se reduce el numero de la muestra
N.train <- sample(1:nrow(RossMan), nrow(RossMan)*0.02)
RossMan.N.train <- RossMan[N.train,]
RossMan.N.test <- RossMan[-N.train,]
Percentage.Error <- data.frame()
nmtry <- c(2,3,4)
ntrees <- c(200,300,400)
for(j in 1:length(nmtry)){
  for(i in 1:length(ntrees)){
    rf.RossMan <- randomForest(Sales~.,data = RossMan.N.train,mtry = nmtry[j],
                               ntree = ntrees[i],importance = TRUE)
    info.random <- predict(rf.RossMan,RossMan.N.test)
    Percentage.Error[j,i]<-
      sqrt(mean((info.random - RossMan.N.test$Sales)^2))
    Percentage.Error
    print(ntrees[i])
  }
  print(nmtry[j])
}
which(Percentage.Error == min(Percentage.Error), arr.ind = TRUE)
RMSPE[3] <- min(na.omit(Percentage.Error))
names(RMSPE)[3] = 'randomForest'

#Boosting
library(gbm)
ntrees <- c(1000,2000,3000)
shrinkage <- c(0.01,0.05,0.1)
nmtry <- c(2,3,4)
error.gbm.tree <- list() #uneeded list, can be used a 3x3x3 matrix
error.gbm <- data.frame()
for(k in 1:length(nmtry)){
  for (j in 1:length(shrinkage)){
    for(i in 1:length(ntrees)){
      boost.RossMan <- gbm(Sales~.,
                           data = RossMan.N.train,
                           distribution = "gaussian",
                           n.trees = ntrees[i],
                           interaction.depth = nmtry[k],
                           shrinkage = shrinkage[j],
                           verbose = F)
      info.gbm <- predict(boost.RossMan,RossMan.N.test,
                          n.trees = ntrees[i])
      error.gbm[j,i] <- sqrt(mean((info.gbm - RossMan.N.test$Sales)^2))
    }
    print(j)
  }
  error.gbm.tree[[k]] <- error.gbm
  error.gbm <- data.frame()
  print(k)
}
#Encontrando las variables que dieron mejores resultados
#mtry
mtryarr <- which(sapply(error.gbm.tree,min)==
                   min(sapply(error.gbm.tree,min)))
nmtryOpt <- nmtry[mtryarr]
#Encontramos los ?indices del mejor mtry
optimal <- which(min(error.gbm.tree[[mtryarr]])== error.gbm.tree[[mtryarr]],arr.ind = TRUE)

#Mejor cantidad de ?arboles
treeOpt <- ntrees[optimal[2]]
#Mejor lambda
shrinkageOpt <- shrinkage[optimal[1]]
#Calculamos el mejor modelo
boost.RossMan <- gbm(Sales~.,
                     data = RossMan.N.train,
                     distribution = "gaussian",
                     n.trees = treeOpt,
                     interaction.depth = nmtryOpt,
                     shrinkage = shrinkageOpt,
                     verbose = F)

info.gbm <- predict(boost.RossMan,RossMan.N.test,n.trees = treeOpt)
RMSPE[4] <- sqrt(mean((info.gbm -RossMan.N.test$Sales)^2))
names(RMSPE)[4] = 'boostedTrees'

#Mejor modelo
min(RMSPE) #boosting!!!

#######################################
###### booted trees devastation! ######
library(gbm)
ntrees <- c(1000, 10000)
shrinkage <- c(0.01, 0.1)
intdepth <- c(1, 4)
bag_f <- c(.5, .75, .9)
cv_folds <- 5

array_error_boosting <- array(data = NA, dim = c(2, 2, 2, 3), 
                              dimnames = list(ntrees, shrinkage, intdepth, bag_f))

for(bag in bag_f){
  for(int in intdepth){
    for(shr in shrinkage){
      for(tre in ntrees){
        
        error_array <- array(data = NA, dim = c(dim(RossMan.N.test)[1], cv_folds))
        for(cv in 1:cv_folds){
          tree_object <- gbm(Sales~.,
                             data = RossMan.N.train,
                             distribution = "gaussian",
                             n.trees = tre,
                             interaction.depth = int,
                             shrinkage = shr,
                             bag.fraction = bag,
                             n.minobsinnode = 15,
                             verbose = T)
          
          error_array[, cv] = predict(tree_object, RossMan.N.test,
                                       n.trees = tre)
        }
        error_vector = apply(error_array, MARGIN = 1, FUN = mean)
        
        array_error_boosting[paste(tre), paste(shr), paste(int), paste(bag)] <- 
          sqrt(mean((error_vector - RossMan.N.test$Sales)^2))
        
      }
    }
  }
}
dimnames(array_error_boosting) = list(paste('tree', ntrees), paste('shrink', shrinkage), 
                                      paste('depth', intdepth), paste('fraction', bag_f))
#mejor combinacion
optimal_dims <- which(min(array_error_boosting) == array_error_boosting, arr.ind = T)

paste(ntrees[optimal_dims[1]], 'trees',
      shrinkage[optimal_dims[2]], '% shrinkage', 
      intdepth[optimal_dims[3]], 'int_depth', 
      bag_f[optimal_dims[4]], '% fraction of bagging')









