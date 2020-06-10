#setwd('6to Semestre')
#setwd('Temas Selectos de Matemáticas')
#setwd('Covid')
suppressMessages(library(dplyr))
suppressMessages(library(verification))
suppressMessages(library(class))
library(tree)
setwd("C:/Users/sole-/Documents/Tecnologia/Sexto semestre/Temas compu/knn")
cv=read.csv("covid.csv",header=T)
cv <- na.omit(cv)
cv=cv%>%tbl_df()
cv=cv%>%filter(as.numeric(RESULTADO)<=1)
cv$FECHA_DEF = ifelse(cv$FECHA_DEF !="9999-99-99",1,0)

p = cv %>% filter(FECHA_DEF == 0) %>% summarize(p=n())  
num_negativo = p[[1]] #total de casos negativos
p = cv %>% filter(FECHA_DEF == 1) %>% summarize(p=n())
num_positivos = p[[1]] #numero de casos positivos

cv_covid_negativo = cv %>% filter(FECHA_DEF == 0)
cv_covid_positivo = cv %>% filter(FECHA_DEF == 1)
h = sample(1:num_negativo,num_positivos,replace = FALSE) #generamos numeros aleatorios
cv_int = cv_covid_negativo %>% slice(h)
cv <- bind_rows(cv_int,cv_covid_positivo)#50% que han muerto y 50% que estan vivos

str(cv)
cv$FECHA_DEF <- as.factor(cv$FECHA_DEF)
cv$ORIGEN <- as.factor(cv$ORIGEN)
cv$SECTOR <- as.factor(cv$SECTOR)
cv$ENTIDAD_UM<- as.factor(cv$ENTIDAD_UM)
cv$SEXO <- as.factor(cv$SEXO)
cv$ENTIDAD_NAC <- as.factor(cv$ENTIDAD_NAC)
cv$ENTIDAD_RES <- as.factor(cv$ENTIDAD_RES)
cv$MUNICIPIO_RES <- as.factor(cv$MUNICIPIO_RES)
cv$TIPO_PACIENTE <- as.factor(cv$TIPO_PACIENTE)
cv$INTUBADO <- as.factor(cv$INTUBADO )
cv$NEUMONIA  <- as.factor(cv$NEUMONIA )
cv$NACIONALIDAD <- as.factor(cv$NACIONALIDAD)
cv$EMBARAZO <- as.factor(cv$EMBARAZO)
cv$HABLA_LENGUA_INDI <- as.factor(cv$HABLA_LENGUA_INDIG)
cv$DIABETES <- as.factor(cv$DIABETES)
cv$EPOC <- as.factor(cv$EPOC)
cv$ASMA <- as.factor(cv$ASMA)
cv$INMUSUPR  <- as.factor(cv$INMUSUPR  )
cv$HIPERTENSION   <- as.factor(cv$HIPERTENSION  )
cv$OTRA_CON <- as.factor(cv$OTRA_COM)
cv$CARDIOVASCULAR <- as.factor(cv$CARDIOVASCULAR)
cv$OBESIDAD  <- as.factor(cv$OBESIDAD )
cv$RENAL_CRONICA  <- as.factor(cv$RENAL_CRONICA )
cv$TABAQUISMO <- as.factor(cv$TABAQUISMO)
cv$OTRO_CASO  <- as.factor(cv$OTRO_CASO  )
cv$RESULTADO    <- as.factor(cv$RESULTADO  )
cv$MIGRANTE <- as.factor(cv$MIGRANTE)
cv$UCI <- as.factor(cv$UCI)
cv$FECHA_DEF = as.factor(cv$FECHA_DEF)
attach(cv)
str(cv)
index <- sample(1:dim(cv), (dim(cv)/2), replace = F)
Train <- cv[index,]
Test <- cv[-index,]

##########LOGISTIC######
  glm1<- glm(FECHA_DEF~ SEXO+NEUMONIA+OBESIDAD+HIPERTENSION+DIABETES+EDAD+INTUBADO+UCI+EPOC+OTRO_CASO+OTRA_CON,family="binomial" ,data = Train)
  summary(glm1)
  yhat2<- predict(glm1, Test,type="response")
  #para elegir la mejor y evaluamos los posibles valores, desde 0.1 hasta 1 y vemos cual es el mean menor 
  #se repite 91 veces 
  size=1
  val_mean <- rep(0,91)
  for(i in seq(0.1,1,by=0.01)){
    y=as.integer(yhat2>i)
    y=ifelse(y =="0",0,1)
    val_mean[size]=mean(y == Test$FECHA_DEF)
    size=size+1
  }
  size=2
  lugar=1
  mejor=val_mean[lugar]
  for(i in seq(0.11,1,by=0.01)){
    if(val_mean[size]>mejor){
      mejor=val_mean[size]
      b=i
      lugar=size
    }
    size=size+1
  }
  y=seq(0.1,1,by=0.01)
  plot(y,val_mean,xlab = "iterador", ylab = "valor mean")
  points(b, val_mean[lugar],  col = "orange", lwd = 10)
 ###valores elegidos
   b
  val_mean[lugar]
####ROCPLOT

  roc= as.numeric(Test$FECHA_DEF)
 
  roc.plot(x = roc, pred =yhat2,
           threshold = seq(0, max(yhat2), 0.05),
           plot.thres = c(0.03, 0.05, 0.1, 0.5, 0.9), main="Logistic")
  
 length(Test$FECHA_DEF)
 length(yhat2)
#################### k-folds
n <- dim(cv)[1]
k <- 10


folds <- cut(1:n,k,labels = F)
acc = rep(0,k) #presicion del modelo
for (i in 1:k){
  index = folds == i
  test = cv[index,]
  train = cv[-index,]
  reg = glm.c <-glm(FECHA_DEF~ SEXO+NEUMONIA+OBESIDAD+HIPERTENSION+DIABETES+EDAD+INTUBADO+UCI+EPOC+OTRO_CASO+OTRA_CON,family="binomial" ,data = train)
  
  y = test$FECHA_DEF
  yhat = predict(reg,test,type = "response")
  res = ifelse(yhat >b,1,0) #Si yhat > 0.5 entonces 1, si no 0
  
  clasiferror <- mean(y != res)
  
  acc[i] = 1 - clasiferror
}
mean(acc) #Presicion promedio del modelo

hist(acc, main = paste("Accuracy using ", k, "- fold CV"))
boxplot(acc,main="precisión")


#######LDA#####
lda1 <- lda(FECHA_DEF~ SEXO+NEUMONIA+OBESIDAD+HIPERTENSION+DIABETES+EDAD+INTUBADO+UCI+EPOC+OTRO_CASO+OTRA_CON,family="binomial" ,data = Train)
lda1
yhat1 <- predict(lda1, Test)$class
yhat1

table(yhat1,Test$FECHA_DEF)
mean(yhat1 == Test$FECHA_DEF)
yy=predict(lda1, Test)
roc.plot(x = as.numeric(Test$RESULTADO), pred =yy$posterior[,2],
         threshold = seq(0, max(as.numeric(yy$posterior[,2])), 0.05),
         plot.thres = c(0.03, 0.05, 0.1, 0.5, 0.9),main="LDA")
######kfolds
n <- dim(cv)[1]
k <- 10


folds <- cut(1:n,k,labels = F)
acc = rep(0,k) #presicion del modelo
for (i in 1:k){
  index = folds == i
  test = cv[index,]
  train = cv[-index,]
  reg = lda(FECHA_DEF~ SEXO+NEUMONIA+OBESIDAD+HIPERTENSION+DIABETES+EDAD+INTUBADO+UCI+EPOC+OTRO_CASO+OTRA_CON,family="binomial" ,data = train)
  
  y = test$FECHA_DEF
  res = predict(reg, test)$class #Si yhat > 0.5 entonces 1, si no 0
  
  clasiferror <- mean(y != res)
  
  acc[i] = 1 - clasiferror
}
mean(acc) #Presicion promedio del modelo

hist(acc, main = paste("Accuracy using ", k, "- fold CV"))
boxplot(acc,main="LDA",xlabel="Precisión")

#############KNN#####
end= nrow(cv_covid_positivo)#numero de valores
mayor=0
for(n in 1:20){
  knn.pred <- knn(cbind(Train$SEXO,Train$NEUMONIA,Train$OBESIDAD,Train$HIPERTENSION,Train$DIABETES,
                        Train$EDAD,Train$INTUBADO,Train$UCI,Train$EPOC,Train$OTRO_CASO,Train$OTRA_CON), 
                  cbind(Test$SEXO,Test$NEUMONIA,Test$OBESIDAD,Test$HIPERTENSION,Test$DIABETES,
                        Test$EDAD,Test$INTUBADO,Test$UCI,Test$EPOC,Test$OTRO_CASO,Test$OTRA_CON), 
                  Train$FECHA_DEF, k=n)
  error= mean(Test$FECHA_DEF == knn.pred)
  if(error>mayor){
    mayor=error
    k_best=n
  }
}
print("la mejor k fue: ")
k_best
mayor   
######## desicion tree####
tree.muerte = tree(FECHA_DEF~ SEXO+NEUMONIA+OBESIDAD+HIPERTENSION+DIABETES+EDAD+INTUBADO+UCI+EPOC+OTRO_CASO+OTRA_CON,data = Train)
summary(tree.muerte)
plot(tree.muerte)
text(tree.muerte, pretty=0)
tree.pred = predict(tree.muerte, Test, type="class")
table(tree.pred,Test$FECHA_DEF)
mean(tree.pred==Test$FECHA_DEF)

cv.tree.muerte = cv.tree(tree.muerte, FUN = prune.misclass)
names(cv.tree.muerte)
cv.tree.muerte
plot(cv.tree.muerte$size, cv.tree.muerte$dev,type="b")
plot(cv.tree.muerte$k,cv.tree.muerte$dev,type="b")
