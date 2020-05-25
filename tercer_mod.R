setwd('6to Semestre')
setwd('Temas Selectos de Matem�ticas')
setwd('Covid')
suppressMessages(library(dplyr))
suppressMessages(library(verification))
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
index <- sample(1:13842, 6921, replace = F)
Train <- cv[index,]
Test <- cv[-index,]
y1=Test$FECHA_DEF
lda1<- lda(FECHA_DEF~ SEXO+NEUMONIA+OBESIDAD+HIPERTENSION+EDAD,family="binomial" ,data = Train)
yhat1 <- as.integer(predict(lda1, Test)$class,type="response")
yhat1
  fix(yhat1)
  str(yhat1)
table(yhat1, Test$FECHA_DEF)
mean(yhat1 == Test$FECHA_DEF)
####
glm1<- glm(FECHA_DEF~ SEXO+NEUMONIA+OBESIDAD+HIPERTENSION+DIABETES+EDAD,family="binomial" ,data = Train)
summary(glm1)
yhat2<- predict(glm1, Test,type="response")

y=as.integer(yhat2>0.5)
y=as.factor(y)
y
table(as.integer(yhat2>0.5), Test$FECHA_DEF)
mean(as.integer(yhat2>0.5) == Test$FECHA_DEF)
as.factor(y)
hist(as.numeric(y),col = rgb(0,1,0,.5),main="reales vs predichos")
hist(as.numeric(y1),add=T,col = rgb(1,0,0,.5))
#################### k-folds
n <- dim(cv)[1]
k <- 20


folds <- cut(1:n,k,labels = F)
acc = rep(0,k) #presicion del modelo
for (i in 1:k){
  index = folds == i
  test = cv[index,]
  train = cv[-index,]
  reg = glm.c <-glm(FECHA_DEF~SEXO+NEUMONIA+OBESIDAD+HIPERTENSION+EDAD,family="binomial" ,data = train)
  
  y = test$FECHA_DEF
  yhat = predict(reg,test,type = "response")
  res = ifelse(yhat >0.5,1,0) #Si yhat > 0.5 entonces 1, si no 0
  
  clasiferror <- mean(y != res)
  
  acc[i] = 1 - clasiferror
}
mean(acc) #Presicion promedio del modelo

cat("Average Mean Square Error from kCV = ",round(mean(acc),1), "\n")
hist(acc, main = paste("Accuracy using ", k, "- fold CV"))