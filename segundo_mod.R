setwd('6to Semestre')
setwd('Temas Selectos de Matemáticas')
setwd('Covid')
suppressMessages(library(dplyr))
suppressMessages(library(verification))
cv=read.csv("covid.csv",header=T)
cv <- na.omit(cv)
cv=cv%>%tbl_df()
cv=cv%>%filter(as.numeric(RESULTADO)<=1)%>%filter(as.numeric(UCI)<=2)

p = cv %>% filter(UCI == 2) %>% summarize(p=n())  
num_negativo = p[[1]] #total de casos negativos en UCI
p = cv %>% filter(UCI == 1) %>% summarize(p=n())
num_positivos = p[[1]] #numero de casos positivos en UCI

cv_covid_negativo = cv %>% filter(UCI == 2) #numero de los que no necesitaron UCI(cuidado intensivos)
cv_covid_positivo = cv %>% filter(UCI == 1) #numero de los que necesitaron UCI(cuidado intensivos)
h = sample(1:num_negativo,num_positivos,replace = FALSE) #generamos numeros aleatorios
cv_int = cv_covid_negativo %>% slice(h)
cv <- bind_rows(cv_int,cv_covid_positivo)
cv # 50% contagiados 50% no contagiados, escogidos al azar los no contagiados


str(cv)
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
cv$FECHA_ACTUALIZACION <- as.Date(cv$FECHA_ACTUALIZACION )
cv$FECHA_INGRESO <- as.Date(cv$FECHA_INGRESO)
cv$FECHA_SINTOMAS <- as.Date(cv$FECHA_SINTOMAS)
cv$FECHA_ACTUALIZACION <- as.Date(cv$FECHA_ACTUALIZACION)
attach(cv)
str(cv)
index <- sample(1:5370, 2685, replace = F)
Train <- cv[index,]
Test <- cv[-index,]

lda1<- lda(UCI~ SEXO+NEUMONIA+OBESIDAD+HIPERTENSION+EDAD,family="binomial" ,data = Train)
summary(lda1)
  yhat1 <- as.integer(predict(lda1, Test)$class,type="response")
  yhat1
  y1=Test$UCI
  str(yhat1)
table(yhat1, Test$UCI)

mean(yhat1 == Test$UCI)

hist(yhat1,col = rgb(0,1,0,.5),main="reales vs predichos-lda-UCI")
hist(as.numeric(y1),add=T,col = rgb(1,0,0,.5))
####
glm1<- glm(UCI~ SEXO+NEUMONIA+OBESIDAD+EDAD+ASMA,family="binomial" ,data = Train)
summary(glm1)
yhat2<- predict(glm1, Test,type="response")

y=as.integer(yhat2>0.7)
y
y=ifelse(y =="0",1,2)
table(y, Test$UCI)
mean(y == Test$UCI)


hist(as.numeric(y),col = rgb(0,1,0,.5),main="reales vs predichos-log-UCI")
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
  reg = glm.c <-glm(UCI~ ENTIDAD_RES+SEXO+NEUMONIA+OBESIDAD+HIPERTENSION+EDAD,family="binomial" ,data = train)
  
  y = test$UCI
  yhat = predict(reg,test,type = "response")
  res = ifelse(yhat >0.5,1,0) #Si yhat > 0.5 entonces 1, si no 0
  
  clasiferror <- mean(y != res)
  
  acc[i] = 1 - clasiferror
}
mean(acc) #Presicion promedio del modelo

cat("Average Mean Square Error from kCV = ",round(mean(acc),1), "\n")
hist(acc, main = paste("Accuracy using ", k, "- fold CV"))
###########
