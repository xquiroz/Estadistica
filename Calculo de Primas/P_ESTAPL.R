
library(survival)
?colon
View(colon)
dim(colon)


CL <- na.omit(colon)
impar <- seq( from= 1, to = 1776, by = 2 )
CL <- CL[impar,]
CL <- na.omit(CL)
CL <- CL[,!names(CL) %in% c("etype", "study", "id", "nodes")]
dim(CL)
# Filas 888, Columnas 14
View(CL)

#Recategorizando variable edad 
summary(CL$age)
e<- rep("adulto mayor", length(CL$age))
e[CL$age> 18 & CL$age<30] <- "joven"#menor de 29 
e[CL$age>= 30 & CL$age<60 ] <- "adulto"
e
CL$age <- e
View(CL)

########## Descriptiva ##########

# Analisis descriptivo de los datos (tablas, histogramas, etc.) 
summary(CL)
names(CL)

# Estado de supervivencia (status)
table(CL$status)
# ( 0 = censurado , 1 = fallecido).

# Tabla de Status/rx
table(CL$status,CL$rx) # Tipo de Tratamiento
#Obs(ervación), Lev(amisol), Lev(amisol)+5-FU

#    Obs Lev Lev+5FU
# 0  141 145     172
# 1  164 149     117

# Tabla de Status/Sex
table(CL$status,CL$sex) # 0 = Mujer, 1 = Hombre
 
#     0   1
# 0  220 238
# 1  208 222

# Tabla de Status/age
table(CL$status,CL$age) #Cantidad de pacientes segun la edad
# De 18 a 85 aÃ±os
#    adulto adulto mayor joven
#  0    209          245     4
 # 1    176          251     3

# Tabla de Status/obstruct
table(CL$status,CL$obstruct) #ObstrucciÃ³n en el colon por un tumor
# 0 = Sin obstruccion , 1 = Obstruccion 

#     0   1
# 0  379  79
# 1  338  92

# Tabla de Status/Perfor
table(CL$status,CL$perfor) #PerforaciÃ³n en el colon
# 0 = Sin perforacion , 1 = PerforaciÃ³n

#     0    1
# 0  446  12
# 1  415  15

# Tabla de Status/Adhere
table(CL$status,CL$adhere) #Adherencia a organos cercanos
# 0 = Sin Adherencia , 1 = Con Adherencia 

#     0   1
# 0  404  54
# 1  356  74

# Tabla de Status/Differ
table(CL$status,CL$differ) #DiferenciaciÃ³n del tumor
# 1 = buena, 2 = moderada, 3 = pobre 

#      1   2   3
# 0   49 347  62
# 1   41 306  83

# Tabla de Status/Extent
table(CL$status,CL$extent) # ExtensiÃ³n de la propagaciÃ³n local 
# 1 = submucosa, 2 = mÃºsculo, 3 = serosa, 4 = estructuras contiguas
 
#      1   2   3   4
#  0  16  67 362  13
#  1   3  35 368  24

# Tabla de Status/Surg
table(CL$status,CL$surg) # Tiempo desde la cirugÃ­a hasta el registro
# 0 = corto, 1 = largo 

#     0   1
# 0  349 109
# 1  301 129

# Tabla de Status/node4
table(CL$status,CL$node4) #MÃ¡s de 4 ganglios linfÃ¡ticos positivos 
# 1 = Mas de 4, 0 = Menos de 4

#     0   1
# 0  390  68
# 1  263 167


CL
# 0 = censurado, 1 = fallecido

# Tiempos promedio para Colon
#promedio tiempo de eventos por categoria de variable 
mean(CL$time[CL$status==1])# Promedio de dÃ­as de fallecimientos  
mean(CL$time[CL$status==0])# Promedio de dÃ­as de censurados 

mean(CL$time[CL$status==1 & CL$rx == "Obs" ])#promedio de dias de fallecimientos con el tratamiento de Obs 
mean(CL$time[CL$status==1 & CL$rx == "Lev" ])#promedio de dias de fallecimientos con el tratamiento de Lev
mean(CL$time[CL$status==1 & CL$rx == "Lev+5FU" ])#promedio de dias de fallecimientos con el tratamiento de Obs 
mean(CL$time[CL$status==0 & CL$rx == "Obs" ])#promedio de dias de censurados con el tratamiento de Obs 
mean(CL$time[CL$status==0 & CL$rx == "Lev" ])#promedio de dias de censurados con el tratamiento de Lev
mean(CL$time[CL$status==0 & CL$rx == "Lev+5FU" ])#promedio de dias de censurados con el tratamiento de Obs 

mean(CL$time[CL$status==1 & CL$sex == 1]) #Promedio en dÃ­as de fallecimientos en hombres
mean(CL$time[CL$status==1 & CL$sex == 0]) #Promedio de dÃ­as de fallecimientos en mujeres
mean(CL$time[CL$status==0 & CL$sex == 1]) #Promedio de dÃ­as de censura en hombres
mean(CL$time[CL$status==0 & CL$sex == 0]) #Promedio de dÃ­as de censura en mujeres

mean(CL$time[CL$status==1 & CL$age == "adulto mayor" ])#promedio de dias de fallecimientos en adultos mayores
mean(CL$time[CL$status==1 & CL$age == "adulto" ])#promedio de dias de fallecimientos en adultos
mean(CL$time[CL$status==1 & CL$age == "joven" ])#promedio de dias de fallecimientos en jovenes 
mean(CL$time[CL$status==0 & CL$age == "adulto mayor" ])#promedio de dias de censurados en adultos mayores
mean(CL$time[CL$status==0 & CL$age == "adulto" ])#promedio de dias de censurados en adultos
mean(CL$time[CL$status==0 & CL$age == "joven" ])#promedio de dias de censurados en jovenes 

mean(CL$time[CL$status==1 & CL$obstruct == 1 ])#promedio de dias de fallecimientos con obstruccion en el colon
mean(CL$time[CL$status==1 & CL$obstruct == 0 ])#promedio de dias de fallecimientos sin obstruccion en el colon 
mean(CL$time[CL$status==0 & CL$obstruct == 1 ])#promedio de dias de censurados con obstruccion en el colon
mean(CL$time[CL$status==0 & CL$obstruct == 0 ])#promedio de dias de censurados sin obstruccion en el colon 

mean(CL$time[CL$status==1 & CL$perfor == 1 ])#promedio de dias de fallecimientos con perforacion en el colon 
mean(CL$time[CL$status==1 & CL$perfor == 0 ])#promedio de dias de fallecimientos sin perforacion en el colon
mean(CL$time[CL$status==0 & CL$perfor == 1 ])#promedio de dias de censurados con perforacion en el colon 
mean(CL$time[CL$status==0 & CL$perfor == 0 ])#promedio de dias de censurados sin perforacion en el colon

mean(CL$time[CL$status==1 & CL$adhere == 1 ])#promedio de dias de fallecimientos con adherencia a organos cercanos
mean(CL$time[CL$status==1 & CL$adhere == 0 ])#promedio de dias de fallecimientos sin adherencia a organos cercanos
mean(CL$time[CL$status==0 & CL$adhere == 1 ])#promedio de dias de censurados con adherencia a organos cercanos
mean(CL$time[CL$status==0 & CL$adhere == 0 ])#promedio de dias de censurados sin adherencia a organos cercanos

mean(CL$time[CL$status==1 & CL$differ == 1 ])#promedio de dias de fallecimientos con diferenciacion de tumor bueno
mean(CL$time[CL$status==1 & CL$differ == 2 ])#promedio de dias de fallecimientos con diferenciacion de tumor regular
mean(CL$time[CL$status==1 & CL$differ == 3 ])#promedio de dias de fallecimientos con diferenciacion de tumor pobre
mean(CL$time[CL$status==0 & CL$differ == 1 ])#promedio de dias de censurados con diferenciacion de tumor bueno
mean(CL$time[CL$status==0 & CL$differ == 2 ])#promedio de dias de censurados con diferenciacion de tumor regular
mean(CL$time[CL$status==0 & CL$differ == 3 ])#promedio de dias de censurados con diferenciacion de tumor pobre

mean(CL$time[CL$status==1 & CL$extent == 1 ])#promedio de dias de fallecimientos con propagación local en submucosa
mean(CL$time[CL$status==1 & CL$extent == 2 ])#promedio de dias de fallecimientos con propagación local en musculo
mean(CL$time[CL$status==1 & CL$extent == 3 ])#promedio de dias de fallecimientos con propagación local en serosa
mean(CL$time[CL$status==1 & CL$extent == 4 ])#promedio de dias de fallecimientos con propagación local en estructuras contiguas 
mean(CL$time[CL$status==0 & CL$extent == 1 ])#promedio de dias de censurados con propagación local en submucosa
mean(CL$time[CL$status==0 & CL$extent == 2 ])#promedio de dias de censurados con propagación local en musculo
mean(CL$time[CL$status==0 & CL$extent == 3 ])#promedio de dias de censurados con propagación local en serosa
mean(CL$time[CL$status==0 & CL$extent == 4 ])#promedio de dias de censurados con propagación local en estructuras contiguas

mean(CL$time[CL$status==1 & CL$surg == 0 ])#promedio de dias de fallecimientos con duracion corta desde su cirugia al registro
mean(CL$time[CL$status==1 & CL$surg == 1 ])#promedio de dias de fallecimientos con  duracion larga desde su cirugia al registro
mean(CL$time[CL$status==0 & CL$surg == 0 ])#promedio de dias de censurados con duracion corta desde su cirugia al registro
mean(CL$time[CL$status==0 & CL$surg == 1 ])#promedio de dias de censurados con  duracion larga desde su cirugia al registro

mean(CL$time[CL$status==1 & CL$node4 == 1 ])#promedio de dias de fallecimientos con mas de 4 ganglios linfaticos
mean(CL$time[CL$status==1 & CL$node4 == 1 ])#promedio de dias de fallecimientos con menos de 4 ganglios linfaticos
mean(CL$time[CL$status==0 & CL$node4 == 1 ])#promedio de dias de censurados con mas de 4 ganglios linfaticos
mean(CL$time[CL$status==0 & CL$node4 == 1 ])#promedio de dias de censurados con menos de 4 ganglios linfaticos

# Histogramas
par(mfrow=c(1,1))
hist(colon$time, main = "Cancer de colon", xlab = "Tiempo", 
     ylab = "Frecuencia",col = "magenta", border = "pink")
par(mfrow=c(3,3))
pie(table(CL$rx), main="Tipo de tratamiento ")
pie(table(CL$age), main="Rango de edades" )
pie(table(CL$obstruct), main="Obstrucción en el colon")
pie(table(CL$perfor), main="Perforación en el colon")
pie(table(CL$adhere), main="Adherencia a organos cercanos")
pie(table(CL$differ), main="Diferenciación de tumor",
    labels=c("bueno", "regular", "pobre"))
pie(table(CL$extent), main="Propagación local",
    labels = c ("submucosa", "musculosa", "serosa", "estructuras contiguas"))
pie(table(CL$surg), main="Tiempo de registro despues de la cirugia",
    labels= c ("corto", "largo"))
pie(table(CL$node4), main= "Mas de 4 ganglios linfaticos")


#regresion logistica, binomial 
r <-step(glm(CL$status ~ CL$rx+CL$sex+CL$age+CL$obstruct+CL$perfor+CL$adhere
    +CL$extent+CL$differ+CL$surg+CL$node4+CL$time, family="binomial"))
summary(r)
#glm(formula = CL$status ~ CL$surg + CL$time, family = "binomial")
#AIC: 379.54
# Analisis de Supervivencia: Modelo de Cox
names(CL)
s<- Surv(CL$time, CL$status)
#variables eliminadas 
#CL$perfor Pr(>|z|) =0.95830, CL$sexPr(>|z|) =0.89899
cox <- coxph(s ~CL$rx+CL$age+CL$obstruct+CL$adhere
              +CL$extent+CL$differ+CL$surg+CL$node4)
summary(cox)                   

#Recategorizamos CL$rxLev Pr(>|z|)=  0.80116
a <- CL$rx
a[CL$rx=="Lev"]<- "Other"
#Variables eliminadas 
# CL$obstruct Pr(>|z|)=0.49291
# CL$differ Pr(>|z|)=0.39092,  CL$agejoven Pr(>|z|)=0.3389
cox <- coxph(s ~ a+CL$adhere+CL$age
             +CL$extent+CL$surg+CL$node4)
summary(cox) 

#CL$agejoven Pr(>|z|)=0.3389 recategorizamos
e <- CL$age
e[CL$age == "joven"]<- NA
CL$age

#variables eliminadas
#CL$adhere Pr(>|z|)= 0.27825, e Pr(>|z|)=0.118786,CL$surg Pr(>|z|)= 0.151288
cox <- coxph(s ~ a+CL$extent+CL$node4)
summary(cox)


# IV: Graficar los residuales del modelo de Cox
par(mfrow=c(1,1))
plot(residuals(cox, type = "deviance"),ylim = c(-3.1,3.1), 
     main="Residuales Device")
abline(h=c(-3,3), col="red")
#hay datos que superan el 3 
which(residuals(cox, type="deviance")>abs(3))
#datos que superan abs(3), 105 149 72 104
CLnew <- CL[-c(105,149, 72, 104), ]
snew <- Surv(CLnew$time, CLnew$status)
anew <- a[-c(105,149, 72, 104)]
View(CLnew)
length(a)
coxnew <-coxph(snew ~ anew+ CLnew$extent  + CLnew$node4)
summary(coxnew)

plot(residuals(coxnew, type = "martingale"), main="Residuales martingala")
abline(h=1, col="blue")
max(residuals(coxnew, type = "martingale"))
#0.9974628
plot(residuals(coxnew, type = "schoenfeld"),main="Residuales Schoenfeld ")
scho <- residuals(coxnew, type="schoenfeld")
scho
par(mfrow=c(2,3))
plot(scho[,1], scho[,2])
plot(scho[,1], scho[,3])
plot(scho[,1], scho[,4])
plot(scho[,2], scho[,3])
plot(scho[,2], scho[,4])
plot(scho[,3], scho[,4])

f <- survfit(coxnew)
summary(f)
par(mfrow=c(1,1))
plot(f, main="Supervivencia estimada por Cox, data=Colon",
     ylab= "supervivencia", xlab="tiempo", ylim=c(.3,1))
###################################################################
#Kaplan Meier 
# con variables anew ,CLnew$extent, CLnew$node4
#anew
s.a <- survfit(snew~anew)
summary(s.a)
#grafico
par(mfrow=c(1,3))
plot(s.a, col= c("red", "blue"), main= "Comparativa por tratamiento",
     xlab="tiempo", ylab="supervivencia", ylim=c(.3, 1))
plot(survfit(snew[anew=="Obs"]~1), col="red", main="Obs",
     xlab="tiempo", ylab="supervivencia", ylim=c(.3, 1))
plot(survfit(snew[anew=="Lev+5FU"]~1), col="blue", main="Lev+5FU",
     xlab="tiempo", ylab="supervivencia", ylim=c(.3, 1))
#prueba de hipotesis 
#H0: las funciones de supervivencia no difieren 
#Ha: las funciones de supervivencia difieren 
survdiff(snew~anew, rho=0)
#rechazo h0 si p= p= 0.001 <0.05, rechazo H0 
#las funciones de supervivencia si difieren 
#dado el graficoel tratamiento Lev+5FU tiene mayor supervivencia

#CLnew$extent
#1 = submucosa, 2 = mÃºsculo, 3 = serosa, 4 = estructuras contiguas
par(mfrow=c(1,1))
s.e <- survfit(snew~CLnew$extent)
summary(s.e)
par(mfrow=c(2,3))
plot(s.e, col=c("magenta", "purple", "blue", "green"), 
     main="Comparativa de Extensión de la propagación local",
     xlab="tiempo", ylab="supervivencia", ylim=c(.2,1))
plot(survfit(snew[CLnew$extent==1]~1), col="magenta",
     main="Submucosa", 
     xlab="tiempo", ylab="supervivencia", ylim=c(.2,1))
plot(survfit(snew[CLnew$extent==2]~1), col="purple",
     main="Musculo", 
     xlab="tiempo", ylab="supervivencia", ylim=c(.2,1))
plot(survfit(snew[CLnew$extent==3]~1), col="blue",
     main="Serosa", 
     xlab="tiempo", ylab="supervivencia", ylim=c(.2,1))
plot(survfit(snew[CLnew$extent==4]~1), col="green",
     main="Estructuras contiguas", 
     xlab="tiempo", ylab="supervivencia", ylim=c(.2,1))
#prueba de hipotesis 
#H0: las funciones de supervivencia no difieren 
#Ha: las funciones de supervivencia difieren 
survdiff(snew~CLnew$extent, rho=0)
#rechazo h0 si p= 2e-05  <0.05, rechazo H0 
#las funciones de supervivencia si difieren 
#dado el graficoel la extensión de propagacion de
#submucosa tiene mayor supervivencia

#CLnew$node4
#MÃ¡s de 4 ganglios linfÃ¡ticos positivos 
# 1 = Mas de 4, 0 = Menos de 4
par(mfrow=c(1,1))
s.n4 <- survfit(snew~CLnew$node4)
summary(s.n4)
par(mfrow=c(1,3))
plot(s.n4, col=c("magenta", "purple"), 
     main="Comparativa de cantidad de glanglios linfaticos",
     xlab="tiempo", ylab="supervivencia", ylim=c(.2,1))
plot(survfit(snew[CLnew$node4==0]~1), col="magenta",
     main="menor o igual a 4", 
     xlab="tiempo", ylab="supervivencia", ylim=c(.2,1))
plot(survfit(snew[CLnew$extent==1]~1), col="purple",
     main="mayor de 4", 
     xlab="tiempo", ylab="supervivencia", ylim=c(.2,1))
#prueba de hipotesis 
#H0: las funciones de supervivencia no difieren 
#Ha: las funciones de supervivencia difieren 
survdiff(snew~CLnew$node4, rho=0)
#rechazo h0 si p= <2e-16 <0.05, rechazo H0 
#las funciones de supervivencia si difieren 
#dado el grafico los que tienen 
#mayor de 4 ganlgios linfaticos tienen mayor supervivencia

######################################################################
#parametrico
par(mfrow=c(1,1))
fs <-survfit(coxnew)
summary(fs)
sp <- survfit(coxnew)$surv
tp <- survfit(coxnew)$time

#revisando familia exp
#riesgo
r<-rep(0,length(sp))#riesgo asignado
for (i in 2:length(sp)) {
  r[i]<-(sp[i-1]-sp[i])/sp[i]
}
r#riesngo calculado
riesgo<-r[-1]
#graficamente buscamos recta horizantal
#familia exponencial
plot(tp[-1],riesgo, main="Exponencial")
#revisando sea horizontal
summary(lm(riesgo~tp[-1]))#h0:recta horizontal
#p-value: 9.98e-10, Multiple R-squared:  0.06832
abline(lm(riesgo~tp[-1]), col="red")
#el riensgo no se describe meiente recta horizontal
#no se asocia a familia horizontal


#Revisando falimia waibull
{
  #Foema 1. con recta al origen t recta normal, se desea que el origen sea mejor
  #-log s(t) vs tiempo t
  plot(tp,-log(sp), main="Weibull")
  #revisando regresion
  summary(lm(-log(sp)~tp))
  #p-value: < 2.2e-16, Multiple R-squared:  0.9776
  abline(lm(-log(sp)~tp),col="red")#corta en el origen 
  #Revisando regresion al origen
  summary(lm(-log(sp)~tp+0))
  #p-value: < 2.2e-16, Multiple R-squared:  0.9951
  abline(lm(-log(sp)~tp+0),col="blue")
}
{
  #forma 2.log del log, una recta cualquiera
  #log[-log(s)] vs el tiempo log t
  plot(log(tp),log(-log(sp)), main="log")
  #revisando regresion
  summary(lm(log(-log(sp))~log(tp)))
  #p-value: < 2.2e-16,Multiple R-squared:  0.9568
  abline(lm(log(-log(sp))~log(tp)),col="red")#9887  
  #la dispercion se describe mediente recta
  #asocoamos los datos a weibull
}
#revisando familia lognormal
{
  #fomar 1.  Î¦âˆ’1[1âˆ’ S (t)] versus log(t) 
  help("rnorm")#q inversa
  qnorm(1-0.05,mean = 0,sd=1)
  inv <- qnorm(1-sp,mean = 0,sd=1)
  plot(log(tp),inv, main="lognormal")
  summary(lm(inv~log(tp)))
  #p-value: < 2.2e-16, Multiple R-squared:  0.9903
  abline(lm(inv~log(tp)),col="red")
  #la dispercion se describe mediente recta
  #asocoamos los datos a lognormal
}
#forma 2. log[(1âˆ’ S (t))/S (t)] versus log( t)
{
  
  auxp<-log((1-sp)/sp)
  plot(log(tp),auxp, main="lognormal")
  summary(lm(auxp~log(tp)))
  #p-value: < 2.2e-16, Multiple R-squared:  0.9732
  abline(lm(auxp~log(tp)),col="red")
  #la dispercion se describe mediente recta
  #asocoamos los datos a lognormal
}

#modelo loglogimetro
#definimos la funcion logit
Logit<-log(sp/(1-sp))
plot(log(tp),Logit, main="logit")
summary(lm(Logit~log(tp)))
abline(lm(Logit~log(tp)),col="red")
# p-value: < 2.2e-16, Multiple R-squared:  0.9732
#la dispercion se describe mediente recta
#asocoamos los datos a loglogimetro

#revision de datos 
w <- survreg(snew ~ anew+ CLnew$extent  + CLnew$node4, dist= "weibull")
w#p= <2e-16 
l<- survreg(snew ~ anew+ CLnew$extent  + CLnew$node4, dist= "lognormal")
l#p= <2e-16 
lgg<- survreg(snew ~ anew+ CLnew$extent  + CLnew$node4, dist= "loglogistic")
lgg# p= <2e-16 

install.packages("flexsurv")
library(flexsurv)
w_c<-flexsurvreg( snew ~anew+ CLnew$extent  + CLnew$node4, dist= "weibull")
plot(w_c, main="weibull")#AIC = 5055.748
summary(w_c)
log_w <-flexsurvreg( snew ~anew+ CLnew$extent  + CLnew$node4, dist= "lognormal")
plot(log_w, main="lognormal")
summary(log_w)#AIC = 5019.673
logg_w <-flexsurvreg( snew ~anew+ CLnew$extent  + CLnew$node4, dist= "llogis")
plot(logg_w, main="loglogistico")
summary(logg_w)#AIC = 5031.061
