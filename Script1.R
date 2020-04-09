rm(list = ls())

library(reshape2)
library(tidyverse)
library(dplyr)
library(ggplot2)

getwd()

db19 <- read.csv('2019.csv', head=TRUE, sep=',', na=NA)
db18 <- read.csv('2018.csv', head=TRUE, sep=',', na=NA)
db17 <- read.csv('2017.csv', head=TRUE, sep=',', na=NA)
db16 <- read.csv('2016.csv', head=TRUE, sep=',', na=NA)
db15 <- read.csv('2015.csv', head=TRUE, sep=',', na=NA)

nuevo<-c('Ranking','Country',"Score",
         'GDP','Social','H&L Expectancy',
         'Freedom', "Generosity",'Corruption')
#verificamos q alinen los nuevos nombres
cbind(colnames(db19),nuevo)

colnames(db19)<-nuevo

db19$Year<-2019
head(db19)

cbind(colnames(db18),nuevo)
colnames(db18)<-nuevo
db18$Year<-2018

viejo<-c("Country","Ranking","Score",
         "hi","lo","GDP",
         "Family","H&L Expectancy","Freedom",
         "Generosity","Corruption","Dystopia")
cbind(colnames(db17),viejo)

colnames(db17)<-viejo
db17$Year<-2017

viejo<-c("Country","Region","Ranking",
         "Score","lo","hi",
         "GDP","Family","H&L Expectancy",
         "Freedom","Corruption","Generosity",
         "Dystopia")
cbind(colnames(db16),viejo)
colnames(db16)<-viejo
db16$Year<-2016
viejo<-c("Country","Region","Ranking",
         "Score","SE","GDP",
         "Family","H&L Expectancy","Freedom",
         "Corruption","Generosity","Dystopia")
cbind(colnames(db15),viejo)

colnames(db15)<-viejo
db15$Year<-2015

columnas.comunes<-colnames(db19)
columnas.comunes<-columnas.comunes[columnas.comunes%in%colnames(db18)]
columnas.comunes<-columnas.comunes[columnas.comunes%in%colnames(db17)]
columnas.comunes<-columnas.comunes[columnas.comunes%in%colnames(db16)]
columnas.comunes<-columnas.comunes[columnas.comunes%in%colnames(db15)]
unificado<-rbind(db19[,columnas.comunes],db18[,columnas.comunes],db17[,columnas.comunes],
                 db16[,columnas.comunes],db15[,columnas.comunes])
head(unificado)
unificado$Corruption<-as.numeric(as.character(unificado$Corruption))

summary(unificado)


barplot(unificado$Score[unificado$Year==2019],horiz = TRUE,
        names.arg = unificado$Country[unificado$Year==2019],
        las=2,cex.names = 0.5,border = NA)

###Histogramas


hist(unificado$Score,col = "cornflowerblue",lty="blank",
     main = "Histograma Score",xlab = "Score",ylab = "Frecuencia")
abline(v=unificado$Score[unificado$Country=="Argentina"&unificado$Year==2019],
       col="orange",lty="dashed",lwd=2)
legend(2.5,120,legend = c("AR 19"),col=c("Orange"),lwd=c(2),lty = c("dashed"))
shapiro.test(unificado$Score)

hist(unificado$GDP,col = "cornflowerblue",lty="blank",
     main = "Histograma GDP",xlab = "Score",ylab = "Frecuencia")
abline(v=unificado$GDP[unificado$Country=="Argentina"&unificado$Year==2019],
       col="orange",lty="dashed",lwd=2)
legend("topright",legend = c("AR 19"),col=c("Orange"),lwd=c(2),lty = c("dashed"))
shapiro.test(unificado$GDP)

hist(unificado$`H&L Expectancy`,col = "cornflowerblue",lty="blank",
     main = "Histograma H & L Expectancy",xlab = "Score",ylab = "Frecuencia")
abline(v=unificado$`H&L Expectancy`[unificado$Country=="Argentina"&unificado$Year==2019],
       col="orange",lty="dashed",lwd=2)
legend("topleft",legend = c("AR 19"),col=c("Orange"),lwd=c(2),lty = c("dashed"))
shapiro.test(unificado$`H&L Expectancy`)

hist(unificado$`Freedom`,col = "cornflowerblue",lty="blank",
     main = "Histograma Freedom",xlab = "Score",ylab = "Frecuencia")
abline(v=unificado$`Freedom`[unificado$Country=="Argentina"&unificado$Year==2019],
       col="orange",lty="dashed",lwd=2)
legend("topleft",legend = c("AR 19"),col=c("Orange"),lwd=c(2),lty = c("dashed"))
shapiro.test(unificado$Freedom)

hist(unificado$`Generosity`,col = "cornflowerblue",lty="blank",
     main = "Histograma Generosity",xlab = "Score",ylab = "Frecuencia")
abline(v=unificado$`Generosity`[unificado$Country=="Argentina"&unificado$Year==2019],
       col="orange",lty="dashed",lwd=2)
legend("topright",legend = c("AR 19"),col=c("Orange"),lwd=c(2),lty = c("dashed"))
shapiro.test(unificado$Generosity)

hist(unificado$`Corruption`,col = "cornflowerblue",lty="blank",
     main = "Histograma Corruption",xlab = "Score",ylab = "Frecuencia")
abline(v=unificado$`Corruption`[unificado$Country=="Argentina"&unificado$Year==2019],
       col="orange",lty="dashed",lwd=2)
legend("topright",legend = c("AR 19"),col=c("Orange"),lwd=c(2),lty = c("dashed"))
shapiro.test(unificado$Corruption)

###Tendencias


solo.argentina<-unificado[unificado$Country=="Argentina",]
##solo.argentina$Corruption<-as.numeric(as.character(solo.argentina$Corruption))
solo.noruega<-unificado[unificado$Country=="Norway",]

mediana<-aggregate(unificado$Score,by=list(unificado$Year),FUN=median)

plot(solo.argentina$Year,solo.argentina$Ranking,col="Orange",
     main = "Tendencia Ranking",
     frame.plot = FALSE,
     ylim = c(1,50),
     type="l",lwd=2,xlab="Años",ylab = "Ranking")
lines(solo.noruega$Year,solo.noruega$Ranking,col="Blue",
      type="l",lwd=2,xlab="Años",ylab = "Ranking")
legend("topleft",legend = c("Argentina","Noruega"),col=c("Orange","Blue"),lwd=c(2,2),lty = c("solid","solid"))

plot(solo.argentina$Year,solo.argentina$Score,col="Orange",
     main = "Tendencia Score",
     frame.plot = FALSE,
     ylim = c(4,8),
     type="l",lwd=2,xlab="Años",ylab = "Score")
lines(solo.noruega$Year,solo.noruega$Score,col="Blue",
      type="l",lwd=2,xlab="Años",ylab = "Score")
lines((aggregate(unificado$Score,by=list(unificado$Year),FUN=median)),col="Grey",
      type="l",lwd=2,xlab="Años",ylab = "Score")
legend("bottomleft",legend = c("Argentina","Noruega","Mediana"),col=c("Orange","Blue","Grey"),lwd=c(2,2,2),lty = c("solid","solid","solid"))


plot(solo.argentina$Year,solo.argentina$GDP,col="Orange",
     main = "Tendencia GDP",
     frame.plot = FALSE,
     ylim = c(0,2),
     type="l",lwd=2,xlab="Años",ylab = "GDP")
lines(solo.noruega$Year,solo.noruega$GDP,col="Blue",
      type="l",lwd=2,xlab="Años",ylab = "GDP")
lines((aggregate(unificado$GDP,by=list(unificado$Year),FUN=median)),col="Grey",
      type="l",lwd=2,xlab="Años",ylab = "GDP")
legend("bottomleft",legend = c("Argentina","Noruega","Mediana"),col=c("Orange","Blue","Grey"),lwd=c(2,2,2),lty = c("solid","solid","solid"))

plot(solo.argentina$Year,solo.argentina$'H&L Expectancy',col="Orange",
     main = "Tendencia H & L Expectancy",
     frame.plot = FALSE,
     ylim = c(0,2),
     type="l",lwd=2,xlab="Años",ylab = "H&L Expectancy")
lines(solo.noruega$Year,solo.noruega$'H&L Expectancy',col="Blue",
      type="l",lwd=2,xlab="Años",ylab = "H&L Expectancy")
lines((aggregate(unificado$'H&L Expectancy',by=list(unificado$Year),FUN=median)),col="Grey",
      type="l",lwd=2,xlab="Años",ylab = "H&L Expectancy")
legend("topleft",legend = c("Argentina","Noruega","Mediana"),col=c("Orange","Blue","Grey"),lwd=c(2,2,2),lty = c("solid","solid","solid"))

plot(solo.argentina$Year,solo.argentina$Freedom,col="Orange",
     main = "Tendencia Freedom",
     frame.plot = FALSE,
     ylim = c(0,1.5),
     type="l",lwd=2,xlab="Años",ylab = "Freedom")
lines(solo.noruega$Year,solo.noruega$Freedom,col="Blue",
      type="l",lwd=2,xlab="Años",ylab = "Freedom")
lines((aggregate(unificado$Freedom,by=list(unificado$Year),FUN=median)),col="Grey",
      type="l",lwd=2,xlab="Años",ylab = "Freedom")
legend("topleft",legend = c("Argentina","Noruega","Mediana"),col=c("Orange","Blue","Grey"),lwd=c(2,2,2),lty = c("solid","solid","solid"))

plot(solo.argentina$Year,solo.argentina$Generosity,col="Orange",
     main = "Tendencia Generosity",
     frame.plot = FALSE,
     ylim = c(0,1),
     type="l",lwd=2,xlab="Años",ylab = "Generosity")
lines(solo.noruega$Year,solo.noruega$Generosity,col="Blue",
      type="l",lwd=2,xlab="Años",ylab = "Generosity")
lines((aggregate(unificado$Generosity,by=list(unificado$Year),FUN=median)),col="Grey",
      type="l",lwd=2,xlab="Años",ylab = "Generosity")
legend("topleft",legend = c("Argentina","Noruega","Mediana"),col=c("Orange","Blue","Grey"),lwd=c(2,2,2),lty = c("solid","solid","solid"))

plot(solo.argentina$Year,solo.argentina$Corruption,col="Orange",
     main = "Tendencia Corruption",
     frame.plot = FALSE,
     ylim = c(0,1),
     type="l",lwd=2,xlab="Años",ylab = "Corruption")
lines(solo.noruega$Year,solo.noruega$Corruption,col="Blue",
      type="l",lwd=2,xlab="Años",ylab = "Corruption")
lines((aggregate(unificado$Corruption,by=list(unificado$Year),FUN=median)),col="Grey",
      type="l",lwd=2,xlab="Años",ylab = "Corruption")
legend("topleft",legend = c("Argentina","Noruega","Mediana"),col=c("Orange","Blue","Grey"),lwd=c(2,2,2),lty = c("solid","solid","solid"))



plot(solo.argentina$Year,solo.argentina$Corruption,col="grey",
     main = "Percepcion de Corrupcion",
     frame.plot = FALSE,pch=19,
     type="p",lwd=2,xlab="Años",ylab = "Corrupcion")
##hasta aqui armar los comparativos
m1<-lm(Corruption~Year,data = solo.argentina)
abline(m1,col="cornflowerblue",lwd=2)
m2<-lm(log(Corruption)~Year,data = solo.argentina)
lines(seq(2015,2019,by=0.1),
      exp(predict(m2,data.frame(Year=seq(2015,2019,by=0.1)))),
      col="orange",
      lwd=2,
      lty="dashed")
summary(m1)
summary(m2)