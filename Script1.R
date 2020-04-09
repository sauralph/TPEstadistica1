rm(list = ls())

library(reshape2)
library(tidyverse)
library(dplyr)
library(ggplot2)

setwd('./world-happiness')

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

summary(unificado)

barplot(unificado$Score[unificado$Year==2019],horiz = TRUE,
        names.arg = unificado$Country[unificado$Year==2019],
        las=2,cex.names = 0.5,border = NA)

hist(unificado$Score,col = "cornflowerblue",lty="blank",
     main = "Histograma Score",xlab = "Score",ylab = "Frecuencia")
abline(v=unificado$Score[unificado$Country=="Argentina"&unificado$Year==2019],
       col="orange",lty="dashed",lwd=2)
legend(2.5,120,legend = c("Argentina 2019"),col=c("Orange"),lwd=c(2),lty = c("dashed"))
shapiro.test(unificado$Score)

hist(unificado$GDP,col = "cornflowerblue",lty="blank",
     main = "Histograma GDP",xlab = "Score",ylab = "Frecuencia")
abline(v=unificado$GDP[unificado$Country=="Argentina"&unificado$Year==2019],
       col="orange",lty="dashed",lwd=2)
legend("topright",legend = c("Argentina 2019"),col=c("Orange"),lwd=c(2),lty = c("dashed"))
shapiro.test(unificado$GDP)

hist(unificado$`H&L Expectancy`,col = "cornflowerblue",lty="blank",
     main = "Histograma H & L Expectancy",xlab = "Score",ylab = "Frecuencia")
abline(v=unificado$`H&L Expectancy`[unificado$Country=="Argentina"&unificado$Year==2019],
       col="orange",lty="dashed",lwd=2)
legend("topleft",legend = c("Argentina 2019"),col=c("Orange"),lwd=c(2),lty = c("dashed"))
shapiro.test(unificado$GDP)

solo.argentina<-unificado[unificado$Country=="Argentina",]
solo.argentina$Corruption<-as.numeric(as.character(solo.argentina$Corruption))

plot(solo.argentina$Year,solo.argentina$GDP,col="Orange",
     frame.plot = FALSE,
     type="l",lwd=2,xlab="Años",ylab = "GDP")

plot(solo.argentina$Year,solo.argentina$Corruption,col="grey",
     main = "Percepcion de Corrupcion",
     frame.plot = FALSE,pch=19,
     type="p",lwd=2,xlab="Años",ylab = "Corrupcion")
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
