library(het.test)
library(olsrr)
library(lmtest)

#-- White test function
white.test <- function(lmod,data=d){
  u2 <- lmod$residuals^2
  y <- fitted(lmod)
  Ru2 <- summary(lm(u2 ~ y + I(y^2)))$r.squared
  LM <- nrow(data)*Ru2
  p.value <- 1-pchisq(LM, 2)
  data.frame("Test statistic"=LM,"P value"=p.value)
}

#-- funzione per ottenere osservazioni outlier univariate
FIND_EXTREME_OBSERVARION <- function(x,sd_factor=2){
  which(x>mean(x)+sd_factor*sd(x) | x<mean(x)-sd_factor*sd(x))  
}


d <- read.csv("PRSA_data_2010.1.1-2014.12.31.csv")

#-- creo la variabile "giorno" che serve per aggregare i dati
d$month[nchar(d$month)==1] <- paste0("0",d$month[nchar(d$month)==1])
d$day[nchar(d$day)==1] <- paste0("0",d$day[nchar(d$day)==1])
d$giorno <- paste0(d$year,"-",d$month,"-",d$day)

aggr1 <- aggregate(d[,c("pm2.5","DEWP","PRES","TEMP")],list(giorno=d$giorno),mean,na.rm=T) #-- aggrego usando la funzione media
aggr2 <- aggregate(d[,c("Is","Ir","Iws")],list(giorno=d$giorno),max,na.rm=T) #-- aggreco usando il massimo
aggr3 <- aggregate(cbind(cbwd=d[,c("cbwd")]),list(giorno=d$giorno),function(x) names(which.max(table(x))))

unione <- merge(aggr1,aggr2,by="giorno")
unione <- merge(unione,aggr3,by="giorno")
unione$giorno <- as.Date(unione$giorno)
unione$year <- substr(paste(unione$giorno),1,4)
unione$month <- substr(paste(unione$giorno),6,7)

#-- creo variabile weekdays/weekend
unione$label_giorno <- weekdays(unione$giorno)
unione$label_giorno[unione$label_giorno!=c("Saturday","Sunday")] <- "Weekdays"
unione$label_giorno[unione$label_giorno==c("Saturday","Sunday")] <- "Weekend"
table(paste(unione$label_giorno))


#-- Descrittive
par(mfrow=c(1,2))
hist(d$pm2.5,main="PM2.5",col="lightblue",xlab="")
hist(log(d$pm2.5),main="log - pm2.5",col="lightblue",xlab="")

par(mfrow=c(1,2))
boxplot(d$pm2.5,main="PM2.5",col="lightblue",xlab="")
boxplot(log(d$pm2.5),main="log - pm2.5",col="lightblue",xlab="")


par(mfrow=c(1,1))
hist(log(unione$pm2.5),freq=F,main="log - pm2.5",col="lightblue",xlab="")
curve(dnorm(x,log(mean(unione$pm2.5,na.rm = T)),sd(log(unione$pm2.5),na.rm = T)),add=T,col=2,lwd=3)

shapiro.test(log(unione$pm2.5))

boxplot(unione$pm2.5~unione$month,main="PM2.5 by month",col="lightblue",xlab="")
boxplot(log(unione$pm2.5)~unione$year,main="PM2.5 by month",col="lightblue",xlab="")


VAR_NUMERIC_1 <- c("pm2.5","DEWP","PRES","TEMP")
VAR_NUMERIC_2 <- c("pm2.5","Iws","Is","Ir")

pairs(unione[,VAR_NUMERIC_1],pch=19,col="blue",cex=.7)
round(cor(unione[,VAR_NUMERIC_1],use="pairwise.complete.obs"),4)*100

pairs(unione[,VAR_NUMERIC_2],pch=19,col="blue",cex=.7)
round(cor(unione[,VAR_NUMERIC_2],use="pairwise.complete.obs"),4)*100

aggregate(unione[,c("pm2.5")],list(cbwd=unione$cbwd),mean,na.rm=T)


#-- Primo modello lineare
mod1 <- lm(pm2.5 ~ DEWP + TEMP + PRES + Is + Ir + Iws + cbwd, unione)
summary(mod1)
ols_vif_tol(mod1)
white.test(mod1)
dwtest(mod1)

plot(mod1,which=1,pch=19)
plot(mod1,which=2,pch=19)
plot(mod1,which=3,pch=19)
plot(mod1,which=4,pch=19)


#-- elimino osservazioni influenti (ma nei modelli seguenti saranno utilizzate comunque tutte le osservazioni)
unione_no_out <- unione[!cooks.distance(mod1)>4/nrow(unione),]

#-- modello log lineare
mod2 <- lm(log(pm2.5) ~ DEWP + TEMP + PRES + Is + Ir + Iws + cbwd, unione)
summary(mod2)

plot(mod2,which=1,pch=19)
plot(mod2,which=2,pch=19)
plot(mod2,which=3,pch=19)
plot(mod2,which=4,pch=19)

white.test(mod2)
dwtest(mod2)


#-- esempio di modello completo
mod3 <- lm(log(pm2.5) ~ year + month + label_giorno + DEWP + TEMP + PRES + Is + Ir + Iws + cbwd + Ir*Iws, unione)
summary(mod3)
plot(mod2)
     
aggregate(unione$pm2.5,list(unione$year),mean,na.rm=T)


#-- Studio dell'autocorrelazione

autocorr <- acf(resid(mod3),lwd=2)

unione$cbwd <- factor(unione$cbwd)
levels(unione$cbwd) <- c("A","B","C","D")


#-- modello arima con e coefficienti autoregressivi di lag 1,2,3
#-- nota: devo definire a mano la matrice disegno per la variabile cbwd

library(Hmisc)

#-- matrice per la variabile cbwd a cui tolto l'intercetta che non serve
model.matrix(~as.factor(unione$cbwd))[,-1]

mod_arima <- arima(unione$pm2.5, order=c(3,0,0), xreg=cbind(unione[,c("DEWP","TEMP","PRES","Is","Ir","Iws")],cbwd=model.matrix(~as.factor(unione$cbwd))[,-1]),method="ML") 
mod_arima
coeftest(mod_arima)



#-- confronto con al altri modelli

library(randomForest)
library(rpart)
sel_unione <- na.exclude(unione)

r_tree <- rpart(pm2.5 ~ year + month + label_giorno + DEWP + TEMP + PRES + Is + Ir + Iws + cbwd, sel_unione)
rf <- randomForest(sel_unione[,c("year","month","DEWP","TEMP","PRES","Is","Ir","Iws","cbwd")],sel_unione$pm2.5)

predict_tree <- predict(r_tree,sel_unione)
predict_rf <- predict(rf,sel_unione)
predict_mod2 <- predict(mod2,sel_unione)
predict_mod3 <- predict(mod3,sel_unione)


#-- errore atteso di previsione
sqrt(sum((predict_tree-sel_unione$pm2.5)^2,na.rm=T)/nrow(sel_unione))
sqrt(sum((predict_rf-sel_unione$pm2.5)^2,na.rm=T)/nrow(sel_unione))
sqrt(sum((exp(predict_mod2)-sel_unione$pm2.5)^2,na.rm=T)/nrow(sel_unione))
sqrt(sum((exp(predict_mod3)-sel_unione$pm2.5)^2,na.rm=T)/nrow(sel_unione))












