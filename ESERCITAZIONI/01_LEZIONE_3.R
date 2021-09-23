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

#-- import dei dati
library(AER)
data("ManufactCosts")
d <- data.frame(ManufactCosts)
names(d) <- c("cost","k","l","e","m","pk","pl","pe","pm")

#-- vettore di variabili numeriche presenti nei dati
VAR_NUMERIC <- c("cost","k","l","e","m","pk","pl","pe","pm")

summary(d[,VAR_NUMERIC])
cor(d[,VAR_NUMERIC])
plot(d[,VAR_NUMERIC],pch=19,cex=.5) #-- scatter plot multivariato

par(mfrow=c(3,3))
for(i in VAR_NUMERIC){
  boxplot(d[,i],main=i,col="lightblue",ylab=i)
}


mod1 <- lm(cost ~ l + pk + pl + pm, d)
summary(mod1)
anova(mod1)

plot(mod1,which=1,pch=19)
plot(mod1,which=2,pch=19)
plot(mod1,which=3,pch=19)
plot(mod1,which=4,pch=19)

white.test(mod1)
dwtest(mod1)

plot(1:nrow(d),resid(mod1),xlab="Observation Index",ylab="Residui",pch=19)
abline(h=0,col=2,lwd=3,lty=2)

library(Hmisc)
d1  <- d
d1$resid <- resid(mod1)
d1$resid_l1 <- Lag(d1$resid,1)

cor(data.frame(d1$resid,d1$resid_l1),use="pairwise.complete.obs")


mod2 <- arima(d1$cost, order=c(1,0,0), xreg = d1[,c("l","pk","pl","pm")],method="ML") 
mod2
coeftest(mod2)
coefci(mod2)
durbinWatsonTest(as.numeric(mod2$residuals))


#-- Esempio 2

#-- import dei dati
ABSOLUTE_PATH <- "C:\\Users\\sbarberis\\Dropbox\\MODELLI STATISTICI"
d <- read.csv(paste0(ABSOLUTE_PATH,"\\F. Esercizi(22) copia\\1.Error-GLS copy(8)\\2.Error-GLS\\companies-1.csv"),sep=";")

VAR_NUMERIC <- c("assets","sales","mark_val","profits","cash","employ")

head(d)

mod1 <- lm(mark_val ~ assets + sales + profits + cash + employ, d) #-- stima modello lineare semplice
summary(mod1)
anova(mod1)
confint(mod1)

white.test(mod1) #-- eteroschedasticità
dwtest(mod1,alternative="two.sided")

plot(mod1,which=1,pch=19)
plot(mod1,which=2,pch=19)
plot(mod1,which=3,pch=19)
plot(mod1,which=4,pch=19)
abline(h=2*4/nrow(d),col=2,lwd=3,lty=2)

plot(mod1,which=5,pch=19)


mod2 <- lm(resid(mod1)^2 ~ assets + sales + profits + cash + employ, d)
summary(mod2)
anova(mod2)

plot(resid(mod1)^2,d$cash,pch=19)

sd_error <- sqrt(fitted(mod2))

mod3 <- lm(I(mark_val/sd_error) ~ 0 + I(1/sd_error) + I(assets/sd_error) + I(sales/sd_error) + I(profits/sd_error) + I(cash/sd_error) + I(employ/sd_error), d)
summary(mod3)

white.test(mod3)
dwtest(mod3)

weight <- 1/fitted(mod2)
mod4 <- lm(mark_val ~ assets + sales + profits + cash + employ, d[-which(weight<0),],weights = weight[-which(weight<0)] )
summary(mod4)


#-- provare con il modello log-lineare


#-- Esempio 3

ABSOLUTE_PATH <- "C:\\Users\\sbarberis\\Dropbox\\MODELLI STATISTICI"
d <- read.csv(paste0(ABSOLUTE_PATH,"\\F. Esercizi(22) copia\\1.Error-GLS copy(8)\\3.Error-GLS\\Hartnagel.txt"),sep=" ")

VAR_NUMERIC <- c("year","tfr","partic","degrees","fconvict","ftheft","mconvict","mtheft")

mod1 <- lm(ftheft ~ partic + degrees + mtheft, d) #-- stima modello lineare semplice
summary(mod1)
anova(mod1)
white.test(mod1)
dwtest(mod1)

plot(mod1,which=1,pch=19)
plot(mod1,which=2,pch=19)
plot(mod1,which=3,pch=19)
plot(mod1,which=4,pch=19)
abline(h=2*4/nrow(d),col=2,lwd=3,lty=2)

plot(mod1,which=5,pch=19)

index <- as.numeric(row.names(mod1$model))
plot(d$year[index],resid(mod1),pch=19,xlab="Year",ylab="Residuo",type="l",col=1,lwd=2)
text(d$year[index],resid(mod1),d$year[index],pos=1,cex=.6)


autocorr <- acf(resid(mod1),main="Autocorrelazion",lwd=2)
data.frame(LAG=autocorr$lag,VALUE=autocorr$acf)[1:5,]

d1 <- data.frame(
  mod1$model,
  resid=resid(mod1)
)

d1$ftheft_l1 <- Lag(d1$ftheft,1)
d1$partic_l1 <- Lag(d1$partic,1)
d1$degrees_l1 <- Lag(d1$degrees,1)
d1$mtheft_l1 <- Lag(d1$mtheft,1)
d1$resid_l1 <- Lag(d1$resid,1)

d1$int_tild <- 1-0.542

d1$ftheft_t <- d1$ftheft-0.542*d1$ftheft_l1
d1$partic_t <- d1$partic-0.542*d1$partic_l1
d1$degrees_t <- d1$degrees-0.542*d1$degrees_l1
d1$mtheft_t <- d1$mtheft-0.542*d1$mtheft_l1
d1$resid_t <- d1$resid-0.542*d1$resid_l1


mod3 <- lm(ftheft_t ~ 0 + int_tild + partic_t + degrees_t +  mtheft_t,d1)
summary(mod3)
white.test(mod3)
dwtest(mod3)


mod4 <- arima(d1$ftheft, order=c(1,0,0), xreg = d1[,c("partic","degrees","mtheft")],method="ML") 
mod4
coeftest(mod4)
durbinWatsonTest(as.numeric(mod4$residuals))

mod5 <- arima(d1$ftheft, order=c(2,0,0), xreg = d1[,c("partic","degrees","mtheft")],method="ML") 
mod5
coeftest(mod5)
durbinWatsonTest(as.numeric(mod5$residuals), max.lag=2)

mod6 <- arima(d1$ftheft, order=c(3,0,0), xreg = d1[,c("partic","degrees","mtheft")],method="ML") 
mod6
coeftest(mod6)
durbinWatsonTest(as.numeric(mod5$residuals), max.lag=3)

