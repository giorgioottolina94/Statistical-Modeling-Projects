
#-- import librerie necessarie
library(het.test)
library(olsrr)

#-- import dati
d <- read.csv("C:\\Users\\sbarberis\\Dropbox\\MODELLI STATISTICI\\F. Esercizi(22) copia\\3.lin(5)\\1.linear\\car.test.txt",sep=" ")
table(d$Type)

VAR_NUMERIC <- c("Price","Mileage","Weight","Disp.")

d[,4]
d[,"Reliability"]
d[,VAR_NUMERIC]

summary(d[,VAR_NUMERIC])
plot(d[,VAR_NUMERIC],cex=.8)
cor(d[,VAR_NUMERIC])
boxplot(d$Price ~ d$Type,col=2)

d$Disp._l <- 2+d$Disp.

#-- costruzione modello lineare e quadratico
mod_lin <- lm( Price ~ Disp.,d)
mod_quad <- lm( Price ~ Disp. + I(Disp.^2),d)

mod_lin_log <- lm( log(Price) ~ Disp.,d)
summary(mod_lin_log)

plot(mod_lin,which=1)
plot(mod_lin,which=2)
plot(mod_lin,which=3)
plot(mod_lin,which=4)
plot(mod_lin,which=5)
plot(mod_lin,which=6)

anova(mod_lin)
summary(mod_lin)
summary(mod_quad)

#-- confronto tra i diversi modelli
grid <- seq(min(d$Disp.),max(d$Disp.),length.out = 40000)
plot(d$Disp.,d$Price,pch=19,cex=.7)
lines(grid,predict(mod_lin,data.frame(Disp.=grid)),col=2,lwd=2)
lines(grid,predict(mod_quad,data.frame(Disp.=grid)),col=3,lwd=2)
lines(grid,exp(predict(mod_lin_log,data.frame(Disp.=grid))),col=4,lwd=2)

hist(mod_lin$residuals)
hist(mod_quad$residuals)

shapiro.test(mod_lin$residuals)
shapiro.test(mod_quad$residuals)

plot(d$Disp.,mod_lin$residuals)
lines(lowess(d$Disp.,mod_lin$residuals)$x,lowess(d$Disp.,mod_lin$residuals)$y,col=2,lwd=3)


par( mfrow = c(2,1) )
curve(dnorm(x,5),0,10)
dnorm(1,4,4)

hist(d$Price,freq=F)
curve( dnorm(x,mean(d$Price),sd(d$Price)), add=T,col=2,lwd=3)

hist(log(d$Price),freq=F)
curve(dnorm(x,mean(log(d$Price)),sd(log(d$Price))),add=T,col=2,lwd=3)

shapiro.test(d$Price)
shapiro.test(log(d$Price))

white.test <- function(lmod,data=d){
  u2 <- lmod$residuals^2
  y <- fitted(lmod)
  Ru2 <- summary(lm(u2 ~ y + I(y^2)))$r.squared
  LM <- nrow(data)*Ru2
  p.value <- 1-pchisq(LM, 2)
  data.frame("Test statistic"=LM,"P value"=p.value)
}

FIND_EXTREME_OBSERVATION <- function(x,sd_factor=2){
  which(  x>mean(x)+sd_factor*sd(x) | 
            x<mean(x)-sd_factor*sd(x)
  )
}

out_y <- FIND_EXTREME_OBSERVATION(d$Price)
out_x <- FIND_EXTREME_OBSERVATION(d$Disp.)

plot(d$Disp.,d$Price,pch=19,cex=.7)
points(d$Disp.[out_y],d$Price[out_y],pch=19,cex=2,col=2)
points(d$Disp.[out_x],d$Price[out_x],pch=19,cex=2,col=3)

white.test(mod_lin)
white.test(mod_quad)
white.test(mod_lin_log)

dwtest(mod_lin)
dwtest(mod_quad)
dwtest(mod_lin_log)

mod_lin <- lm( Price ~ Disp.+ Mileage + Reliability + HP + Weight,d)
summary(mod_lin)

res <- ols_step_forward(mod_lin)
res$steps
res$re
res$predictors
plot(res)

res <- ols_step_backward(mod_lin)
res$removed
plot(res)

ols_vif_tol(mod_lin)

round(ols_eigen_cindex(mod_lin),2)

cor(d[,c("Disp.","Mileage","Weight")])



