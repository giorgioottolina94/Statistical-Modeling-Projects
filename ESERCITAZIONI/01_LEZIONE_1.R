
set.seed(1234) #-- fisso seme generatore

n <- 200 #-- quanti dati simulare
x <- rnorm(n)
y <- 6 + 2*x + rnorm(n,sd=1) #-- outcome

dati <- data.frame(y,x) #-- dataset

plot(dati$x,dati$y) #-- scatter plot

#-- Descrittive generali
summary(dati$x)
boxplot(dati$y)
hist(y)

#-- stima modello lineare semplice
mod <- lm(y~x,dati)
mod$coefficients

dati$residuo <- mod$residuals #-- residui del modello
dati$fitted_values <- mod$fitted.values #-- valori fittati
dati$residuo_manuale <- dati$y - dati$fitted_values

dati$peso <- dati$y
dati$altezza <- dati$x

#-- y-X*beta
y[1]-mod$fitted.values[1]

plot(dati$x,dati$y)
abline(mod,col=2,lwd=3)

nuovi_dati <- data.frame(x=6)
predict(mod,nuovi_dati)

summary(mod)

(cbind(1,dati$x) %*% coef(mod) )[1:10]
mod$fitted.values[1:10]

hist(mod$residuals)
plot(dati$x,mod$residuals)


########################################


n <- 200 #-- quanti dati simulare
x <- rnorm(n,5,3) #-- x
y <- 2*x + 1/9*x^2+ rnorm(n,sd=1) #-- outcome

dati <- data.frame(y,x) #-- dataset

plot(dati$x,dati$y)

dati$x_quad <- dati$x^2

mod_lin <- lm( y ~ x,dati)
mod_quad <- lm( y ~ x + x_quad,dati)

summary(mod_lin)
summary(mod_quad)

grid <- seq(-5,20,length.out = 2000)

plot(dati$x,dati$y,pch=19,cex=.4)
lines(grid,predict(mod_lin,data.frame(x=grid)),col=2,lwd=2)
lines(grid,predict(mod_quad,data.frame(x=grid,x_quad=grid^2)),col=4,lwd=2)

range(x)

#-- import dati
d <- read.csv("C:\\Users\\sbarberis\\Dropbox\\MODELLI STATISTICI\\F. Esercizi(22) copia\\3.lin(5)\\1.linear\\car.test.txt",sep=" ")

names(d)
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


mod_lin <- lm( Price ~ Disp. + Type,d)
summary(mod_lin)

