library(car)
library(sjstats)
library(plotrix)
library(sjPlot)
library(sjmisc)
library(lme4)
library(pander)
library(car)
library(olsrr)
library(systemfit)
library(het.test)
panderOptions('knitr.auto.asis', FALSE)

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

#-- ESERCIZIO 1
ABSOLUTE_PATH <- "C:\\Users\\sbarberis\\Dropbox\\MODELLI STATISTICI"
d <- read.csv(paste0(ABSOLUTE_PATH,"\\esercizi (5)  copia\\1.mult\\countries.txt"),sep="\t")
VAR_NUMERIC <- c("Life.expectancy","Unemployment","Literacy.Rate","ISPs.million","Irrigated","Under.14")

summary(d[,VAR_NUMERIC])
cor(d[,VAR_NUMERIC])
plot(d[,VAR_NUMERIC],pch=19,cex=.5)

par(mfrow=c(2,3))
for(i in VAR_NUMERIC){
  boxplot(d[,i],main=i,col="lightblue",ylab=i)
}

#-- modello per Life.expectancy
mod1 <- lm(Life.expectancy ~ ISPs.million + Irrigated + Under.14 + Literacy.Rate, d)
summary(mod1)
anova(mod1)
white.test(mod1)
dwtest(mod1)
plot(mod1,which=2,pch=19)

hist(resid(mod1),col="lightblue",freq=F,xlab="Resid",main="")
lines(density(resid(mod1)),col=2,lwd=2)

#-- modello per Unemployment
mod2 <- lm(Unemployment ~ ISPs.million + Irrigated + Under.14 + Literacy.Rate, d)
summary(mod2)
anova(mod2)
white.test(mod2)
dwtest(mod2)
plot(mod2,which=2,pch=19)

hist(resid(mod2),col="lightblue",freq=F,xlab="Resid",main="")
lines(density(resid(mod2)),col=2,lwd=2)

#-- regressione multivariata
mod3 <- lm(cbind(Unemployment,Life.expectancy) ~ ISPs.million + Irrigated + Under.14 + Literacy.Rate, d)
summary(mod3)
Anova(mod3, type="III")

summary(manova(cbind(Life.expectancy, Unemployment) ~ Irrigated, data = d))
summary(manova(cbind(Life.expectancy, Unemployment) ~ Under.14, data = d))
summary(manova(cbind(Life.expectancy, Unemployment) ~ Literacy.Rate, data = d))


#-- ESERCIZIO 2

ABSOLUTE_PATH <- "C:\\Users\\sbarberis\\Dropbox\\MODELLI STATISTICI"
d <- read.csv(paste0(ABSOLUTE_PATH,"\\esercizi (5)  copia\\3.mult\\nazioni.csv"),sep=";")
#d$relig <- factor(d$relig,1:3,c("catt","ortod","prot"))
d$dummy_cat <- ifelse(d$relig==1,1,0)
d$dummy_ort <- ifelse(d$relig==2,1,0)
d$dummy_prot <- ifelse(d$relig==3,1,0)

#-- vettore di variabili numeriche presenti nei dati
VAR_NUMERIC <- c("densita","urbana","alfabet","pil")

cor(d[,VAR_NUMERIC])

mod1 <- lm(vitamas ~ densita + urbana + alfabet + pil + dummy_ort + dummy_prot, d)
summary(mod1)
anova(mod1)

mod2 <- lm(vitafem ~ densita + urbana + alfabet + pil + dummy_ort + dummy_prot, d)
summary(mod2)
anova(mod2)

mod3 <- lm(cbind(vitamas,vitafem) ~ densita + urbana + alfabet + pil + dummy_ort + dummy_prot, d)

summary(mod3)
pander(manova(mod3))
aov(mod3)

summary(manova(cbind(vitamas,vitafem) ~ densita, data = d))
summary(manova(cbind(vitamas,vitafem) ~ urbana, data = d))
summary(manova(cbind(vitamas,vitafem) ~ alfabet, data = d))
summary(manova(cbind(vitamas,vitafem) ~ pil, data = d))
summary(manova(cbind(vitamas,vitafem) ~ dummy_ort, data = d))
summary(manova(cbind(vitamas,vitafem) ~ dummy_prot, data = d))



#-- Modello SURE

#-- import dei dati
ABSOLUTE_PATH <- "C:\\Users\\sbarberis\\Dropbox\\MODELLI STATISTICI"
d <- read.csv(paste0(ABSOLUTE_PATH,"\\esercizi (5)  copia\\4.mult\\Cigarette.txt"),sep=" ")

#-- vettore di variabili numeriche presenti nei dati
VAR_NUMERIC <- c("cpi","pop","packpc","income","tax")

d_ca <- d[d$state=="CA",]
names(d_ca) <- paste0(names(d_ca),"_CA")

d_ar <- d[d$state=="AR",]
names(d_ar) <- paste0(names(d_ar),"_AR")

d1 <- cbind(d_ar,d_ca)

d_tx <- d[d$state=="TX",]
names(d_tx) <- paste0(names(d_tx),"_TX")

d2 <- cbind(d_tx,d_ca)

mod1_AR <- lm(packpc_AR ~ cpi_AR + pop_AR + income_AR + tax_AR, d1)
summary(mod1_AR)
anova(mod1_AR)

mod1_CA <- lm(packpc_CA ~ cpi_CA + pop_CA + income_CA + tax_CA, d1)
summary(mod1_CA)
anova(mod1_CA)

cor(data.frame(resid(mod1_CA),resid(mod1_AR)))


#-- -ora considero la correlazione fra gli stessi individui nelle diverse equazioni
e1 <- packpc_AR ~ cpi_AR + pop_AR + income_AR + tax_AR
e2 <- packpc_CA ~ cpi_CA + pop_CA + income_CA + tax_CA
sistema <- list(e1=e1,e2=e2)

mod1 <- systemfit(sistema,"SUR",data=d1)
summary(mod1)
coef(mod1)

#-- testo se cpl_AR (eq 1) = tax_CA (eq 2)
R1 <- matrix(0,nrow=1,ncol=10)
R1[ 1, 2 ] <- 1
R1[ 1, 10 ] <- -1
colnames(R1) <- names(coef(mod1))

linearHypothesis(mod1,R1,test="FT")

#-- TEST: clp_AR = cpl_CA
R1 <- matrix(0,nrow=1,ncol=10)
R1[ 1, 2 ] <- 1
R1[ 1, 7 ] <- -1
linearHypothesis(mod1,R1,test="FT")

#-- TEST: tax_AR = tax_CA
R2 <- matrix(0,nrow=1,ncol=10)
R2[ 1, 5 ] <- 1
R2[ 1, 10 ] <- -1
linearHypothesis(mod1,R2,test="FT")

#-- Ora modello SURE con diversi regressori tra equazioni

mod1_AR <- lm(packpc_AR ~ cpi_AR + income_AR + tax_AR, d1)
summary(mod1_AR)
anova(mod1_AR)

  
mod1_CA <- lm(packpc_CA ~ cpi_CA + pop_CA, d1)
summary(mod1_CA)
anova(mod1_CA)

cor(data.frame(resid(mod1_CA),resid(mod1_AR)))


e1 <- packpc_AR ~ cpi_AR + income_AR + tax_AR
e2 <- packpc_CA ~ cpi_CA + pop_CA
sistema <- list(e1=e1,e2=e2)

mod1 <- systemfit(sistema,"SUR",data=d1)
summary(mod1)

linearHypothesis(mod1,"e1_cpi_AR = -249.217",test="FT")
linearHypothesis(mod1,"e1_tax_AR = -1.43426",test="FT")
linearHypothesis(mod1,"e2_cpi_CA = -67.7451",test="FT")




#-- Gasoline

ABSOLUTE_PATH <- "C:\\Users\\sbarberis\\Dropbox\\MODELLI STATISTICI"
d <- read.csv(paste0(ABSOLUTE_PATH,"\\esercizi (5)  copia\\5.mult\\data.csv"),sep=";")

d_au <- d[d$COUNTRY=="AUSTRIA",]
names(d_au) <- paste0(names(d_au),"_AU")

d_be <- d[d$COUNTRY=="BELGIUM",]
names(d_be) <- paste0(names(d_be),"_BE")

d1 <- cbind(d_au,d_be)

#-- vettore di variabili numeriche presenti nei dati
VAR_NUMERIC <- c("LGASPCAR_AU","LINCOMEP_AU","LRPMG_AU","LCARPCAP_AU","LGASPCAR_BE","LINCOMEP_BE","LRPMG_BE","LCARPCAP_BE")


mod1_AU <- lm(LGASPCAR_AU ~ LCARPCAP_AU + LINCOMEP_AU + LRPMG_AU, d1)
summary(mod1_AU)
anova(mod1_AU)

mod1_BE <- lm(LGASPCAR_BE ~ LCARPCAP_BE + LINCOMEP_BE + LRPMG_BE, d1)
summary(mod1_BE)
anova(mod1_BE)

cor(data.frame(resid(mod1_BE),resid(mod1_AU)))


e1 <- LGASPCAR_AU ~ LCARPCAP_AU + LINCOMEP_AU + LRPMG_AU
e2 <- LGASPCAR_BE ~ LCARPCAP_BE + LINCOMEP_BE + LRPMG_BE
sistema <- list(e1=e1,e2=e2)

mod1 <- systemfit(sistema,"SUR",data=d1)
summary(mod1)

linearHypothesis(mod1,"e1_LINCOMEP_AU = e2_LINCOMEP_BE",test="FT")
linearHypothesis(mod1,"e1_LCARPCAP_AU = e2_LCARPCAP_BE",test="FT")
linearHypothesis(mod1,"e1_LRPMG_AU = e2_LRPMG_BE",test="FT")

#-- regressori diversi
mod1_BE <- lm(LGASPCAR_BE ~ LCARPCAP_BE + LINCOMEP_BE, d1)
mod1_AU <- lm(LGASPCAR_AU ~ LCARPCAP_AU + LINCOMEP_AU + LRPMG_AU, d1)

e1 <- LGASPCAR_AU ~ LCARPCAP_AU + LINCOMEP_AU + LRPMG_AU
e2 <- LGASPCAR_BE ~ LCARPCAP_BE + LINCOMEP_BE
sistema <- list(e1=e1,e2=e2)

mod1 <- systemfit(sistema,"SUR",data=d1)
summary(mod1)

linearHypothesis(mod1,"e1_LCARPCAP_AU = -0.5199",test="FT")
linearHypothesis(mod1,"e2_LCARPCAP_BE = -0.6687",test="FT")


########################################################################## MODELLO MULTILEVEL

ABSOLUTE_PATH <- "C:\\Users\\sbarberis\\Dropbox\\MODELLI STATISTICI"
d <- read.csv(paste0(ABSOLUTE_PATH,"\\esercizi (3) copia\\1.multilevel\\CASchools.txt"),sep=" ")

table(d$county)
length(unique(d$county))

#-- vettore di variabili numeriche presenti nei dati
VAR_NUMERIC <- names(d)[6:ncol(d)]

mod1 <- lmer(math ~ 1 + (1 | county),d,REML=T) #-- empty model
summary(mod1)
deviance(mod1)

Anova(mod1, type="III")
data.frame("ICC"=icc(mod1)) #-- ICC

mod1_null <- lm(math ~ 1,d) #-- modello nullo
anova(mod1,mod1_null) #-- test del rapporto di verosimiglianza



res <- sjp.lmer(mod1, type = "re.qq", sort.est = "sort.all",show.values=T,title="T",prnt.plot=F)
res$data$lower <- res$data$y-res$data$ci
res$data$upper <- res$data$y+res$data$ci

res$data[1:10,c("ID","y","upper","lower")]
mean(res$data$y)

plotCI(1:nrow(res$data),res$data$y,ui=res$data$upper, li=res$data$lower,pch=19,scol="blue",xlab="Country",ylab="Estimate",xaxt="n")
abline(h=mean(res$data$y),col=2,lwd=3,lty=2)

plot(1:nrow(d),d$math,pch=19,cex=.7)
points(which(d$county=="Sonoma"),d$math[d$county=="Sonoma"],col=2,pch=19,cex=2)
abline(h=653.7)
abline(h=653.7+res$data$y[res$data$ID=="Sonoma"],col=2,lwd=2)


mod1 <- lmer(math ~ read + (read| county),d,REML=F)
res <- sjp.lmer(mod1, type = "re.qq", sort.est = "sort.all",show.values=T,title="T",prnt.plot=F)
res$data$lower <- res$data$y-res$data$ci
res$data$upper <- res$data$y+res$data$ci

plot(d$read,d$math,pch=19,cex=.7)
points(d$read[d$county=="Sonoma"],d$math[d$county=="Sonoma"],col=2,pch=19,cex=2)
abline(a=72.778,b= 0.886,col="blue")
abline(a=72.778+res$data$y[res$data$ID=="Sonoma"][1],b=0.886+res$data$y[res$data$ID=="Sonoma"][2],col=2,lwd=2)




mod1 <- lmer(math ~ calworks + read + calworks*read + (1| county),d,REML=F)
summary(mod1)
Anova(mod1, type="III")

data.frame("ICC"=icc(mod1)) #-- ICC 

mod1 <- lmer(math ~ calworks + read + calworks*read + (calworks| county),d,REML=T)
summary(mod1)
pander(Anova(mod1, type="III"),big.mark=",")

res <- sjp.lmer(mod1, type = "re.qq", sort.est = "sort.all",show.values=T,title="T",prnt.plot=F)
res$data$upper <- res$data$y+res$data$ci
res$data$lower <- res$data$y-res$data$ci

res_int <- subset(res$data,ind=="(Intercept)")
res_hw <- subset(res$data,ind=="calworks")

pander(res_int[1:10,c("ID","y","upper","lower")],big.mark=",")
pander(res_hw[1:10,c("ID","y","upper","lower")],big.mark=",")

plotCI(1:nrow(res_int),res_int$y,ui=res_int$upper, li=res_int$lower,pch=19,scol="blue",xlab="Country",ylab="Estimate",main="Intercept")
abline(h=mean(res_int$y),col=2,lwd=3,lty=2)

plotCI(1:nrow(res_hw),res_hw$y,ui=res_hw$upper, li=res_hw$lower,pch=19,scol="blue",xlab="Country",ylab="Estimate",main="Calworks")
abline(h=mean(res_hw$y),col=2,lwd=3,lty=2)



