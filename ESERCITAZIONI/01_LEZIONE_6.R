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


#### MODELLO MULTILEVEL

ABSOLUTE_PATH <- "C:\\Users\\sbarberis\\Dropbox\\MODELLI STATISTICI"
d <- read.csv(paste0(ABSOLUTE_PATH,"\\esercizi (3) copia\\1.multilevel\\CASchools.txt"),sep=" ")

table(d$county)
length(unique(d$county))

#-- vettore di variabili numeriche presenti nei dati
VAR_NUMERIC <- names(d)[6:ncol(d)]

mod1 <- lmer(math ~ 1 + (1 | county),d,REML=T) #-- empty model
summary(mod1)

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


mod1 <- lmer(math ~ read + (read | county),d,REML=F)
summary(mod1)
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


#-- Esercizio 2

ABSOLUTE_PATH <- "C:\\Users\\sbarberis\\Dropbox\\MODELLI STATISTICI"
d <- read.csv(paste0(ABSOLUTE_PATH,"\\esercizi (3) copia\\2.multilevel\\imm10.csv"),sep=";")

#-- fisso scuola di riferimento
d$schnum <- factor(d$schnum)
contrasts(d$schnum) <- contr.treatment(levels(d$schnum),base=which(levels(d$schnum) == '10'))

VAR_NUMERIC <- c("homework","percmin","math","meanses")

boxplot(d$math~d$schid,main="Math by school",col="lightblue",ylab="math")

mod1 <- lm(math ~ schnum,d)
summary(mod1)


mod1 <- lmer(math ~ (1| schnum),d,REML=T)
summary(mod1)
Anova(mod1, type="III")

mod1_null <- lm(math ~ 1,d) 
anova(mod1,mod1_null)

data.frame("ICC"=icc(mod1)) #-- ICC 

res <- sjp.lmer(mod1, type = "re.qq", sort.est = "sort.all",show.values=T,title="T",prnt.plot=F)
res$data$lower <- res$data$y-res$data$ci
res$data$upper <- res$data$y+res$data$ci

pander(res$data[1:10,c("ID","y","upper","lower")])
plotCI(1:nrow(res$data),res$data$y,ui=res$data$upper, li=res$data$lower,pch=19,scol="blue",xlab="School",ylab="Estimate",main="Intercept")
abline(h=mean(res$data$y),col=2,lwd=3,lty=2)

#-- Random intercept
mod1 <- lmer(math ~ homework + (1| schnum),d,REML=T)
summary(mod1)
Anova(mod1, type="III")
data.frame("ICC"=icc(mod1))

res <- sjp.lmer(mod1, type = "re.qq", sort.est = "sort.all",show.values=T,title="T",prnt.plot=F)
res$data$lower <- res$data$y-res$data$ci
res$data$upper <- res$data$y+res$data$ci

pander(res$data[1:10,c("ID","y","upper","lower")],big.mark=",")
plotCI(1:nrow(res$data),res$data$y,ui=res$data$upper, li=res$data$lower,pch=19,scol="blue",xlab="School",ylab="Estimate",main="Intercept")
abline(h=mean(res$data$y),col=2,lwd=3,lty=2)



mod1 <- lmer(math ~ homework + ses + (1| schnum),d,REML=T)
summary(mod1)
pander(Anova(mod1, type="III"),big.mark=",")
pander(data.frame("ICC"=icc(mod1)),big.mark=",") #-- ICC 

res <- sjp.lmer(mod1, type = "re.qq", sort.est = "sort.all",show.values=T,title="T",prnt.plot=F)
res$data$lower <- res$data$y-res$data$ci
res$data$upper <- res$data$y+res$data$ci

pander(res$data[1:10,c("ID","y","upper","lower")],big.mark=",")
plotCI(1:nrow(res$data),res$data$y,ui=res$data$upper, li=res$data$lower,pch=19,scol="blue",xlab="School",ylab="Estimate",main="Intercept")
abline(h=mean(res$data$y),col=2,lwd=3,lty=2)



#-- random slope
mod1 <- lmer(math ~ homework + ses + (homework| schnum),d,REML=T)
summary(mod1)
pander(Anova(mod1, type="III"),big.mark=",")

res <- sjp.lmer(mod1, type = "re.qq", sort.est = "sort.all",show.values=T,title="T",prnt.plot=F)
res$data$lower <- res$data$y-res$data$ci
res$data$upper <- res$data$y+res$data$ci

res_int <- subset(res$data,ind=="(Intercept)")
res_hw <- subset(res$data,ind=="homework")

pander(res_int[1:10,c("ID","y","upper","lower")])
pander(res_hw[1:10,c("ID","y","upper","lower")])

plotCI(1:nrow(res_int),res_int$y,ui=res_int$upper, li=res_int$lower,pch=19,scol="blue",xlab="School",ylab="Estimate",main="Intercept")
abline(h=mean(res_int$y),col=2,lwd=3,lty=2)

plotCI(1:nrow(res_hw),res_hw$y,ui=res_hw$upper, li=res_hw$lower,pch=19,scol="blue",xlab="School",ylab="Estimate",main="Homework")
abline(h=mean(res_hw$y),col=2,lwd=3,lty=2)



#-- Esercizio 3

ABSOLUTE_PATH <- "C:\\Users\\sbarberis\\Dropbox\\MODELLI STATISTICI"
d <- read.csv(paste0(ABSOLUTE_PATH,"\\esercizi (3) copia\\3.multilevel\\Exam.txt"),sep=" ")
d$school <- factor(d$school)
contrasts(d$school) <- contr.treatment(levels(d$school),base=which(levels(d$school) == '65'))

VAR_NUMERIC <- c("normexam","schavg","standLRT")


boxplot(d$normexam~d$school,main="Normexam by school",col="lightblue",ylab="Normexam")

mod1 <- lm(normexam ~ school,d)


mod1 <- lmer(normexam ~ (1| school),d,REML=T)
summary(mod1)
Anova(mod1,type="III")

mod1_null <- lm(normexam ~ 1,d) 
anova(mod1,mod1_null)

data.frame("ICC"=icc(mod1)) #-- ICC 

res <- sjp.lmer(mod1, type = "re.qq", sort.est = "sort.all",show.values=T,title="T",prnt.plot=F)
res$data$lower <- res$data$y-res$data$ci
res$data$upper <- res$data$y+res$data$ci

pander(res$data[1:10,c("ID","y","upper","lower")])
plotCI(1:nrow(res$data),res$data$y,ui=res$data$upper, li=res$data$lower,pch=19,scol="blue",xlab="School",ylab="Estimate",main="Intercept")
abline(h=mean(res$data$y),col=2,lwd=3,lty=2)


mod1 <- lmer(normexam ~ sex + intake + standLRT + (1| school),d,REML=T)
summary(mod1)
Anova(mod1, type="III")
data.frame("ICC"=icc(mod1))

res <- sjp.lmer(mod1, type = "re.qq", sort.est = "sort.all",show.values=T,title="T",prnt.plot=F)
res$data$lower <- res$data$y-res$data$ci
res$data$upper <- res$data$y+res$data$ci

pander(res$data[1:10,c("ID","y","upper","lower")])
plotCI(1:nrow(res$data),res$data$y,ui=res$data$upper, li=res$data$lower,pch=19,scol="blue",xlab="School",ylab="Estimate",main="Intercept")
abline(h=mean(res$data$y),col=2,lwd=3,lty=2)


mod1 <- lmer(normexam ~ vr + intake + sex + type + schgend + (1| school),d,REML=T)
summary(mod1)
pander(Anova(mod1, type="III"),big.mark=",")
pander(data.frame("ICC"=icc(mod1)),big.mark=",") #-- ICC 

res <- sjp.lmer(mod1, type = "re.qq", sort.est = "sort.all",show.values=T,title="T",prnt.plot=F)
res$data$lower <- res$data$y-res$data$ci
res$data$upper <- res$data$y+res$data$ci

pander(res$data[1:10,c("ID","y","upper","lower")])
plotCI(1:nrow(res$data),res$data$y,ui=res$data$upper, li=res$data$lower,pch=19,scol="blue",xlab="School",ylab="Estimate",main="Intercept")
abline(h=mean(res$data$y),col=2,lwd=3,lty=2)


#-- Random slope


mod1 <- lmer(normexam ~ standLRT + schavg + (standLRT| school),d,REML=T)
summary(mod1)
Anova(mod1, type="III")
data.frame("ICC"=icc(mod1))

res <- sjp.lmer(mod1, type = "re.qq", sort.est = "sort.all",show.values=T,title="T",prnt.plot=F)
res$data$lower <- res$data$y-res$data$ci
res$data$upper <- res$data$y+res$data$ci

res_int <- subset(res$data,ind=="(Intercept)")
res_hw <- subset(res$data,ind=="standLRT")

pander(res_int[1:10,c("ID","y","upper","lower")])
pander(res_hw[1:10,c("ID","y","upper","lower")])

plotCI(1:nrow(res_int),res_int$y,ui=res_int$upper, li=res_int$lower,pch=19,scol="blue",xlab="School",ylab="Estimate",main="Intercept")
abline(h=mean(res_int$y),col=2,lwd=3,lty=2)

plotCI(1:nrow(res_hw),res_hw$y,ui=res_hw$upper, li=res_hw$lower,pch=19,scol="blue",xlab="School",ylab="Estimate",main="StandLRT")
abline(h=mean(res_hw$y),col=2,lwd=3,lty=2)






