dati<-read.csv("SuicideChina.csv")
str(dati)  #struttura dei dati


head(dati)

summary(dati)

#attribuzione livelli 
Morto<-factor(dati$Died, levels=c("no","yes"), labels=c("no","yes"))
Educazione<-factor(dati$Education, levels=c("iliterate","primary","Secondary","unknown"),
                   labels=c("iliterate","primary","Secondary","unknown"))
Metodo<-factor(dati$method, levels=c("Others","Cutting","Drowning","Hanging","Jumping","Other poison","Pesticide","Poison unspec","unspecified"),
               labels=c("Others","Cutting","Drowning","Hanging","Jumping","Other poison","Pesticide","Poison unspec","unspecified"))
Ricoverato<-factor(dati$Hospitalised, levels=c("no","yes"),
                   labels=c("no","yes"))
Area_Urbana<-factor(dati$Urban, levels=c("no","yes"),
                    labels=c("no","yes"))
Sesso<-factor(dati$Sex, levels=c("male","female"),
              labels=c("male","female"))
Occupazione<-factor(dati$Occupation, levels=c("others" ,"business/service","farming","household","others/unknown","professional","retiree","student","unemployed"),
                    labels=c("others" ,"business/service","farming","household","others/unknown","professional","retiree","student","unemployed"))
Età<-dati$Age

#attribuzione NA 
getmode <- function(v) {
uniqv <- unique(v)
uniqv[which.max(tabulate(match(v, uniqv)))]
}

Educazione[is.na(Educazione)]<-getmode(Educazione)
Metodo[is.na(Metodo)]<-getmode(Metodo)
Morto[is.na(Morto)]<-getmode(Morto)
Ricoverato[is.na(Ricoverato)]<-getmode(Ricoverato)
Area_Urbana[is.na(Area_Urbana)]<-getmode(Area_Urbana)
Sesso[is.na(Sesso)]<-getmode(Sesso)
Età[is.na(Età)]<-mean(Età)
Occupazione[is.na(Occupazione)]<-getmode(Occupazione)

table(Morto)
cols<-c("darkblue","orange")
pie(table(Morto), main="Morte",col=cols)
labs<-c(levels(Morto))
plot(Morto)


summary(Età)
boxplot(Età, ylab="età")
hist(Età)

#campo di variazione
max(Età) - min(Età) 


#distanza interquartilica
IQR(Età)

#calcolo varianza e deviazione standard
var(Età)
sd(Età)

#moda
getmode(Età)

table(Educazione)
cols<-c("darkblue","lightyellow","orange","lightblue")
pie(table(Educazione), main="Educazione", col=cols)


plot(Educazione)

table(Ricoverato)
cols<-c("red","lightblue")
pie(table(Ricoverato), main="Ricovero", col=cols)
plot(Ricoverato)

table(Metodo)
plot(Metodo)        

table(Area_Urbana)
cols<-c("darkblue","lightyellow")
pie(table(Area_Urbana), main="Area_Urbana", col=cols)
plot(Area_Urbana)

table(Occupazione)
plot(Occupazione)  

#sesso
table(Sesso)
cols<-c("red","lightblue")
pie(table(Sesso), main="Sesso", col=cols)
plot(Sesso)

y<-Morto

#eta e sesso
fit.y.e.s<-glm(y~Età+Sesso,family=binomial(link=logit))
summary(fit.y.e.s)

#+area urbana
fit.y.e.s.a<-glm(y~Età+Sesso+Area_Urbana,family=binomial(link=logit))
summary(fit.y.e.s.a)


#+ Ricoverato
fit.y.e.s.r<-glm(y~Età+Sesso+Ricoverato,family=binomial(link=logit))
summary(fit.y.e.s.r)

fit.y.e.s.m<-glm(y~Età+Sesso+Metodo,family=binomial(link=logit))
summary(fit.y.e.s.m)

anova(fit.y.e.s, fit.y.e.s.m, test="Chisq")  

#ricodificazione categorie
summary(Metodo)
Metodo<-as.character(Metodo)
Metodo[which(Metodo=="Other poison" | Metodo =="Poison unspec")]<-"Poison"
Metodo<-factor(Metodo, levels=c("Others","Cutting","Drowning","Hanging","Jumping","Poison","Pesticide","unspecified"),labels=c("Others","Cutting","Drowning","Hanging","Jumping","Poison","Pesticide","unspecified"))
levels(Metodo)
summary(Metodo)

Metodo<-as.character(Metodo)
Metodo[which(Metodo=="unspecified")] <-"Others"
Metodo<-factor(Metodo, levels=c("Others","Pesticide","Cutting","Drowning","Hanging","Jumping","Poison"),labels=c("Others","Pesticide","Cutting","Drowning","Hanging","Jumping","Poison"))
levels(Metodo)
summary(Metodo)

fit.y.e.s.m<-glm(y~Età+Sesso+Metodo,family=binomial(link=logit))
summary(fit.y.e.s.m)

#ulteriore ricodificazione ( Pesticide e Poison sono causa di avvelenamento "Poisoning")
Metodo<-as.character(Metodo)
Metodo[which(Metodo=="Pesticide" | Metodo=="Poison")] <-"Poisoning"
Metodo<-factor(Metodo, levels=c("Others","Poisoning","Cutting","Drowning","Hanging","Jumping"),labels=c("Others","Poisoning","Cutting","Drowning","Hanging","Jumping"))
levels(Metodo)
summary(Metodo)
Metodo<-as.character(Metodo)
Metodo[which(Metodo=="Hanging" | Metodo=="Drowning")] <-"Choking"
Metodo<-factor(Metodo, levels=c("Others","Poisoning","Cutting","Choking","Jumping"),labels=c("Others","Poisoning","Cutting","Choking","Jumping"))
levels(Metodo)
summary(Metodo)
Metodo<-as.character(Metodo)
Metodo[which(Metodo=="Cutting" | Metodo=="Jumping")] <-"Physical_Injury"
Metodo<-factor(Metodo, levels=c("Others","Poisoning","Choking","Physical_Injury"),labels=c("Others","Poisoning","Choking","Physical_Injury"))
levels(Metodo)
summary(Metodo)
plot(Metodo)

fit.y.e.s.m<-glm(y~Età+Sesso+Metodo,family=binomial(link=logit))
summary(fit.y.e.s.m)

fit.y.e.s.m.o<-glm(y~Età+Sesso+Metodo+Occupazione,family=binomial(link=logit))
summary(fit.y.e.s.m.o)
anova(fit.y.e.s.m, fit.y.e.s.m.o, test="Chisq")

summary(Occupazione)
Occupazione<-as.character(Occupazione)
Occupazione[which(Occupazione=="others/unknown")] <-"others"
Occupazione<-factor(Occupazione, levels=c("others" ,"business/service","farming","household","professional","retiree","student","unemployed"), labels=c("others" ,"business/service","farming","household","professional","retiree","student","unemployed"))
summary(Occupazione)

Occupazione<-as.character(Occupazione)
Occupazione[which(Occupazione=="retiree")] <-"others"
Occupazione<-factor(Occupazione, levels=c("others" ,"business/service","farming","household","professional","student","unemployed"), labels=c("others" ,"business/service","farming","household","professional","student","unemployed"))
summary(Occupazione)

plot(Occupazione)

fit.y.e.s.m.o<-glm(y~Età+Sesso+Metodo+Occupazione,family=binomial(link=logit))
summary(fit.y.e.s.m.o)

fit.y.e.s.m.o.e<-glm(y~Età+Sesso+Metodo+Occupazione+Educazione,family=binomial(link=logit))
summary(fit.y.e.s.m.o.e)
anova(fit.y.e.s.m.o, fit.y.e.s.m.o.e, test="Chisq")


(modello_finale<-fit.y.e.s.m.o.e)

prova<-glm(y~Età+Sesso*Area_Urbana+Metodo+Occupazione+Educazione,family=binomial(link=logit))
summary(prova)

#metodo:
table(Morto, Metodo)
plot(Metodo, Morto, pch = 16, xlab = "Metodo", ylab = "Morte")


#Sesso:
table(Morto,Sesso)
plot(Sesso, Morto, pch = 16, xlab = "Metodo", ylab = "Morte")


#Occupazione:
table(Morto, Occupazione)
plot(Occupazione, Morto, xlab = "Metodo", ylab = "Morte")

#Educazione
table(Morto, Educazione)
plot(Educazione, Morto, pch = 16, xlab = "Metodo", ylab = "Morte")

table(Educazione, Occupazione)

tab <- table(y, modello_finale$fitted >.5)                     
tab

sum(diag(tab))/sum(tab) # percentuale di corretta classificazione

summary(residuals.glm(modello_finale))
plot(residuals.glm(modello_finale), col="orange", pch=20, xlab="Osservazioni", ylab="Residui")
abline(h=0)

library(generalhoslem)
hl<-logitgof(modello_finale$y, fitted(modello_finale))
hl

#intervalli di confidenza per i parametri B ,e per exp(B)
ICB<-confint(modello_finale)
ICexpB<-exp(confint(modello_finale))
stim<-cbind(modello_finale$coefficients,ICB, exp(modello_finale$coefficients),  ICexpB)
stim<-rbind(c("Parametri","IC lower","IC upper","Exp(Parametri)","IC lower","IC upper"),stim)
stim

funzione_IC_Pigreco<-function(x) {
  se<-x$se.fit
  x<-x$fit
  #se<-as.numeric(se)
  #x<-as.numeric(x)
  low=x-(1.96*se)
  upp=x+(1.96*se)
  ic=c(low,upp)
  return(ic)
}

p1<-predict.glm(modello_finale,type= "response",newdata = data.frame(Età=20,Sesso="female",Metodo="Poisoning", Occupazione = "student", Educazione="primary"),se.fit=T)
p1$fit
funzione_IC_Pigreco(p1)

p2<-predict.glm(modello_finale,type= "response",newdata = data.frame(Età=20,Sesso="male",Metodo="Poisoning", Occupazione = "student", Educazione="primary"),se.fit=T)
p2$fit
funzione_IC_Pigreco(p2)

p3<-predict.glm(modello_finale,type= "response",newdata = data.frame(Età=20,Sesso="female",Metodo="Choking", Occupazione = "household", Educazione="iliterate"),se.fit=T)
p3$fit
funzione_IC_Pigreco(p3)

p4<-predict.glm(modello_finale,type= "response",newdata = data.frame(Età=20,Sesso="female",Metodo="Physical_Injury", Occupazione = "household", Educazione="iliterate"),se.fit=T)
p4$fit
funzione_IC_Pigreco(p4)

p5<-predict.glm(modello_finale,type= "response",newdata = data.frame(Età=20,Sesso="female",Metodo="Poisoning", Occupazione = "household", Educazione="iliterate"),se.fit=T)
p5$fit
funzione_IC_Pigreco(p5)

