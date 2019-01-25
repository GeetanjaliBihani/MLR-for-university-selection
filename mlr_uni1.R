##PROJECT

#data prep
setwd('/Users/geetanjalibihani/Downloads/')
adm<-as.data.frame(read.csv('Admission_Predict.csv', header=T))
head(adm)
adm1<-adm[,-1]
colnames(adm1)<-c('GRE', 'TOEFL','University_Rating','SOP','LOR','CGPA','Research','chance')
head(adm1)
plot(adm1)
boxplot(adm1[,1:7])
cor(adm1[,1:7])
adm<-adm1


head(adm)
 #fit first order lm
admmod<-lm(chance~GRE+TOEFL+University_Rating+SOP+LOR+CGPA+Research, data=adm1)
anova(admmod)
summary(admmod)
library(car)
residualPlots(admmod)
qqnorm(residuals(admmod))
qqline(residuals(admmod))
rs<-rstandard(admmod)
par(mfrow=c(2,2))
plot(admmod)

#diagnostic
library(onewaytests)
adm1$fit<-admmod$fitted.values
adm1$resid<-admmod$residuals
adm1$group<-cut(adm1$fit,5)
bf.test(resid~group,adm1) ##constant variance
shapiro.test(adm1$resid) ##non normality issue

##best model selection by subsets
library(ALSM)
step(lm(chance~GRE+TOEFL+University_Rating+SOP+LOR+CGPA+Research, data=adm), method='both', trace=TRUE)
##reduced model removes sop and uni ranking 
admreduced<-lm(chance~GRE+TOEFL+LOR+CGPA+Research, data=adm)
admfull<-admmod
cpc(admreduced,admfull)
AICp(admreduced)
SBCp(admreduced)
pressc(admreduced)
par(mfrow=c(2,3))
plotmodel.s(adm[,1:7],adm$chance)
BestSub(adm[,1:7],adm$chance,num=1)##reduced model removes sop and uni ranking 

##adressing multicollinearity
  #Considering interactions
adminteract1<-lm(chance~GRE+TOEFL+University_Rating+SOP+LOR+CGPA+Research+GRE:TOEFL+TOEFL:CGPA+CGPA:GRE+GRE:TOEFL:CGPA,data=adm)
summary(adminteract1)
summary(admfull)
##not including interaction because not significant i.e. large p values

#trying ridge regression to address multicollinearity
library(lmridge)
admr<-lmridge(chance~GRE+TOEFL+LOR+CGPA+Research, data=adm, K=seq(0,1,0.02))
plot(admr)
vif(admr)
summary(lmridge(chance~GRE+TOEFL+LOR+CGPA+Research, data=adm, K=0.18))
summary(lmridge(chance~GRE+TOEFL+LOR+CGPA+Research, data=adm, K=0.2))
##We select the model with K=0.18 as its VIF values are closest to 1 for all the variables, moreover, AIC and BIC values are also small for this. Laslty parameter estimates are also stable

#working on reduced model because ridge ki maa ki
anova(admreduced)
summary(admreduced)
library(car)
residualPlots(admreduced)
qqnorm(residuals(admreduced))
qqline(residuals(admreduced))
library(onewaytests)
adm2<-adm
adm2$fit<-admreduced$fitted.values
adm2$resid<-admreduced$residuals
adm2$group<-cut(admreduced$fit,5)
bf.test(resid~group,adm2) ##constant variance
shapiro.test(adm2$resid)
##curvilinear relation, constant variance and non normality means transform x, but starting with y
x_matrix<-as.matrix(cbind(adm[,c(1,2,5,6)]))
logx<-log10(x_matrix)
admnew<-as.data.frame(cbind(logx[,1],logx[,2],logx[,3],logx[,4],adm$Research,adm$chance))
head(admnew)
colnames(admnew)<-c('GRE','TOEFL','LOR','CGPA','Research','Chance')
head(admnew)
admnewmodel<-lm(Chance~GRE+TOEFL+LOR+CGPA+Research, data=admnew)

#doing diagnostic tests
library(onewaytests)
adm2<-adm
adm2$fit<-admnewmodel$fitted.values
adm2$resid<-admnewmodel$residuals
adm2$group<-cut(admnewmodel$fit,5)
bf.test(resid~group,adm2) ##constant variance
shapiro.test(adm2$resid)


#residual plots again
rs<-rstandard(admnewmodel)
par(mfrow=c(2,2))
plot(admnewmodel)

##transforming y
bcmle=boxcox(lm(Chance~GRE+TOEFL+LOR+CGPA+Research, data=admnew),lambda = seq(-3,3,by=0.1))
lambda=bcmle$x[which.max(bcmle$y)]
lambda
admreducednew<-lm(chance^2~GRE+TOEFL+LOR+CGPA+Research, data=admnew)
qqnorm(admreducednew$residuals)
qqline(residuals(admreducednew))


##data summary
adm<-as.data.frame(read.csv('Admission_Predict.csv', header=T))
head(adm)
adm1<-adm[,-1]
colnames(adm1)<-c('GRE', 'TOEFL','University_Rating','SOP','LOR','CGPA','Research','chance')
head(adm1)
plot(adm1)
boxplot(adm1[,1:7])
cor(adm1[,1:7])
adm<-adm1

head(adm)
GRE<-summary(adm$GRE)
TOEFL<-summary(adm$TOEFL)
University_Rating<-summary(adm$University_Rating)
SOP<-summary(adm$SOP)
LOR<-summary(adm$LOR)
CGPA<-summary(adm$CGPA)
Research<-summary(adm$Research)
Chance<-summary(adm$chance)

i<-rbind(GRE,TOEFL,University_Rating,SOP,LOR,CGPA,Research,Chance)

i

adm$GRE

install.packages('psych')
library(psych)
par(mar=c(4,2,2,1))
par(mfrow=c(2,2))
hist(x=adm$GRE, xlab='GRE', ylab='number of rows', main='Distribution of GRE data points', col='light blue', labels=TRUE)
hist(x=adm$TOEFL, xlab='TOEFL', ylab='number of rows', main='Distribution of TOEFL data points', col='red', labels=TRUE)
hist(x=adm$University_Rating, xlab='University_Rating', ylab='number of rows', main='Distribution of University_Rating data points', col='green', labels=TRUE)
hist(x=adm$SOP, xlab='SOP', ylab='number of rows', main='Distribution of SOP data points', col='blue', labels=TRUE)
par(mar=c(4,2,2,1))
par(mfrow=c(2,2))
hist(x=adm$LOR, xlab='GRE', ylab='number of rows', main='Distribution of LOR data points', col='grey', labels=TRUE)
hist(x=adm$CGPA, xlab='GRE', ylab='number of rows', main='Distribution of CGPA data points', col='white', labels=TRUE)
hist(x=adm$Research, xlab='GRE', ylab='number of rows', main='Distribution of Research data points', col='pink', labels=TRUE)
hist(x=adm$chance, xlab='GRE', ylab='number of rows', main='Distribution of chance data points', col='purple', labels=TRUE)


