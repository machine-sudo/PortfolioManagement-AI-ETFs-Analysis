#matrix to get data
install.packages("PerformanceAnalytics")
install.packages("pdfetch")
install.packages('moments')
install.packages("corrplot")
install.packages("car")
install.packages("lmtest")
install.packages("tidyverse")
install.packages("dplyr")
library(pdfetch)
library(moments)
library(MASS)
library(car)
library(lmtest)
library(psych)
library(corrplot)
library(PerformanceAnalytics)
library(tidyverse)
library(glue)
library(dplyr)
library(lubridate)
library(readxl)


# Describe the distribution of the weekly returns using descriptive statistics
# Index Data & Log Return
ticker = c("^GSPC","^IRX")
index   = pdfetch_YAHOO(ticker, fields = "adjclose", from="2014-01-01", to="2019-01-01", interval= "1w")
dj = read_xls("Dow Jones Technology.xls")
dj1=dj[1637:2936,3]
logdj=dj1[1:260,1]
sum=1
for(val in 1:260) {
  logdj[val,1]=100*log(dj1[sum+4,1]/dj1[sum,1]) 
  
  sum=sum+5
}
logsp   = na.omit(diff(log(index[,1]))*100)
allindex= data.frame(logsp,logdj,index[-1,2])
names(allindex) = c("SP500", "Dow Jones Tech","RF")
view(allindex)

# ETFs Data
tickers = c("XLK","QQQ","PNQI","ROBO","IGM")
etf = pdfetch_YAHOO(tickers,fields = "adjclose",
                    from="2014-01-01", to="2019-01-01", interval= "1w")
View (etf)

#log return etfs
logrtn = na.omit(diff(log(etf))*100)
View(logrtn)


# A. Central tendency, dispersion and shape of distributions
#Method 1
etfsts <- function (r){
  mean<-mean(r)
  median<-median(r)
  quantile1<-unname(quantile(r,.25))
  quantile3<-unname(quantile(r,.75))
  std<-sd(r)
  skewness<-unname(skewness(r))
  kurtosis<-unname(kurtosis(r))
  return(c(Mean = mean, Median = median, FstQuantile = quantile1, ThrdQuantile = quantile3,
           STD = std, Skewness = skewness, Kurtosis = kurtosis))
}

Report<- sapply(logrtn,etfsts)
Report

#Method 2
#summary(logrtn)
#describe(logrtn)
chart.Correlation(logrtn,method="pearson")

hist(logrtn[,1],prob = TRUE,breaks = 20,col = "lightblue", 
     border = "pink", main = paste("XLK Distribution"),
     xlab = "Log Return", ylab = "Density")
lines(density(logrtn[,1],lwd = 5))
curve(dnorm(x,mean=mean(logrtn[,1]),sd = sd(logrtn[,1])),add = TRUE)

hist(logrtn[,2],prob = TRUE,breaks = 20,col = "lightblue", 
     border = "pink", main = paste("QQQ Distribution"),
     xlab = "Log Return", ylab = "Density")
lines(density(logrtn[,2],lwd = 5))
curve(dnorm(x,mean=mean(logrtn[,2]),sd = sd(logrtn[,2])),add = TRUE)

hist(logrtn[,3],prob = TRUE,breaks = 20,col = "lightblue", 
     border = "pink", main = paste("PNG Distribution"),
     xlab = "Log Return", ylab = "Density")
lines(density(logrtn[,3],lwd = 5))
curve(dnorm(x,mean=mean(logrtn[,3]),sd = sd(logrtn[,3])),add = TRUE)

hist(logrtn[,4],prob = TRUE,breaks = 20,col = "lightblue", 
     border = "pink", main = paste("ROBO Distribution"),
     xlab = "Log Return", ylab = "Density")
lines(density(logrtn[,4],lwd = 5))
curve(dnorm(x,mean=mean(logrtn[,4]),sd = sd(logrtn[,4])),add = TRUE)


hist(logrtn[,5],prob = TRUE,breaks = 20,col = "lightblue", 
     border = "pink", main = paste("IGM Distribution"),
     xlab = "Log Return", ylab = "Density")
lines(density(logrtn[,5],lwd = 5))
curve(dnorm(x,mean=mean(logrtn[,5]),sd = sd(logrtn[,5])),add = TRUE)



# Analyze the correlations, and variance/covariance of the fund returns
# B. Var/CoVar Corr Matrix for all ETFs. Compare risks and performance with index
#Correlation - Covariance Matrix opt 1 by data.frame
funds  <- data.frame(logrtn,logsp,logdj)
etfs   <- c("XLK","QQQ","PNQI","ROBO","IGM","S&P 500","Dow Jones")
names(funds) = etfs
covmatrix    = matrix(c(cov(funds)), nrow = 7, ncol = 7)
dimnames(covmatrix)     = list(etfs,etfs)
covmatrix
cor(funds)
cormat  <- cor(funds)
corrplot(cormat)



# Analyze the tracking error of each ETF.
# Conduct hypothesis test for the mean and variance of returns between each ETF and its tracked index.
#C. Tracking error, hypothesis test for mean and variance of returns
allindex2 = xts(allindex,order.by =as.Date(rownames(allindex)))
"Tracking Error between XLK compare to SP500"
TrackingError(logrtn[,1],allindex2[,1])

"Tracking Error between QQQ compare to SP500"
TrackingError(logrtn[,2],allindex2[,1])

"Tracking Error between PNQI compare to SP500"
TrackingError(logrtn[,3],allindex2[,1])

"Tracking Error between ROBO compare to SP500"
TrackingError(logrtn[,4],allindex2[,1])

"Tracking Error between IGM compare to SP500"
TrackingError(logrtn[,5],allindex2[,1])

"Tracking Error between XLK compare to DowJones"
TrackingError(logrtn[,1],allindex2[,2])

"Tracking Error between QQQ compare to DowJones"
TrackingError(logrtn[,2],allindex2[,2])

"Tracking Error between PNQI compare to DowJones"
TrackingError(logrtn[,3],allindex2[,2])

"Tracking Error between ROBO compare to DowJones"
TrackingError(logrtn[,4],allindex2[,2])

"Tracking Error between IGM compare to DowJones"
TrackingError(logrtn[,5],allindex2[,2])

#Hypothesis
XLK <- array(logrtn[,1])
QQQ <- array(logrtn[,2])
PNQI <- array(logrtn[,3])
ROBO <- array(logrtn[,4])
IGM <- array(logrtn[,5])
GSPC <- array(allindex[,1])
DJT <- array(allindex[,2])

#hypothesis testing for mean & variance of returns between XLK, GSPC & XLK,DJT
var.test(XLK,GSPC)
var.test(XLK,DJT)
t.test(XLK,GSPC,var.test=FALSE)
t.test(XLK,DJT,var.test= FALSE)

#hypothesis testing for mean & variance of returns between QQQ, GSPC & QQQ,DJT
var.test(QQQ,GSPC)
var.test(QQQ,DJT)
t.test(QQQ,GSPC,var.test=FALSE)
t.test(QQQ,DJT,var.test= FALSE)

#hypothesis testing for mean & variance of returns between PNQI, GSPC & PNQI,DJT
var.test(PNQI,GSPC)
var.test(PNQI,DJT)
t.test(PNQI,GSPC,var.test=FALSE)
t.test(PNQI,DJT,var.test= FALSE)

#hypothesis testing for mean & variance of returns between ROBO, GSPC & ROBO,DJT
var.test(ROBO,GSPC)
var.test(ROBO,DJT)
t.test(ROBO,GSPC,var.test=FALSE)
t.test(ROBO,DJT,var.test= FALSE)

#hypothesis testing for mean & variance of returns between IGM, GSPC & IGM,DJT
var.test(IGM,GSPC)
var.test(IGM,DJT)
t.test(IGM,GSPC,var.test=FALSE)
t.test(IGM,DJT,var.test= FALSE)




# Compute the excess returns of each ETF and the two indexes separately. 
# Use the factor model to estimate the alphas and betas.
# (Regress each fund excess returns on market excess return over the same time period, respectively). 
#D. Regression
options(scipen=100) #remove e
rf = allindex[,3]/52


#excess return for etfs
excessxlk  = logrtn[,1] - rf
excessqqq  = logrtn[,2] - rf
excesspnqi = logrtn[,3] - rf
excessrobo = logrtn[,4] - rf
excessigm  = logrtn[,5] - rf

#excess return for index
excesssp   = allindex[,1]- rf
excessdj   = allindex[,2]- rf

excess  = cbind(excessxlk,excessqqq,excesspnqi,excessrobo,excessigm,excesssp,excessdj)
names(excess)=c("XLK","QQQ","PNQI","ROBO","IGM","SP","DowJonesTech")

#regression XLK SP500
xlksp <- lm(XLK~SP, data = excess)
summary(xlksp)
plot(y= excessxlk,x= excesssp,pch = 10, cex = 0.5, col = "red",
     ylab = "XLK",xlab = "SP500", main = "Linear Regression XLK~SP500")
abline(lm(xlksp))

#4 graphs
layout(matrix(c(1,2,3,4),2,2))  
plot(xlksp)
#Heteroskadasticity Test XLKSP
bptest(xlksp)

#regression QQQ SP500
qqqsp  <- lm(QQQ~SP, data = excess)
summary(qqqsp)
plot(y= excessqqq,x= excesssp,pch = 10, cex = 0.5, col = "blue",
     ylab = "QQQ",xlab = "SP500", main = "Linear Regression QQQ~SP500")
abline(lm(qqqsp))
#4 graphs
layout(matrix(c(1,2,3,4),2,2))  
plot(qqqsp)
#Heteroskadasticity Test QQQSP
bptest(qqqsp)

#regression PNQI SP500
pnqisp <- lm(PNQI~SP, data = excess)
summary(pnqisp)
plot(y= excesspnqi,x= excesssp,pch = 10, cex = 0.5, col = "green",
     ylab = "PNQI", xlab = "SP500", main = "Linear Regression PNQI~SP500")
abline(lm(pnqisp))
#4 graphs
layout(matrix(c(1,2,3,4),2,2))  
plot(pnqisp)
#Heteroskadasticity Test PNQISP
bptest(pnqisp)

#regression ROBO SP500
robosp <- lm(ROBO~SP, data = excess)
summary(robosp)
plot(y= excessrobo,x= excesssp,pch = 10, cex = 0.5, col = "purple",
     ylab = "ROBO",xlab = "SP500", main = "Linear Regression ROBO~SP500")
abline(lm(robosp))
#4 graphs
layout(matrix(c(1,2,3,4),2,2))  
plot(robosp)
#Heteroskadasticity Test ROBOSP
bptest(robosp)

#regression IGM SP500
igmsp  <- lm(IGM~SP, data = excess)
summary(igmsp)
plot(y= excessigm,x= excesssp,pch = 10, cex = 0.5, col = "orange",
     ylab = "IGM",xlab = "SP500", main = "Linear Regression IGM~SP500")
abline(lm(igmsp))
#4 graphs
layout(matrix(c(1,2,3,4),2,2))  
plot(igmsp)
#Heteroskadasticity Test IGMSP
bptest(igmsp)

#how strong the model is for Rsquare 
#regression XLK DJTEC
xlkdj  <- lm(XLK~DowJonesTech, data = excess)
summary(xlkdj)
plot(y= excessxlk,x= excessdj,pch = 10, cex = 0.5, col = "red",
     ylab = "XLK",xlab = "DowJonesTech", main = "Linear Regression XLK~DowJones")

abline(lm(xlkdj))
#4 graphs
layout(matrix(c(1,2,3,4),2,2))  
plot(xlkdj)
#Heteroskadasticity Test XLKDJ
bptest(xlkdj)

#regression QQQ DJTEC
qqqdj <- lm(QQQ~DowJonesTech, data = excess)
summary(qqqdj)
plot(y= excessqqq,x= excessdj,pch = 10, cex = 0.5, col = "blue",
     ylab = "QQQ",xlab = "DowJonesTech", main = "Linear Regression QQQ~DowJones")
abline(lm(qqqdj))
#4 graphs
layout(matrix(c(1,2,3,4),2,2))  
plot(qqqdj)
#Heteroskadasticity Test QQQDJ
bptest(qqqdj)

#regression PNQI DJTEC
pnqidj  <- lm(PNQI~DowJonesTech, data = excess)
summary(pnqidj)
plot(y= excesspnqi,x= excessdj,pch = 10, cex = 0.5, col = "green",
     ylab = "PNQI",xlab = "DowJonesTech", main = "Linear Regression PNQI~DowJones")
abline(lm(pnqidj))
#4 graphs
layout(matrix(c(1,2,3,4),2,2))  
plot(pnqidj)
#Heteroskadasticity Test PNQIDJ
bptest(pnqidj)


#regression ROBO DJTEC
robodj  <- lm(ROBO~DowJonesTech, data = excess)
summary(robodj)
plot(y= excessrobo,x= excessdj,pch = 10, cex = 0.5, col = "purple",
     ylab = "ROBO",xlab = "DowJonesTech", main = "Linear Regression ROBO~DowJones")
abline(lm(robodj))
#4 graphs
layout(matrix(c(1,2,3,4),2,2))  
plot(robodj)
#Heteroskadasticity Test ROBODJ
bptest(robodj)

#regression IGM DJTEC
igmdj  <- lm(IGM~DowJonesTech, data = excess)
summary(igmdj)
plot(y= excessigm,x= excessdj,pch = 10, cex = 0.5, col = "orange",
     ylab = "IGM",xlab = "DowJonesTech", main = "Linear Regression IGM~DowJones")
abline(lm(igmdj))
#4 graphs
layout(matrix(c(1,2,3,4),2,2))  
plot(igmdj)
#Heteroskadasticity Test IGMDJ
bptest(igmdj)

#E. lm(excess~1+2+3)
#	Use the Fama-French 3 factors to estimate the alphas and betas
# FAMA FRENCH 3 FACTOR MERGE

fama      = read.csv("F-F_Research_Data_Factors_weekly.CSV",header=TRUE)
famasmall = fama[4566:4826,]
mergedata <- data.frame(famasmall[-1,],excess)
mergedata$Date <- NULL 
View(mergedata)

xlkfama   <-lm(excessxlk ~ Mkt.RF + SMB + HML, data = mergedata) 
summary(xlkfama)  #FF 3 Factors XLK

qqqfama   <-lm(excessqqq ~ Mkt.RF + SMB + HML, data = mergedata) 
summary(qqqfama)  #FF 3 Factors QQQ

pnqifama   <-lm(excesspnqi ~ Mkt.RF + SMB + HML, data = mergedata) 
summary(pnqifama)  #FF 3 Factors PNQI

robofama   <-lm(excessrobo ~ Mkt.RF + SMB + HML, data = mergedata) 
summary(robofama)  #FF 3 Factors ROBO

igmfama   <-lm(excessigm ~ Mkt.RF + SMB + HML, data = mergedata) 
summary(igmfama)  #FF 3 Factors IGM

dbxlk =as.data.frame(cbind(excessxlk,mergedata$Mkt.RF,mergedata$SMB,mergedata$HML))
names(dbxlk)=c("excessxlk","Mkt.RF","SMB","HML")
db1=lm(excessxlk~Mkt.RF+SMB+HML, data=dbxlk)
durbinWatsonTest(db1)

dbqqq =as.data.frame(cbind(excessqqq,mergedata$Mkt.RF,mergedata$SMB,mergedata$HML))
names(dbqqq)=c("excessqqq","Mkt.RF","SMB","HML")
db2=lm(excessqqq~Mkt.RF+SMB+HML, data=dbqqq)
durbinWatsonTest(db2)

dbpnqi=as.data.frame(cbind(excesspnqi,mergedata$Mkt.RF,mergedata$SMB,mergedata$HML))
names(dbpnqi)=c("excesspnqi","Mkt.RF","SMB","HML")
db3=lm(excesspnqi~Mkt.RF+SMB+HML, data=dbpnqi)
durbinWatsonTest(db3)

dbrobo=as.data.frame(cbind(excessrobo,mergedata$Mkt.RF,mergedata$SMB,mergedata$HML))
names(dbrobo)=c("excessrobo","Mkt.RF","SMB","HML")
db4=lm(excessrobo~Mkt.RF+SMB+HML, data=dbrobo)
durbinWatsonTest(db4)

dbigm =as.data.frame(cbind(excessigm,mergedata$Mkt.RF,mergedata$SMB,mergedata$HML))
names(dbigm)=c("excessigm","Mkt.RF","SMB","HML")
db5=lm(excessigm~Mkt.RF+SMB+HML, data=dbigm)
durbinWatsonTest(db5)

#Use r-square and ANOVA table to evaluate the overall model goodness of fit. 
#Examine whether the ETF earn significant abnormal return beyond the market or the technology sector. 
#Anova Test
anova(xlkfama)

anova(qqqfama)

anova(pnqifama)

anova(robofama)

anova(igmfama)




