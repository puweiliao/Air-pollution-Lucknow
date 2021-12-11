rm(list=objects())
setwd("~/Desktop")
library(xts)
alldata<-read.csv("city_hour.csv", header=TRUE)
data1<-alldata[,c("City","Datetime","NOx")] #avant le virgule on spécifie les lignes
data2<-data1[which(data1$City =="Lucknow"),]
data1718<-data2[which(data2$Datetime=="2017-09-26 18:00:00"):which(data2$Datetime=="2018-12-31 23:00:00"),]
data19<-data2[which(data2$Datetime=="2019-01-01 00:00:00"):which(data2$Datetime=="2019-12-31 23:00:00"),]

Date = as.POSIXct(strptime(data1718$Datetime, "%Y-%m-%d %H:%M:%S", tz="UTC"))
NOx<-data1718[,c("NOx")]
NOx<-na.locf(NOx)
data1718<-data.frame(Date, NOx)

#analyse descriptive
boxplot(data1718$NOx) 
summary(data1718$NOx)
hist(data1718$NOx) #fréquence de NOx contenue dans l'air


#Représentation graphique de séries temporelles
plot(data1718$Date, data1718$NOx, type = "l", xaxt = "n", xlab = "", ylab = "NOx", col = "royalblue3")
axis.POSIXct(1, at = seq(data1718$Date[1], tail(data1718$Date, 1), "weeks"), format = "%d/%m/%y", las = 2)


#plot ts avec fréquence par semaine
NOx.ts <- ts(data1718$NOx, start = 1, frequency =12) 
plot(NOx.ts, col = "violetred2")


#croisement par facteur
heure<-as.factor(format(data1718$Date,"%H"))
hourlyNOx<-tapply(data1718$NOx,heure,mean)
plot(hourlyNOx,type="b")


boxplot(data1718$NOx ~ heure, col = "lightblue", pch = 20, cex = 0.5)



#tendance
#tendance par mobile moyenne

#Pour une période d'un an(=4320 heures)
P = 4320
MA<-filter(data1718$NOx, filter=array(1/P,dim=P), method = c("convolution"), sides = 2, circular = FALSE)
MA<-xts(MA,order.by=data1718$Date)
plot(data1718$NOx,type='l') 
plot(MA,col='red')

#une estimation de la tendance de la série par régression linéaire
t=1:(length(data1718$NOx))
reg<-lm(data1718$NOx~t+I(t^2))
reg<-lm(data1718$NOx~t)
summary(reg)
ychap.lm <- reg$fitted
plot(as.numeric(reg$residuals), type='l')
ychap.lm<-xts(as.numeric(reg$fitted),order.by=data1718$Date)    
# ychap.lm<-xts(reg$fitted,order.by=Date)
plot(data1718$NOx,type='l')
lines(ychap.lm,col='blue')



#une estimation de la tendance de la série par noyau
noyau <- ksmooth(data1718$Date, data1718$NOx, kernel = c("normal"), bandwidth = 10)
par(mfrow = c(1, 2))
plot(data1718$Date, data1718$NOx, type = "l", xlab = "",ylab = "", col = "blue") 
lines(data1718$Date, noyau$y, col = "red", lwd = 1) 
plot(data1718$Date, data1718$NOx - noyau$y, type = "l",xlab = "", ylab = "", col = "orangered2")


#une estimation de la tendance de la série par poly locaux
par(mfrow=c(1,1))
lo <- loess(data1718$NOx ~ t, data = data1718, degree = 2, span = 0.7) 
plot(data1718$Date, data1718$NOx, type = "l", xlab = " ",ylab = "", col = "blue") 
lines(data1718$Date, lo$fitted, col = "orangered2", lwd = 2)



#Modification de la série par la méthode des polynomes locaux

NOx.detrend = data1718-lo$fitted
plot(data1718$Date, data1718$NOx, type = "l", xlab = " ",ylab = "", col = "blue") 
lines(NOx.detrend, col = "green")

#saisonnalité
#moyenne mobile

K = 24*31 #sur une période d'un mois
MA <- filter(NOx.detrend$NOx, filter = array(1/K, dim = K), method = c("convolution"), sides = 2, circular = FALSE)
plot(data1718$Date,NOx.detrend$NOx , type = "l", xlab = "", ylab = "consumption (kw)", col = "seagreen4", lwd = 1)
plot(data1718$Date,MA, col = "red","l")



#saisonnalité sur un mois
NOxMonth = NOx.detrend$NOx[1:744]
Km = 12 #sur une période d'un mois
MAm <- filter(NOxMonth, filter = array(1/Km, dim = Km), method = c("convolution"), sides = 2, circular = FALSE)
plot(data1718$Date[1:744],MAm,"l")

#Stationnarité de la série
#On prend les données pour laquelle on a enlever la tendance

NOx_acf = acf(NOx.detrend$NOx)
NOx_pcaf = pacf(NOx.detrend$NOx)
#NOx_pcaf$acf



#lissage
#lissage simple
expSmooth=function(x,alpha)
{
  xsmooth=x
  for(i in c(2:length(x)))
  {
    xsmooth[i]<-(1-alpha)*xsmooth[i-1]+alpha*x[i]
  }
  return(xsmooth)
}
alpha<-0.2
NOx.smooth<-expSmooth(data1718$NOx,alpha)
plot(data1718$Date,data1718$NOx,type='l')
lines(data1718$Date,NOx.smooth,col='red', lwd=2)



n<-length(data1718$NOx)
alpha <-seq(0.05,0.95,length=100)
forecast<-lapply(alpha,expSmooth,x=data1718$NOx)
str(forecast)


erreur<-unlist(
  lapply(forecast,
         function(x){mean((tail(data1718$NOx,n-1)-head(x,n-1))^2)}))

plot(alpha,erreur,type='l')
X1.smooth<-expSmooth(data1718$NOx,alpha[which.min(erreur)])
plot(data1718$NOx,type='l')
lines(X1.smooth,col='red')


#lissage double
DoubleExpSmooth=function(x,alpha)
{
  xsmooth=x
  l<-array(x[1],dim=length(x))
  b<-array(x[2]-x[1],dim=length(x))
  
  for(i in c(2:length(x)))
  {
    l[i]<-xsmooth[i-1]+(1-(1-alpha)^2)*(x[i]-xsmooth[i-1])
    b[i]<-b[i-1]+alpha^2*(x[i]-xsmooth[i-1])
    xsmooth[i]<-l[i]+b[i]
  }
  
  res<-list()
  res$smooth<-xsmooth
  res$l=l
  res$b<-b
  return(res)
}

alpha<-seq(0.05,0.95,length=100)
forecast<-lapply(alpha,DoubleExpSmooth,x=data1718$NOx)
erreur<-unlist(
  lapply(forecast,
         function(x){mean((tail(data1718$NOx,n-1)-head(x$smooth,n-1))^2)}))
plot(alpha,erreur,type='l')

NOx.smooth<-DoubleExpSmooth(data1718$NOx,alpha[which.min(erreur)])
plot(data1718$Date,data1718$NOx,type='l')
lines(data1718$Date,NOx.smooth$smooth,col='red')

plot(data1718$Date,NOx.smooth$l,type='l',ylim=range(NOx.smooth$l,NOx.smooth$b),col='blue')
lines(data1718$Date,NOx.smooth$b,col='red')




#lissage double saisonnier de Holt-Winters
SeasonalDoubleExpSmooth=function(x,alpha,beta,delta,T)
{
  xsmooth=x
  l<-array(x[2],dim=length(x))
  b<-array(x[2]-x[1],dim=length(x))
  s<-array(x[1],dim=length(x))
  
  for(i in c(2:length(x)))
  {
    l[i]<-alpha*(x[i]-s[max(i-T,1)])+(1-alpha)*(l[i-1]+b[i-1])
    b[i]<-beta*(l[i]-l[i-1])+(1-beta)*b[i-1]
    s[i]<-delta*(x[i]-l[i])+(1-delta)*s[max(i-T,1)]
    xsmooth[i]<-l[i]+b[i]+s[i]
  }
  
  res<-list()
  res$smooth<-xsmooth
  res$l=l
  res$b<-b
  res$s<-s
  return(res)
}


alpha <- 0.2
beta <- 0.2
delta <- 0.2
T <- 4464 #période de 6mois
X.seas.exp.mooHW <- SeasonalDoubleExpSmooth(data1718$NOx,alpha,beta,delta,T)
plot(data1718$Date,data1718$NOx, type='l')
lines(data1718$Date,X.seas.exp.mooHW$smooth, col='red')
#plot(data1718$Date,X.seas.exp.mooHW$b, type='l')
#plot(data1718$Date,X.seas.exp.mooHW$l, type='l')
#plot(data1718$Date,X.seas.exp.mooHW$s, type='l')



#prévision  à l'horizon h
predict.expSmooth<-function(Xsmooth,inst,horizon,smooth.type="double")
{
  
  if(smooth.type=="simple")
  {
    n<-length(Xsmooth)
    prev<-c(Xsmooth[1:inst],rep(Xsmooth[inst],horizon))
  }
  
  if(smooth.type=="double")
  {
    n<-length(Xsmooth$smooth)
    prev<-c(Xsmooth$smooth[1:inst],Xsmooth$l[inst]+Xsmooth$b[inst]*c(1:horizon))
  }
  return(prev)
}


alpha=0.2
X.d.exp.mooHW<-DoubleExpSmooth(data1718$NOx,alpha)
prevHW<-predict.expSmooth(X.d.exp.mooHW,
                          inst=length(data1718$NOx),horizon=100,smooth.type="double")

#plot(data19$NOx, col = 'blue',type="l")
#plot(prevHW[length(data1718$NOx)+1:length(prevHW)], col = "green")
plot(data1718$NOx,pch=20,ylim=range(data1718$NOx,prevHW))
lines(prevHW,col='red',lwd=2)
abline(v=90,lty='dashed')




#prévision
#exp simple
last_obs = length(data1718$NOx)
horizon = 3000

plot(1:last_obs, data1718$NOx[1:last_obs], type="l", xlab="Time", ylab="NOx")
lines((last_obs+1):(last_obs+horizon), data1718$NOx[(last_obs+1):(last_obs+horizon)], col="red")
legend("topleft", legend=c("Training data", "Forecasting data"), lty=1,
       col=c("black", "red"))

alpha = seq(0.05,0.95,length=100)

smoothing = lapply(alpha, ExpSmooth, x=X3[1:last_obs])

erreur = sapply(smoothing, mse_smoothing_simple, true_values=X3[1:last_obs])

smoothing = ExpSmooth(X3[1:last_obs], alpha[which.min(erreur)])
forecast = c(smoothing, rep(smoothing[last_obs], horizon))

plot(X3, pch=20, ylim=range(X3, forecast), xlab="Time", ylab=expression(X^3))
lines(forecast, col='red', lwd=2)
abline(v=last_obs, lty='dashed')
legend("topleft", legend=c("Observed data", "Smoothed and forecast"), lty=c(NA,1), pch=c(20,NA),
       lwd=c(1,2), col=c("black", "red"))

