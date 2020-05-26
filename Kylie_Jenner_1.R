


#Plot the model
Kylie_Jenner.ts <- ts(read.table("Kylie_Jenner_Data.txt",header = TRUE)[,1],start=7/1/2015,freq=1)

par(mar=c(3.1,4.1,1.1,2.1),cex=0.8)
plot.ts(Kylie_Jenner.ts, xlab = "Days", ylab = "Kylie Jenner Wikipedia Webpage Hits")
#help(plot.ts)

#Check mean

cat("Observed mean = ",mean(Kylie_Jenner.ts),".  Theoretial mean = ",0,"\n")
cat("Observed variance = ",var(Kylie_Jenner.ts))

###########################################################

#AR(p) model

#Log Transformation
log.Kylie_Jenner.ts <- log(Kylie_Jenner.ts)
plot(log.Kylie_Jenner.ts)

#Check mean

cat("Observed mean = ",mean(log.Kylie_Jenner.ts),".  Theoretial mean = ",0,"\n")
cat("Observed variance = ",var(log.Kylie_Jenner.ts))


#Fit AR(1) model
Kylie_Jenner.ts.ar <- ar(Kylie_Jenner.ts,order.max=1,method="mle")


log.Kylie_Jenner.ts.ar <- ar(log.Kylie_Jenner.ts,order.max=1,method="mle")

armaxorder <- ar(Kylie_Jenner.ts,aic= FALSE, method="mle")
armaxorder.aic <- ar(Kylie_Jenner.ts,aic= TRUE,method="mle")
ar.order1 <- ar(Kylie_Jenner.ts,order.max=1,method="mle")

Kylie_Jenner.ts.phi.MLE = optimize(log.Kylie_Jenner.ts, y=y, lower=-1, upper=1, maximum=TRUE)$maximum

armaxorder
armaxorder.aic
ar.order1

plot.ts((predict(armaxorder, n.ahead = 25)))
plot.ts((predict(armaxorder.aic, n.ahead = 25)))
plot.ts((predict(ar.order1, n.ahead = 25)))

#Prediction through AR1
ar(Kylie_Jenner.ts,order.max=1,method="mle")






Kylie_Jenner.ts.diff = Kylie_Jenner.ts[2:length(Kylie_Jenner.ts)] - Kylie_Jenner.ts[1:(length(Kylie_Jenner.ts)-1)]
plot(Kylie_Jenner.ts.diff,type="l")
cat("Observed mean = ",mean(log.Kylie_Jenner.ts.diff),".  Theoretial mean = ",0,"\n")
cat("Observed variance = ",var(log.Kylie_Jenner.ts.diff))



#help(ar)

# The series is non-stationary and has seasonality

#plot ACF
plot(acf(log.Kylie_Jenner.ts))

#plot PACF
plot(pacf(log.Kylie_Jenner.ts))


#We are going to model this time series using AR(1) model
log.Kylie_Jenner.ts.diff = log.Kylie_Jenner.ts[2:length(log.Kylie_Jenner.ts)] - log.Kylie_Jenner.ts[1:(length(log.Kylie_Jenner.ts)-1)]
plot(log.Kylie_Jenner.ts.diff,type="l")

cat("Observed mean = ",mean(log.Kylie_Jenner.ts.diff),".  Theoretial mean = ",0,"\n")
cat("Observed variance = ",var(log.Kylie_Jenner.ts.diff))


#plot ACF
plot(acf(log.Kylie_Jenner.ts.diff))

#plot PACF
plot(pacf(log.Kylie_Jenner.ts.diff))


armaxorder.aic <- ar(log.Kylie_Jenner.ts,aic= TRUE,method="mle")

plot.ts(armaxorder.aic)

armaxorder.aic


#Forecast
h=30
forecast.ar1 <- predict(log.Kylie_Jenner.ts.diff,n.ahead = h)

plot(forecast.ar1)

n <- length(log.Kylie_Jenner.ts.diff)
plot(log.Kylie_Jenner.ts.diff, type="l",ylim=c(0,100),,xlim =c(1955,1995), ylab = "Log time series")
lines(forecast$f, col="red")
lines(forecast$f+1.95*sqrt(unlist(forecast$Q)), col="blue")
lines(forecast$f-1.95*sqrt(unlist(forecast$Q)), col="blue")

# Prediction of 36 lags using the best model
h <- 36
forecast <- predict(best_AR_Kylie_Jenner_model,n.ahead = h)

n <- length(AR_Kylie_Jenner.ts.log)
plot(c(AR_Kylie_Jenner.ts.log,rep(NA,h)),type="l",ylim=c(8,12),ylab = "Log time series")
lines((n+1):(n+h),forecast$pred,col="blue")  
lines((n+1):(n+h),forecast$pred+1.96*forecast$se,lty=2,col="red") # Confidence Interval
lines((n+1):(n+h),forecast$pred-1.96*forecast$se,lty=2,col="red")


# Plotting the actual forecast
par(mfrow=c(1,1))
n <- length(AR_Kylie_Jenner.ts)
plot(c(AR_Kylie_Jenner.ts,rep(NA,h)),type = 'l',ylab = "AR_Kylie_Jenner production", ylim=c(1000,80000))
lines((n+1):(n+h),exp(forecast$pred),col="blue")
lines((n+1):(n+h),exp(forecast$pred+1.96*forecast$se),lty=2,col="red") # Confidence Interval
lines((n+1):(n+h),exp(forecast$pred-1.96*forecast$se),lty=2,col="red")


auto.arima(Kylie_Jenner.ts)

