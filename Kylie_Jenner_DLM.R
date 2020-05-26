
######################################################################

#DLM

# Chocolate Database
library('ggplot2')
library('forecast')
library('tseries')
require(dlm)

#chocolate.ts <- ts(read.table("chocolate_beer_electricity.txt",header = TRUE)[,1],start=1958,freq=12)
DLM_Kylie_Jenner.ts <- ts(read.table("Kylie_Jenner_Data.txt",header = TRUE)[,1],start=7/1/2015,freq=1)

#DLM_Kylie_Jenner.ts <- tsclean(Raw_DLM_Kylie_Jenner.ts)


par(mar=c(3.1,4.1,1.1,2.1),cex=0.8)
plot(DLM_Kylie_Jenner.ts)

#Using log transformation to account for seasonal pattern

log.DLM_Kylie_Jenner.ts <- log(DLM_Kylie_Jenner.ts)
par(mar=c(3.1,4.1,1.1,2.1),cex=0.8)
plot(log.DLM_Kylie_Jenner.ts)

adf.test(log.DLM_Kylie_Jenner.ts)
decomp = stl(log.DLM_Kylie_Jenner.ts, s.window = "periodic")


# The series is non-stationary and has seasonality

# We are going to model this time series using 2nd order polynomial trend and a Fourier representation with all Fourier frequencies
#Define the model and MLE of observational and system variances

###model <- function(parm) {
###  dlmModPoly(order = 2, dV = exp(parm[1]), dW = c(exp(parm[2]),exp(parm[3]))) + dlmModTrig(s = 12, dV = 0, dW=exp(parm[4]))
}
###fit.model <- dlmMLE(log.chocolate.ts, rep(0.1,4), model)
###fit.model$convergence
###unlist(model(fit.model$par)[c("V","W")])

model_Kylie_Jenner <- function(parm) {
  dlmModPoly(order = 2, dV = exp(parm[1]), dW = c(exp(parm[2]),exp(parm[3]))) + dlmModTrig(s = 12, dV = 0, dW=exp(parm[4]))
}
fit.model_Kylie_Jenner <- dlmMLE(log.DLM_Kylie_Jenner.ts, rep(0.1,4), model_Kylie_Jenner)
fit.model_Kylie_Jenner$convergence
unlist(model_Kylie_Jenner(fit.model_Kylie_Jenner$par)[c("V","W")])

#Define model using MLE

Kylie_Jenner_mod.MLE <- model_Kylie_Jenner(fit.model_Kylie_Jenner$par)

#Kalman filter
Kylie_Jennerfilt <- dlmFilter(log.DLM_Kylie_Jenner.ts, Kylie_Jenner_mod.MLE)

Kylie_Jennercov.filt <- with(fit.model_Kylie_Jenner, dlmSvd2var(U.C,D.C))

seas.term = 3

#Analysis of the seasonal Fourier frequencies indicates
#that we may need 5 or 6 harmonics. So little or no gain would be obtained from removing model.

#One-step ahead forecast errors
Kylie_Jenner_model.res <- residuals(Kylie_Jennerfilt, sd = FALSE)


#Plot one-step ahead forecast errors
plot(Kylie_Jenner_model.res, type='h'); abline(h=0)

#Plot ACF and PACG on one-step ahead forecast errors
acf(Kylie_Jenner_model.res, na.action = na.pass)
pacf(Kylie_Jenner_model.res, na.action = na.pass)

#Plot qq plot of one-step ahead forecast errors
qqnorm(Kylie_Jenner_model.res);qqline(Kylie_Jenner_model.res)
# Results show some lags are statistically significant

#Diagnostic Test
#Normality test with Shapiro-Wilk normality test
#Null hypothesis : errors are normally distributed

shapiro.test(Kylie_Jenner_model.res)

#P-value is large . So no departure form normality

#Ljung-Cox test to test the autocorrelation
#Null hypothesis : errors are independent

Box.test(Kylie_Jenner_model.res, lag=20, type="Ljung")
sapply(1:20,function(i)
  Box.test(Kylie_Jenner_model.res, lag = i, type ="Ljung-Box")$p.value)


# Prediction of 36 lags using the best model
h <- 36
Kylie_Jenner_forecast <- dlmForecast(Kylie_Jennerfilt,n = h)

plot(Kylie_Jenner_forecast$f)

n <- length(Kylie_Jennerfilt)
plot(log.DLM_Kylie_Jenner.ts, type="l",ylim=c(6,14),,xlim =c(1,900), ylab = "Log time series")
lines(Kylie_Jenner_forecast$f, col="red")
lines(Kylie_Jenner_forecast$f+1.95*sqrt(unlist(Kylie_Jenner_forecast$Q)), col="blue")
lines(Kylie_Jenner_forecast$f-1.95*sqrt(unlist(Kylie_Jenner_forecast$Q)), col="blue")

# Plotting the actual forecast
par(mfrow=c(1,1))
n <- length(DLM_Kylie_Jenner.ts)
plot(c(DLM_Kylie_Jenner.ts,rep(NA,h)),type = 'l',ylab = "Web page views", ylim=c(0,80000))
lines((n+1):(n+h),exp(Kylie_Jenner_forecast$f),col="blue")
lines((n+1):(n+h),exp(Kylie_Jenner_forecast$f+1.95*sqrt((unlist(Kylie_Jenner_forecast$Q)))),lty=2,col="red") # Confidence Interval
lines((n+1):(n+h),exp(Kylie_Jenner_forecast$f-1.95*sqrt((unlist(Kylie_Jenner_forecast$Q)))),lty=2,col="red")