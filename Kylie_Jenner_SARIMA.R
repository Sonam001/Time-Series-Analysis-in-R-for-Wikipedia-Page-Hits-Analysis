##################################################################################################
# Definig and plotting time series
#Plot the model
SARIMA_Kylie_Jenner.ts <- ts(read.table("Kylie_Jenner_Data.txt",header = TRUE)[,1],start=7/1/2015,freq=1)

par(mar=c(3.1,4.1,1.1,2.1),cex=0.8)
plot.ts(SARIMA_Kylie_Jenner.ts, xlab = "Days", ylab = "Kylie Jenner Wikipedia Webpage Hits")
#help(plot.ts)


#par(mfrow=c(1,1))
#SARIMA_Kylie_Jenner.ts <- ts(production$SARIMA_Kylie_Jenner,start =1958,frequency = 12)
#plot(SARIMA_Kylie_Jenner.ts, ylab = "SARIMA_Kylie_Jenner production")

# Log of time series transformation
SARIMA_Kylie_Jenner.ts.log <- log(SARIMA_Kylie_Jenner.ts)
plot(SARIMA_Kylie_Jenner.ts.log, ylab = "log SARIMA_Kylie_Jenner production")

# Converting to a stationary time series
diff.SARIMA_Kylie_Jenner.ts.log.ts <- diff(SARIMA_Kylie_Jenner.ts.log, differences = 1)
plot(diff.SARIMA_Kylie_Jenner.ts.log.ts)

# Removing the seasonality
d12.d1.SARIMA_Kylie_Jenner.ts.log.ts <- diff(diff.SARIMA_Kylie_Jenner.ts.log.ts, lag = 12)
plot(d12.d1.SARIMA_Kylie_Jenner.ts.log.ts)


# ACF and PACF values
par(mfrow=c(1,2))   
acf_SARIMA_Kylie_Jenner <- acf(d12.d1.SARIMA_Kylie_Jenner.ts.log.ts)
pacf_SARIMA_Kylie_Jenner <- pacf(d12.d1.SARIMA_Kylie_Jenner.ts.log.ts)
# 

# Fitting Best Model

n = length(SARIMA_Kylie_Jenner.ts.log)
max.p = 7
max.d = 1
max.q = 2
max.P = 2
max.D = 1
max.Q = 2
BIC.array =array(NA,dim=c(max.p+1,max.d+1,max.q+1,max.P+1,max.D+1,max.Q+1))
AIC.array =array(NA,dim=c(max.p+1,max.d+1,max.q+1,max.P+1,max.D+1,max.Q+1))
best.bic <- 1e8
x.ts = SARIMA_Kylie_Jenner.ts.log

for (p in 0:max.p) for(d in 0:max.d) for(q in 0:max.q) 
  for (P in 0:max.P) for(D in 0:max.D) for(Q in 0:max.Q) 
  {
    cat("p=",p,", d=",d,", q=",q,", P=",P,", D=",D,", Q=",Q,"\n")
    
    fit <- arima(x.ts, order = c(p,d,q),  
                 seas = list(order = c(P,D,Q), 
                             frequency(x.ts)),method="CSS-ML")
    number.parameters <- length(fit$coef) + 1
    BIC.array[p+1,d+1,q+1,P+1,D+1,Q+1] = -2*fit$loglik + log(n)*number.parameters
    AIC.array[p+1,d+1,q+1,P+1,D+1,Q+1] = -2*fit$loglik + 2*number.parameters
    
    if (BIC.array[p+1,d+1,q+1,P+1,D+1,Q+1] < best.bic) 
    {
      best.bic <- BIC.array[p+1,d+1,q+1,P+1,D+1,Q+1]
      best.fit <- fit
      best.model <- c(p,d,q,P,D,Q) 
    }
    
  }

best.bic
best.fit
best.model

# Best BIC model is [111102] with BIC of -783.5349
best_SARIMA_Kylie_Jenner_model <- arima(SARIMA_Kylie_Jenner.ts.log, order = c(0,0,0), seasonal = list(order=c(1,1,2),period=12),method = "ML")

number.parameters <- length(best_SARIMA_Kylie_Jenner_model$coef) + 1
-2*best_SARIMA_Kylie_Jenner_model$loglik + log(length(SARIMA_Kylie_Jenner.ts.log))*number.parameters


# Prediction of 36 lags using the best model
h <- 36
forecast <- predict(best_SARIMA_Kylie_Jenner_model,n.ahead = h)

n <- length(SARIMA_Kylie_Jenner.ts.log)
plot(c(SARIMA_Kylie_Jenner.ts.log,rep(NA,h)),type="l",ylim=c(8,12),ylab = "Log time series")
lines((n+1):(n+h),forecast$pred,col="blue")  
lines((n+1):(n+h),forecast$pred+1.96*forecast$se,lty=2,col="red") # Confidence Interval
lines((n+1):(n+h),forecast$pred-1.96*forecast$se,lty=2,col="red")


# Plotting the actual forecast
par(mfrow=c(1,1))
n <- length(SARIMA_Kylie_Jenner.ts)
plot(c(SARIMA_Kylie_Jenner.ts,rep(NA,h)),type = 'l',ylab = "SARIMA_Kylie_Jenner production", ylim=c(1000,80000))
lines((n+1):(n+h),exp(forecast$pred),col="blue")
lines((n+1):(n+h),exp(forecast$pred+1.96*forecast$se),lty=2,col="red") # Confidence Interval
lines((n+1):(n+h),exp(forecast$pred-1.96*forecast$se),lty=2,col="red")
