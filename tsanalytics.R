#arima and read any csv
library(forecast)

xt <- ts(as.numeric(x), frequency = 7)
plot(xt)

xt.stl <- stl(xt, s.window = "periodic")
plot(xt.stl)

xt.arima<-auto.arima(xt,trace=T,stepwise=T,seasonal=T)

plot(forecast(xt.arima,range = c(hoge,muga), h = mage))

#var

library(vars)
library(tseries)

VARselect(x,lag.max = hoge)
x.var = VAR(x,p=VARselect(x,lag.max=hoge)$selection[1])
summary(x.var)
x.pred <- predict(x.var, n.ahead = 20, ci = 0.95)
plot(x.pred)

#impluse and granger

x.irf <-irf(msci.var, n.ahead = 14, ci = 0.95)
plot(x.irf)


#Phillips-Perron Unit Root Test

pp.test(x)
#if p-value result over .05, this has unit root

#search cointegrating vector
x.vecm<-ca.jo(x,ecdet="none",type="eigen",K=2,spec="longrun",season=4)
summary(sjf.vecm)

x.vec2var <- vec2var(x.vecm, r= )
x.pred <- predict(x.vec2var, n.ahead = 25, ci = 0.95)
plot(x.pred)


#statement model

library(dlm)

build_local_trend <- function(theta){
  dlmModPoly(order=2,dV=exp(theta[1]),dW=c(0,0))+
  dlmModSeas(fr=12,dW=c(0,rep(0,10)),dV=0)}

fit.4 <- dlmMLE(
  data,
  parm=dlmMLE(data,parm=c(1),build_local_trend,method="Nelder-Mead")$par,
  build_local_trend,
  method="BFGS"
)

DLM.4 <- build_local_trend(fit.4$par)
Filt.4 <- dlmFilter(data, DLM.4)
Smooth.4 <- dlmSmooth(Filt.4)

plot(data, col=1, type="o", lwd=1)
lines(dropFirst(Filt.4$m)[, 1] + dropFirst(Filt.4$m)[, 3], col=2, lwd=2)
lines(dropFirst(Smooth.4$s)[, 1] + dropFirst(Smooth.4$s)[, 3], col=4, lwd=2)

legend("bottomright", pch=c(1,NA,NA),
        col=c(1,2,4), lwd=c(1,2,2), legend=c("data","Filter","Smooth"))

Fore <- dlmForecast(Filt.4, nAhead=24, sampleNew=5)
plot(data, type="o")
lines(dropFirst(Smooth.5$s)[,1]+dropFirst(Smooth.5$s)[,3],col=4)
lines(Fore$f,col=2,lwd=2)

legend(
  "bottomright",pch=c(1,NA),col=c(1,2),lwd=c(1,2),legend=c("実測値","予測値")
)
