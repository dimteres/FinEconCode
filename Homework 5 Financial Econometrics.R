install.packages("fGarch")
install.packages("rugarch")
install.packages("rmgarch")
library(fGarch)
library(rmgarch)
library(rugarch)
library(tseries)
library(zoo)

# Retrieving return data
aapl <- get.hist.quote(instrument = "AAPL",  start = "2016-01-01",
                      quote = "AdjClose")
goog <- get.hist.quote(instrument = "GOOG",  start = "2016-01-01",
                       quote = "AdjClose")
amzn <- get.hist.quote(instrument = "AMZN",  start = "2016-01-01",
                       quote = "AdjClose")
#Returns
aapl.ret<-diff(log(aapl))
goog.ret<-diff(log(goog))
amzn.ret<-diff(log(amzn))

# univariate normal GARCH(1,1) for each series
garch.spec = ugarchspec(mean.model = list(armaOrder = c(0,0)), 
                          variance.model = list(garchOrder = c(1,1), 
                                                model = "sGARCH"), 
                          distribution.model = "norm")

# dcc specification - GARCH(1,1) for conditional correlations
dcc.garch.spec = dccspec(uspec = multispec( replicate(3, garch.spec) ), 
                           dccOrder = c(1,1), 
                           distribution = "mvnorm")

ret <- merge(aapl.ret, goog.ret, amzn.ret , all = FALSE)
plot(ret)

dcc.fit <- dccfit(dcc.garch.spec, data = na.omit(ret))
dcc.fit

# Estimated variance
plot(dcc.fit, which = 2)

# compared to GARCH, this seems very similar. The model overall seems to be fitting decently, however we observe that the normality
# of the variance makes for clearly flawed spikes and lows.


# Conditional correlation series
plot(dcc.fit, which = 4)

