
library(tseries)
sp500.prices=get.hist.quote(
  instrument = "^GSPC",
  quote = "Adj",
  provider = c("yahoo"), method = NULL,
  origin = "2006-12-30", compression = "d",
  retclass = c("zoo"), quiet = FALSE, drop = FALSE
)
sp500=as.data.frame(sp500.prices)
N=length(sp500[,1])