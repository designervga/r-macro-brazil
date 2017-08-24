library(dplyr)
library(tidyr)
library(htmltab)
library(lubridate)
library(zoo)
library(dygraphs)

# IBOVESPA MONTHLY VAR %     ================================


url1 <- "http://www.ipeadata.gov.br/ExibeSerie.aspx?serid=31875&module=M"
ibovespa          <- htmltab(doc = url1, which = "//table[@id='grd_DXMainTable']", rm_nodata_cols = TRUE)
names(ibovespa)       <- c("month", "ibov")
ibovespa$ibov     <- gsub(",", "\\.", ibovespa$ibov)
ibovespa$ibov     <- as.numeric(ibovespa$ibov)
ibovespa$ibov     <- round(ibovespa$ibov, 3)
ibovespa$month    <- ymd(ibovespa$month, truncated = 2)
ibovespa          <- subset(ibovespa, ibovespa$month > as.Date("2000-12-01"))
ibovespa.ts       <- ts(ibovespa[, 2], start = c(2001, 1), end = c(2017, 6), freq = 12)

plot(ibovespa.ts, main = "IBOV Monthly Var %")
abline(h = seq(0, 2, .4), v = seq(2001, 20017, 1), lty = 3, col = "gray")
dygraph(ibovespa.ts, "Retorno Ibovespa")


# IBOVESPA MONTH-END VALUE   ================================

ibovespa2          <- read.table("ibov.csv", sep = ",", header = TRUE)
names(ibovespa2)       <- c("month", "ibov")
ibovespa2$ibov     <- gsub(",", "\\.", ibovespa2$ibov)
ibovespa2$ibov     <- as.numeric(ibovespa2$ibov)
ibovespa2$ibov     <- round(ibovespa2$ibov, 3)
ibovespa2$month    <- ymd(ibovespa2$month)
ibovespa2          <- subset(ibovespa2, ibovespa2$month > as.Date("2000-12-01"))
ibovespa2.ts       <- ts(ibovespa2[, 2], start = c(2001, 1), end = c(2017, 6), freq = 12)

plot(ibovespa2.ts, main = "ibovespa2")
abline(h = seq(0, 2, .4), v = seq(2001, 20017, 1), lty = 3, col = "gray")
dygraph(ibovespa2.ts, "Retorno ibovespa2")

ret_ibovespa2.ts <- NULL
for (i in 1:length(ibovespa2.ts)) {
  ret_ibovespa2.ts[i] <- log(ibovespa2.ts[i+1]/ibovespa2.ts[i])
}
ret_ibovespa2.ts <- ts(ret_ibovespa2.ts, start = c(2001,1), end = c(2017, 6), frequency = 12)
dygraph(ret_ibovespa2.ts, "Retorno IBOVESPA2")



# CDI OVER MONTH ================================
url2          <- "http://www.ipeadata.gov.br/ExibeSerie.aspx?serid=32237&module=M"
cdiover <- htmltab(doc = url2, which = "//table[@id='grd_DXMainTable']", rm_nodata_cols = TRUE)
names(cdiover)    <- c("month", "cdi")
cdiover$cdi       <- gsub(",", "\\.", cdiover$cdi)
cdiover$cdi       <- as.numeric(cdiover$cdi)
cdiover$cdi       <- round(cdiover$cdi, 3)
cdiover$month     <- ymd(cdiover$month, truncated = 2)
cdiover           <- subset(cdiover, cdiover$month > as.Date("2000-12-01"))
cdiover.ts        <- ts(cdiover[, 2], start = c(2001, 1), end = c(2017, 6), freq = 12)

plot(cdiover.ts, main = "CDI Over - Monthly Brazil")
abline(h = seq(0, 2, .4), v = seq(2001, 20017, 1), lty = 3, col = "gray")

ret_cdiover.ts <-NULL
for (i in 1:length(cdiover.ts)) {
  ret_cdiover.ts[i] <- log(cdiover.ts[i+1]/cdiover.ts[i])
}
ret_cdiover.ts <- ts(ret_cdiover.ts, start = c(2001,1), end = c(2017, 6), frequency = 12)
dygraph(ret_cdiover.ts, "Retorno CDI")


## -------- Leverage Effect ----------- 

layout(1:2)
ts.plot(cdiover.ts)
ts.plot(ret_cdiover.ts)

layout(1:2)
ts.plot(ibovespa.ts)
ts.plot(ret_ibovespa2.ts)
