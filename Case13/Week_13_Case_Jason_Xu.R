## Case 13
## Jason Xu

setwd("/Users/xupeili/Desktop/msbc5031QtMethods/Wk13Case/fwdweek13case")
source("binscatter_function.R")
library(lfe)
library(ggplot2)

#Part 1
SPY<-read.csv("SPY.csv", header = TRUE)
SLB<-read.csv("SLB.csv", header = TRUE)
BICSX<-read.csv("BICSX.csv", header = TRUE)
OIL<-read.csv("OIL.csv", header = TRUE)

clean_yahoo = function(dat){ 
  dat$Date <- as.Date(dat$Date, format ="%m/%d/%Y")
  namz <- c("Date", "Adj.Close")
  dat <- dat[,namz]
  names(dat) <- c("Date", "Price")
  Price_l <- c(NA, dat[1:(nrow(dat)-1),"Price"])
  dat$ret <- dat$Price/Price_l -1
  dat$Gross_Ret_ytd <- dat$Price/dat$Price[1]
  return(dat)
}

SPY <- clean_yahoo(SPY)
SLB <- clean_yahoo(SLB)
BICSX <- clean_yahoo(BICSX)

names(SPY) <- c("Date", "SPY_price", "SPY_ret", "SPY_grossret")
names(SLB) <- c("Date", "SLB_price", "SLB_ret", "SLB_grossret")
names(BICSX) <- c("Date", "BICSX_price", "BICSX_ret", "BICSX_grossret")

merged_dat <- merge(SPY, SLB, by = "Date", na.rm = FALSE, all = TRUE)
merged_dat <- merge(merged_dat, BICSX, by ="Date", na.rm = FALSE, all = TRUE)
merged_dat <- merge(merged_dat, OIL, by = "Date", na.rm = FALSE, all = TRUE)

#Part 2
summary(merged_dat)

summary(SPY$SPY_ret)
summary(SLB$SLB_ret)
summary(BICSX$BICSX_ret)

sd(SPY$SPY_ret, na.rm=TRUE)
sd(SLB$SLB_ret, na.rm=TRUE)
sd(BICSX$BICSX_ret, na.rm=TRUE)

mean(SPY$SPY_ret, na.rm = TRUE)
mean(SLB$SLB_ret, na.rm = TRUE)
mean(BICSX$BICSX_ret, na.rm = TRUE)

hist(SPY$SPY_ret)
hist(SLB$SLB_ret)
hist(BICSX$BICSX_ret)

## Which patterns do you note?
## All datasets are right skewed.

lmSs <- lm(SPY$SPY_ret~SLB$SLB_ret,data = merged_dat)
lmSb <- lm(SPY$SPY_ret~BICSX$BICSX_ret,data = merged_dat)

lmOs <- lm(OIL$Percent.Change[1:length(SLB$SLB_ret)]~SLB$SLB_ret)
lmOb <- lm(OIL$Percent.Change[1:length(BICSX$BICSX_ret)]~BICSX$BICSX_ret)

plot(x=SLB$SLB_ret,y=SPY$SPY_ret,xlab = "SLB Returns",ylab = "SPY Returns", abline(lmSs))
plot(x=BICSX$BICSX_ret,y=SPY$SPY_ret,xlab = "BICSX Returns",ylab = "SPY Returns", abline(lmSb))

plot(x=SLB$SLB_ret,y=OIL$Percent.Change[1:length(SLB$SLB_ret)],
     xlab = "SLB Returns",ylab = "OIL Percent Change", abline(lmOs))
plot(x=BICSX$BICSX_ret,y=OIL$Percent.Change[1:length(BICSX$BICSX_ret)],
     xlab = "BICSX Returns",ylab = "OIL Percent Change", abline(lmOb))

## explain how and why the multiple regression estimate differs 
## from the single regression estimate.
## Because we now have 2 variables instead of just one.

## Estimate two multiple linear regressions
lmMultipleRegre <- lm(OIL$Percent.Change[1:length(SLB$SLB_ret)]~(SLB$SLB_ret+BICSX$BICSX_ret))

plot(x=SLB$SLB_ret,y=OIL$Percent.Change[1:length(SLB$SLB_ret)],abline(lmMultipleRegre),
     xlab = "Returns",ylab = "Oil Percent Change",main = "Multiple Regression")

## provide a contextual interpretation of the estimated slope 
## on oil price changes in the vein of "holding constant."
## The multiple regression slope curves up compared to the two single regressions,
## So the trend holds constant.

## Non-usable codes
#binscatter("Percent.Change~SLB_ret", key_var = "Percent.Change", data = merged_dat)
#binscatter("Percent.Change~BICSX_ret", key_var = "Percent.Change", data = merged_dat)

## Provide an interpretation of the intercept 
## from the single regression versus the multiple regression
## For BICSX, the single regression predicts its returns will decrease.
## While on multiple regression, it shows its returns will accordingly increase.

## Part 2 E
lm(OIL$Percent.Change[1:length(SPY$SPY_ret)]~SPY$SPY_ret)
## after accounting for oil price change, we found beta for spy is -30.454.

