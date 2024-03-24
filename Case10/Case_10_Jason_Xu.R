## Week 10 Case
## Jason Xu

## Part 1
ge <- read.csv("GE.csv",header = TRUE)
spy <- read.csv("SPY.csv",header = TRUE)
tsla <- read.csv("TSLA.csv",header = TRUE)

clean_yahoo = function(dat)
  {
  dat$Date <- as.Date(dat$Date, format ="%Y/%m/%d")
  namz <- c("Date", "Adj.Close")
  dat <- dat[,namz]
  names(dat) <- c("Date", "Price")
  Price_l <- c(NA, dat[1:(nrow(dat)-1),"Price"])
  dat$ret <- dat$Price/Price_l -1
  dat$Gross_Ret_ytd <- dat$Price/dat$Price[1]
  return(dat)
}

ge <- clean_yahoo(ge)
spy <- clean_yahoo(spy)
tsla <- clean_yahoo(tsla)

names(spy) <- c("Date", "spy_price", "spy_ret", "spy_grossret")
names(ge) <- c("Date", "ge_price", "ge_ret", "ge_grossret")
names(tsla) <- c("Date", "tsla_price", "tsla_ret", "tsla_grossret")

merged_dat <- merge(spy, tsla, by = "Date")
merged_dat <- merge(merged_dat, ge, by ="Date")

## Part 2
geMn <- mean(ge$ge_ret, trim = 0, na.rm = TRUE)
spyMn <- mean(spy$spy_ret, trim = 0, na.rm = TRUE)
tslaMn <- mean(tsla$tsla_ret, trim = 0, na.rm = TRUE)

sd(ge$ge_ret, na.rm = TRUE)
sd(spy$spy_ret, na.rm = TRUE)
sd(tsla$tsla_ret, na.rm = TRUE)

hist(ge$ge_ret)
hist(spy$spy_ret)
hist(tsla$tsla_ret)

## Patterns I noticed:
## They are skewed.

corTsla <- cor(spy$spy_ret, tsla$tsla_ret, method = c("pearson", "kendall", "spearman"),use = "complete.obs")
corGe <- cor(spy$spy_ret, ge$ge_ret, method = c("pearson", "kendall", "spearman"),use = "complete.obs")

linearOne <- lm(spy$spy_ret~tsla$tsla_ret, data = merged_dat)
linearTwo <- lm(spy$spy_ret~ge$ge_ret, data = merged_dat)

plot(x=spy$spy_ret,y=tsla$tsla_ret,xlab = "SPY Returns",ylab = "TESLA Returns", 
     main = "SPY and Tesla",abline(linearOne),abline(v=spyMn),abline(h=tslaMn))
plot(x=spy$spy_ret,y=ge$ge_ret,xlab = "SPY Returns",ylab = "GE Returns", 
     main = "SPY and GE",abline(linearTwo),abline(v=spyMn),abline(h=geMn))

## Do these vertical and horizontal lines cross on the regression line?
## They crossed on the regression line.
## Average values on the vertical must cross the regression line
## Average values on the horizontal may cross the regression line

lmSumTsla <- summary(linearOne)
lmSumGe <- summary(linearTwo)

## Multiple R-squared:  0.2035,	Adjusted R-squared:  0.1898 for Spy vs Tsla
## Multiple R-squared:  0.1653,	Adjusted R-squared:  0.1509 for Spy vs GE
## These values mean that returns for Tesla and GE both are low.

resids_tsla <- resid(linearOne)
resids_ge <- resid(linearTwo)

avgTsla <- mean(resids_tsla,trim = 0, na.rm = TRUE)
avgGe <- mean(resids_ge,trim = 0,na.rm = TRUE)

## What is the correlation of these variables with the SPY returns? 
## Correlation for GE vs SPY is 0.40658
## Correlation for TSLA vs SPY is 0.45110

## Part 3
ge36 <- read.csv("GE_36.csv",header = TRUE)
spy36 <- read.csv("SPY_36.csv",header = TRUE)
tsla36 <- read.csv("TSLA_36.csv",header = TRUE)

clean_yahoo = function(dat)
{
  dat$Date <- as.Date(dat$Date, format ="%Y/%m/%d")
  namz <- c("Date", "Adj.Close")
  dat <- dat[,namz]
  names(dat) <- c("Date", "Price")
  Price_l <- c(NA, dat[1:(nrow(dat)-1),"Price"])
  dat$ret <- dat$Price/Price_l -1
  dat$Gross_Ret_ytd <- dat$Price/dat$Price[1]
  return(dat)
}

ge36 <- clean_yahoo(ge36)
spy36 <- clean_yahoo(spy36)
tsla36 <- clean_yahoo(tsla36)

names(spy36) <- c("Date", "spy_price", "spy_ret", "spy_grossret")
names(ge36) <- c("Date", "ge_price", "ge_ret", "ge_grossret")
names(tsla36) <- c("Date", "tsla_price", "tsla_ret", "tsla_grossret")

merged_dat <- merge(spy, tsla, by = "Date")
merged_dat <- merge(merged_dat, ge, by ="Date")

geMn2 <- mean(ge36$ge_ret, trim = 0, na.rm = TRUE)
spyMn2 <- mean(spy36$spy_ret, trim = 0, na.rm = TRUE)
tslaMn2 <- mean(tsla36$tsla_ret, trim = 0, na.rm = TRUE)

corTsla <- cor(spy36$spy_ret, tsla36$tsla_ret, method = c("pearson", "kendall", "spearman"),use = "complete.obs")
corGe <- cor(spy36$spy_ret, ge36$ge_ret, method = c("pearson", "kendall", "spearman"),use = "complete.obs")

linearThr <- lm(spy36$spy_ret~tsla36$tsla_ret, data = merged_dat)
linearFur <- lm(spy36$spy_ret~ge36$ge_ret, data = merged_dat)

plot(x=spy36$spy_ret,y=tsla36$tsla_ret,xlab = "SPY Returns 2",ylab = "TESLA Returns 2", 
     main = "SPY and Tesla 2",abline(linearThr),abline(v=spyMn2),abline(h=tslaMn2))
plot(x=spy36$spy_ret,y=ge36$ge_ret,xlab = "SPY Returns 2",ylab = "GE Returns 2", 
     main = "SPY and GE 2",abline(linearFur),abline(v=spyMn),abline(h=geMn))

summary(linearThr)
summary(linearFur)

resids_tsla2 <- resid(linearThr)
resids_ge2 <- resid(linearFur)

mean(resids_tsla2,trim = 0, na.rm = TRUE)
mean(resids_ge2,trim = 0,na.rm = TRUE)

## Beta of GE is 1.05
## Beta of TSLA is 1.89
## Betas of these two companies are higher than I calculated.