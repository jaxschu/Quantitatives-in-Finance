## Week 11
## Jason Xu

setwd("C:/Users/jason/OneDrive/MSBC5031QuantMethods/Case11")

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

prnhx <- read.csv("PRNHX.csv",header = TRUE)
vmgix <- read.csv("VMGIX.csv",header = TRUE)
xmmo <- read.csv("XMMO.csv",header = TRUE)

prnhx <- clean_yahoo(prnhx)
vmgix <- clean_yahoo(vmgix)
xmmo <- clean_yahoo(xmmo)

hist(prnhx$ret)
hist(vmgix$ret)
hist(xmmo$ret)

summary(prnhx$ret)
summary(vmgix$ret)
summary(xmmo$ret)

dat <- merge(prnhx, vmgix, all = TRUE, na.rm=TRUE)
dat <- merge(xmmo, dat, all = TRUE, na.rm=TRUE)

char_lm <- lm(vmgix$ret~prnhx$ret, data = dat)

## Comment: The lengths of XMMO and VMGIX are different, so lm doesn't apply.

vmgixMn <- mean(vmgix$ret,trim = 0, na.rm = TRUE)

corre <- cor(vmgix$ret, prnhx$ret, method = c("pearson", "kendall", "spearman"),use = "complete.obs")

##this correlation value tells that the correlation between PRNHX and VMGIX is strong.

plot(y=prnhx$ret,x=vmgix$ret,ylab = "PRNHX Returns",xlab = "VMGIX Returns")
abline(v=vmgixMn)
abline(char_lm)

summary(char_lm)
rsqrtVal <- summary(char_lm)$r.squared

## This R-Square value with the correlation tells that they are similar.

## The intercept is right in the middle of the data cluster.

## standard error for the intercept is 0.0002002,
## and the standard error for the slope is 0.0129441.
## This tells that the standard error for these data is relatively very small.

tTestP <- t.test(prnhx$ret)
tTestV <- t.test(vmgix$ret)
tTestX <- t.test(xmmo$ret)

## t-test in this context concludes that p-value for PRNHX is 0.0438
## p-value for VMGIX is 0.115
## Compared to the other p-values, p-value for XMMO is larger, making it less stable.

##Part 3
mylm2 <- lm(vmgix$ret~scale(prnhx$ret), data=dat)
summary(mylm2)

plot(y=prnhx$ret,x=vmgix$ret,ylab = "PRNHX Returns 2",xlab = "VMGIX Returns 2")
abline(v=mean(vmgix$ret,trim = 0, na.rm = TRUE))
abline(mylm2)

mylm3 <- lm(I(100*vmgix$ret)~I(100*prnhx$ret), data=dat)
summary(mylm3)

plot(y=prnhx$ret,x=vmgix$ret,ylab = "PRNHX Returns 3",xlab = "VMGIX Returns 3")
abline(v=mean(vmgix$ret,trim = 0, na.rm = TRUE))
abline(mylm3)

t.test(scale(prnhx$ret))
t.test(I(100*prnhx$ret))

summary(mylm2)$r.squared
summary(mylm3)$r.squared

## Changes: p-values change.
## No change: r-square values don't change.
## Other stats change.

## Part II H
## t.test(scale(prnhx$ret)) p-value = 1.
## This suggests no difference between the groups other than due to chance.



