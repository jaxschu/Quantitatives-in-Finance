download.file("http://www.openintro.org/stat/data/ames.RData", destfile = "ames.RData")
load("ames.RData")
ames_trim <- ames [,c("Saleprice","Lot.Area", "Gr.Liv.Area", "Yr.sold", "Year.Built", "age")]
ames$age<-ames$Year.Built-ames$Yr.sold
head(ames_trim)

summary (ames_trim)

sd(ames_trim$Saleprice, na.rm=TRUE)
sd(ames_trim$age, na.rm=TRUE)
sd(ames_trim$Lot.Area, na.rm=TRUE)
sd(ames_trim$Gr.Liv.Area, na.rm=TRUE)

hist (ames_trim$Saleprice)
hist (ames_trim$age)
hist (ames_trim$Lot.Area)
hist (ames_trim$Gr.Liv.Area)


plot (ames_trim$Lot.Area, ames_trim$Saleprice, xlab = "Lot Area", ylab="sales Price") 
+abline(lm(ames_trim$Saleprice~ames_trim$Lot.Area))

plot (ames_trim$Gr.Liv.Area, ames_trim$Saleprice, xlab = "Greater Living Area", ylab="sales Price") 
+abline(lm(ames_trim$Saleprice~ames_trim$Gr.Liv.Area))

plot (ames_trim$age, ames_trim$Saleprice, xlab = "age", ylab="sales Price") 
+abline(lm(ames_trim$Saleprice~ames_trim$age))

summary(lm(ames_trim$Saleprice-ames_trim$Lot.Area)) 
summary(lm(ames_trim$Saleprice-ames_trim$Gr.Liv.Area)) 
summary(lm(ames_trim$Saleprice-ames_trim$age))

summary (lm(saleprice~Lot.Area, data=ames_trim[ames_trim$Lot.Area<50000,]))
summary (lm(saleprice~Gr.Liv.Area, data=ames_trim[ames_trim$Gr.Liv.Area<40000,]))
summary (lm(saleprice~age, data=ames_trim[ames_trim$age>120,]))

summary (lm(log(saleprice)~Lot.Area, data=ames_trim[ames_trim$Lot.Area<50000,]))
summary (lm(log(saleprice)~Gr.Liv.Area, data=ames_trim[ames_trim$Gr.Liv.Area<40000,]))
summary (lm(log(saleprice)~age, data=ames_trim[ames_trim$age>120,]))