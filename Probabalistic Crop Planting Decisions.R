########
#LAB 08#
########

#upload data
cruPrecip <- read.csv('cruPpt.csv', header = FALSE)
nino34 <- read.csv("Nino34.csv", header = FALSE)

#rename columns 
colnames(cruPrecip) <- c("years", "precipitation")
colnames(nino34) <- c("years", "sst")

#plotting to see what they look like
plot(cruPrecip$years, cruPrecip$precipitation)
plot(nino34$years, nino34$sst)

#calculate mean and standard deviation
mean_rainfall <- mean(cruPrecip$precipitation)
sd_rainfall <- sd(cruPrecip$precipitation)

#calculate Z-scores for 424mm and 514mm
lowBoundary <- (424 - mean_rainfall) / sd_rainfall
highBoundary <- (514 - mean_rainfall) / sd_rainfall

#calculate probabilities
lowProb <- pnorm(lowBoundary)
highProb <- 1- pnorm(highBoundary)
midProb <- 1-(lowProb+highProb)

#standardize data
stdPrecip <- (cruPrecip$precipitation - mean_rainfall)/sd_rainfall
stdSST <- (nino34$sst - mean(nino34$sst))/sd(nino34$sst)

#run my regression
regFit <- lm(stdPrecip~stdSST)
summary(regFit)
slope <- regFit$coefficients[2]

#visualize regression
plot(stdSST, stdPrecip, cex=0.6,
     col="blue",
     xlab="Niño3.4 (Standard Deviations)",
     ylab="Rainfall (Standard Deviations)",
     main="Regression Plot for Niño3.4 and Rainfall")
abline(regFit,col="red")


#calculate standard error of prediction
standardError <-sqrt(1- cor(stdPrecip, stdSST)^2)

#using nino 1997 as example
yearInd <- which(nino34$years == 1997)
sst1997 <- stdSST[yearInd]
predPrecip <- slope * sst1997 #0.722#

low1997 <- pnorm(lowBoundary, predPrecip, standardError)
high1997 <- 1- pnorm(highBoundary, predPrecip, standardError)
mid1997 <- 1-(low1997+high1997)

emvBelowNormal <- 97998345*(lowProb)+256763643*(midProb)+288271289*(highProb)
emvNormal <- 95631375*(lowProb)+259525918*(midProb)+307549071*(highProb)
emvAboveNormal <- 15785226*(lowProb)+185678001*(midProb)+337742431*(highProb)

emv1997BelowNormal <- 97998345*(low1997)+256763643*(mid1997)+288271289*(high1997)
emv1997Normal <- 95631375*(low1997)+259525918*(mid1997)+307549071*(high1997)
emv1997AboveNormal <- 15785226*(low1997)+185678001*(mid1997)+337742431*(high1997)