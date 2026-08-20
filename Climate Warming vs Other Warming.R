#LAB 03: 

install.packages('BSDA')
install.packages('e1071')
library(BSDA)
library(e1071)

str(yumaRaw)
head(yumaRaw)

str(phoenixRaw)
head(phoenixRaw)

yumaRaw <- read.csv('yuma_temp.csv', header=FALSE)
phoenixRaw <- read.csv('phoenix_temp.csv', header=FALSE)

names(yumaRaw) <- month.abb
names(phoenixRaw) <- month.abb

years <- 1949:2020
yuma <- cbind(years, yumaRaw)
phoenix <- cbind(years, phoenixRaw)

hist(as.matrix(yuma[,2:13]))
yuma[yuma <= -100] <- NA
sum(is.na(yuma))

#Question 1: 
phoenixTempJul <- phoenix$Jul
plot(years, phoenixTempJul, type='l', col='red', xlab = 'Years', ylab = 'Temperatures (ºC)',
     main = 'July Temperatures in Phoenix from 1949-2020')

#Goal: figure out what July data points happen between 1990-Present

which(phoenix$years > 1989) #all the positions that are these years
phoenixLast30 <- which(phoenix$year > 1989) #making these position a variable
phoenixLast30
phoenixTempJul[phoenixLast30]
phoenixSample <- phoenixTempJul[phoenixLast30]
  
#standard error of the normal distribution
((sd(phoenixTempJul)))/(sqrt(length(phoenixSample)))
standardError <- ((sd(phoenixTempJul))/(sqrt(length(phoenixSample)))
print(standardError)
                  
#Z-Score
((mean(phoenixSample))-(mean(phoenixTempJul)))/standardError
sampleMean <- mean(phoenixSample)
popMean <- mean(phoenixTempJul)

(sampleMean-popMean)/standardError
zScorePhx <- (sampleMean-popMean)/standardError

popSD <- sd(phoenixTempJul)

z.test(phoenixSample, sigma.x=popSD ,mu=popMean, alternative = 'greater')

pnorm(zScorePhx)
1-pnorm(zScorePhx)

pScorePhoenix <- pnorm(-abs(zScorePhx))

print(pScorePhoenix)

#Question 2: 

mean(<data>, na.rm=TRUE)

yumaTempJul <- yuma$Jul
plot(years, yumaTempJul, type='l', col='red', xlab = 'Years', ylab = 'Temperatures (ºC)',
     main = 'July Temperatures in Yuma from 1949-2020')

which(yuma$years > 1989) #all the positions that are these years
yumaLast30 <- which(yuma$years > 1989)
yumaLast30
yumaTempJul[yumaLast30]
yumaSample <- yumaTempJul[yumaLast30]

((sd(yumaTempJul, na.rm=TRUE)))/(sqrt(length((yumaSample))))
standardErrorYuma <- ((sd(yumaTempJul, na.rm=TRUE)))/(sqrt(length((yumaSample))))
print(standardErrorYuma)

((mean(yumaSample, na.rm=TRUE)))
sampleMeanYuma <- ((mean(yumaSample, na.rm=TRUE)))

mean(yumaTempJul, na.rm=TRUE)
popMeanYuma <- mean(yumaTempJul, na.rm=TRUE)

(sampleMeanYuma-popMeanYuma)/standardErrorYuma
zScoreYuma <- (sampleMeanYuma-popMeanYuma)/standardErrorYuma

popSDyuma <- sd(yumaTempJul, na.rm=TRUE)

z.test(yumaSample, sigma.x=popSDyuma, mu=popMeanYuma, alternative = 'greater')

pnorm(zScoreYuma)
1-pnorm(zScoreYuma)
PScoreYuma <- pnorm(-abs(zScoreYuma))