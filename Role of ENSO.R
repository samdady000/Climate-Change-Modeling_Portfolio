###########
##PART 1###
###########

co2 <- read.table('01_MaunaLoaCO2.txt')
jeniTemp <- read.table('02_JenisejskSiberia2mtNDJFM.txt')

#examine
str(co2)
str(jeniTemp)

#give the columns good names:
names(co2) <- c('year','co2')
names(jeniTemp) <- c('year','temp')

#create variables
WinterTemp <- jeniTemp$temp
MaunaLoaCO2 <- co2$co2

#plot 
plot (MaunaLoaCO2, WinterTemp, col='blue', ylab='Temperature (Celsius)', xlab='CO2 Concentration (ppm)', 
      main='Plotting the Potential Correlation Between 
Temperature and CO2 Concentration from 1960-2018')

#Pearson
cor.test(WinterTemp, MaunaLoaCO2, method = 'pearson')

(0.3189483)^2

#Spearman
cor.test(WinterTemp, MaunaLoaCO2, method = 'spearman')

#Kendall
cor.test(WinterTemp, MaunaLoaCO2, method = 'kendall')

###########
##PART 2###
###########

tampaPr <- read.table('03_Tampa_prcp.txt')
sst <- read.table('04_nino3p4_SST_1950_2020.txt')

#examine our new temperature data to make sure they line up
str(sst)
str(tampaPr)

names(tampaPr) <- c('year',month.abb)
names(sst) <- c('year',month.abb)

#plot: January
tampaPrJan <- tampaPr$Jan
NinoSSTJan <- sst$Jan

plot(NinoSSTJan, tampaPrJan, col='red', ylab='Precipitation in Tampa', xlab='Sea Suface Temperature', 
     main='Plotting the Relationship Between Precipitation in Tampa 
and Sea Surface Temperature in January from 1950-2020')

#plot: February
tampaPrFeb <- tampaPr$Feb
NinoSSTFeb <- sst$Feb

plot(NinoSSTFeb, tampaPrFeb, col='red', ylab='Precipitation in Tampa', xlab='Sea Suface Temperature', 
     main='Plotting the Relationship Between Precipitation in Tampa 
and Sea Surface Temperature in February from 1950-2020')

#plot: March
tampaPrMar <- tampaPr$Mar
NinoSSTMar <- sst$Mar

plot(NinoSSTMar, tampaPrMar, col='red', ylab='Precipitation in Tampa', xlab='Sea Suface Temperature', 
     main='Plotting the Relationship Between Precipitation in Tampa 
and Sea Surface Temperature in March from 1950-2020')

#Pearson: January
cor.test(tampaPrJan, NinoSSTJan, method = 'pearson')
cor.test(tampaPrJan, NinoSSTJan, method = 'spearman')
cor.test(tampaPrJan, NinoSSTJan, method = 'kendall')

#Pearson: February
cor.test(tampaPrFeb, NinoSSTFeb, method='pearson')
cor.test(tampaPrFeb, NinoSSTFeb, method='spearman')
cor.test(tampaPrFeb, NinoSSTFeb, method='kendall')

#Pearson: March
cor.test(tampaPrMar, NinoSSTMar, metod='pearson')
cor.test(tampaPrMar, NinoSSTMar, metod='spearman')
cor.test(tampaPrMar, NinoSSTMar, metod='kendall')


#2b: 
cor.test(rowMeans(tampaPr[2:4]), rowMeans(sst[2:4]))

###########
##PART 3###
###########

#this question needs the ncdf package, make sure it is installed!!
install.packages('ncdf4')
library(ncdf4)

#load the NINO3.4 SST
sstDJFRaw <- read.table('05_nino3p4_1950_2020_DJF.txt')

#check the data, notice it is a bunch of columns instead of a bunch of rows
str(sstDJFRaw)

#turn it into a numeric matrix (changes from columns to rows):

sstDJF <- as.numeric(sstDJFRaw)
str(sstDJF)

#load the precip data netCDF
ncin <- nc_open('06_CRU_prcp_DJF.nc')
print(ncin)

#we want the variable pre
lon <- ncvar_get(ncin, 'X')
lat <- ncvar_get(ncin, 'Y')
precip <- ncvar_get(ncin,'pre')

#check the size of the dimensions
dim(precip)

nc_close(ncin)
#looks good: lon, lat, time, with 71 years that match the SST data.

## 1

#get the lon and lat dimenion sizes:
lonSize <- dim(precip)[1]
latSize <- dim(precip)[2]

#first create the empty matrix to hold the correlations that will be calculated in for Loop:
corPrSST <- matrix(NA,lonSize,latSize)

#now do a 2 dimensional loop over lon and lat

for(i in 1:lonSize){
  
  for(j in 1:latSize){
    
    #get the precipitation data at that specific grid cell (lat/lon pair)
    prGridCell <- precip[i,j,]
    #calculate the correlation between precip and SST and assign it to the same lon and lat gridcell in the correlation matrix
    corPrSST[i,j] <- cor(prGridCell,sstDJF)
  }
}

filled.contour(lon,lat,corPrSST,color.palette=rainbow,xlab='longitude',ylab='latitude',main='Correlation between precipitation and NINO3.4 SST',cex.main=1)
