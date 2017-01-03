#Get external data
setwd('/Users/Sophie/Documents/Caltex')
###FOR ANANLYSIS###
#Assume Sydney for rainfall, as no location given
rainfall <- read.csv('rainfall.csv')
head(rainfall)
summary(rainfall)
rainfall$Calendar.day <- as.Date(paste(rainfall$Year, rainfall$Month, rainfall$Day, sep = '-'))
rainfall <- subset(rainfall, 
                   subset = rainfall$Calendar.day >= as.Date('2013-01-01')
                   & rainfall$Calendar.day <= as.Date('2015-06-18'),
                   select = c(Rainfall.amount..millimetres., Calendar.day))
colnames(rainfall)[colnames(rainfall) == 'Rainfall.amount..millimetres.'] <- 'am.rainfall'
rainfall$rain.days <- as.factor(as.numeric((rainfall$am.rainfall > 0)))
head(rainfall[is.na(rainfall$am.rainfall),])
#Four without values, may decide to set to 0 but will wait and see
#Plan to create a 'heavy rainfall variable', but will see what the data suggests
hist(rainfall$am.rainfall[rainfall$rain.days == 1])
summary(rainfall)

#CPI
library(gdata)
cpi <- read.xls('cpi.xls', sheet = 'Data1')
#Keep only national CPI
colnames(cpi)
cpi <- data.frame(cpi['X'],  cpi['Index.Numbers....All.groups.CPI....Australia..'])
colnames(cpi) <- c('Calendar.day', 'CPI')

#Drop information rows - first 9 rows
head(cpi,10)
cpi <- cpi[-c(1:9),]

#Format
cpi$Calendar.day <- as.Date(paste('01', as.character(cpi$Calendar.day), sep='-'), '%d-%b-%Y')
cpi$CPI <- as.numeric(as.character(cpi$CPI))
summary(cpi)
head(cpi)

#Subset
cpi <- cpi[cpi$Calendar.day >= as.Date('2012-12-01')
           & cpi$Calendar.day <= as.Date('2015-09-01'),]
cpi

#Interpolate (linear) CPI values for all dates
days <- as.Date(c(as.Date('2013-01-01'):as.Date('2015-09-01')), origin = '1970-01-01')
tmp <- as.data.frame(approx(x = cpi$Calendar.day, y = cpi$CPI,
                     xout = days, method = 'linear'))
colnames(tmp) <- c('Calendar.day', 'CPI.lin')
cpi <- merge(cpi, tmp, by = 'Calendar.day', all = TRUE)

#Interpolate (flat) CPI values for all dates
tmp <- as.data.frame(approx(x = cpi$Calendar.day, y = cpi$CPI,
                            xout = days, method = 'constant'))
colnames(tmp) <- c('Calendar.day', 'CPI.step')
cpi <- merge(cpi, tmp, by = 'Calendar.day', all = TRUE)
cpi <- subset(cpi, 
              subset = cpi$Calendar.day >= as.Date('2013-01-01')
              & cpi$Calendar.day <= as.Date('2015-06-18'))

#Check
plot(cpi$Calendar.day, cpi$CPI.step, type = 'l')
lines(cpi$Calendar.day, cpi$CPI.lin, lty = 2)
points(cpi$Calendar.day, cpi$CPI, col = 'red')


#Average Weekly Earnings (AWE)
awe <- read.xls('awe.xls', sheet = 'Data1')
#Keep only national CPI
colnames(awe)
awe <- data.frame(awe['X'],  awe['Earnings..Persons..Total.earnings..'])
colnames(awe) <- c('Calendar.day', 'AWE')

#Drop information rows - first 9 rows
head(awe,10)
awe <- awe[-c(1:9),]

#Format
awe$Calendar.day <- as.Date(paste('01', as.character(awe$Calendar.day), sep='-'), '%d-%b-%Y')
awe$AWE <- as.numeric(as.character(awe$AWE))
summary(awe)
head(awe)

#Subset
awe <- awe[awe$Calendar.day >= as.Date('2012-11-01')
           & awe$Calendar.day <= as.Date('2016-01-01'),]
awe
#Data only to May 2015

#Interpolate (linear) CPI values for all dates
days <- as.Date(c(as.Date('2012-11-01'):as.Date('2015-07-01')), origin = '1970-01-01')
tmp <- as.data.frame(approx(x = awe$Calendar.day, y = awe$AWE,
                            xout = days, method = 'linear', rule = 2))
colnames(tmp) <- c('Calendar.day', 'AWE.lin')
awe <- merge(awe, tmp, by = 'Calendar.day', all = TRUE)

#Interpolate (flat) CPI values for all dates
tmp <- as.data.frame(approx(x = awe$Calendar.day, y = awe$AWE,
                            xout = days, method = 'constant', rule = 2))
colnames(tmp) <- c('Calendar.day', 'AWE.step')
awe <- merge(awe, tmp, by = 'Calendar.day', all = TRUE)
awe <- subset(awe, 
              subset = awe$Calendar.day >= as.Date('2013-01-01')
              & awe$Calendar.day <= as.Date('2015-06-18'))

#Check
plot(awe$Calendar.day, awe$AWE.step, type = 'l')
lines(awe$Calendar.day, awe$AWE.lin, lty = 2)
points(awe$Calendar.day, awe$AWE, col = 'red')

#Merge all external data together

tmp <- merge(rainfall, cpi, by = 'Calendar.day', all = TRUE)
external.data <- merge(tmp, awe, by = 'Calendar.day', all = TRUE)
head(external.data)
summary(external.data)

###FOR VISUALISATION###
#Rainfall
#Temperature
#Solar Exposure