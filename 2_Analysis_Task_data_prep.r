#Modelling task data preparation

#Import and format data
setwd('/Users/Sophie/Documents/Caltex')
profit <- read.csv('profit.csv')
main <- read.csv('competitor.csv')

#Format profitability data
#Note: It looks like profit is shown by site rather than individual location -
#  assuming site is a group of locations?
#summary(profit)
#Format dates
profit$Calendar.year...week <- as.character(profit$Calendar.year...week)
temp <- strsplit(profit$Calendar.year...week,'\\.')
profit$year <- as.factor(sapply(temp, '[[', 2))
profit$week <- as.factor(sprintf('%02d',as.numeric(sapply(temp, '[[', 1))))
profit$yyyyww <- as.factor(paste(profit$year,profit$week,sep=''))
profit$Posting.date <- as.Date(profit$Posting.date, '%d.%m.%Y')
rm(temp)
profit$Calendar.year...week <- NULL
#Transform profits from character to numeric, input format is #,###
profit$VORTEX <- as.numeric(gsub(',','',profit$VORTEX))
profit$VORTEX.98 <- as.numeric(gsub(',','',profit$VORTEX.98))
profit$UNLEADED.PETROL <- as.numeric(gsub(',','',profit$UNLEADED.PETROL))
profit$ULSD <- as.numeric(gsub(',','',profit$ULSD))
profit$BLENDED.DIESELS <- as.numeric(gsub(',','',profit$BLENDED.DIESELS))
summary(profit)

summary(profit$year[profit$Location == 'Site 7']) #2012-2015

#Format main analysis dataset
summary(main)
#Split out a store profit by looking at total revenue for each fuel type at site 7 and prices,
#then assume a fixed base price, then derive store of interest's share of profit
main$Location.ID <- as.factor(main$Location.ID)
main$Value <- as.numeric(gsub(',','',main$Value))
main$X7Eleven.Price <- as.numeric(gsub(',','',main$X7Eleven.Price))
main$Coles.Price <- as.numeric(gsub(',','',main$Coles.Price))
main$BP.Price <- as.numeric(gsub(',','',main$BP.Price))
main$Week.end.date <- as.Date(main$Week.end.date,'%d.%m.%Y')
main$Calendar.day <- as.Date(main$Calendar.day,'%d.%m.%Y')
summary(main)

xtabs( ~ Location + Location.ID, main)
#One Location.ID for each Location, use Location to merge on profitabilit data

#Create modelling dataset, without external variables
#Base data with pricing for all competitors
Loc22529 <- subset(main, subset = (main$Location.ID == '22529'
                   & main$Item == 'Fuel Sales\nincl GST (cpl)'
                   & (main$Fuel.Grade == 'E10' | main$Fuel.Grade == 'Vtx 95')
                   & !is.na(main$Calendar.day)
                   & !is.na(Value)),
                   select = c(-Item,-Unit))
summary(Loc22529)
colnames(Loc22529)[colnames(Loc22529)=='Value'] <- 'price.Caltex'
colnames(Loc22529)[colnames(Loc22529)=='BP.Price'] <- 'price.BP'
colnames(Loc22529)[colnames(Loc22529)=='X7Eleven.Price'] <- 'price.711'
colnames(Loc22529)[colnames(Loc22529)=='Coles.Price'] <- 'price.Coles'
Loc22529$price.mktavg <- rowMeans(Loc22529[c('price.BP', 'price.711', 'price.Coles', 'price.Caltex')],
                                           na.rm = TRUE)
Loc22529$price.mktavg.exCaltex <- rowMeans(Loc22529[c('price.BP', 'price.711', 'price.Coles')],
                              na.rm = TRUE)
summary(Loc22529)

#Fuel quantity sold to merge onto modelling data
quantity <- subset(main, subset = (main$Location.ID == '22529'
                                   & main$Item == 'Fuel Sales\nQuantity'
                                   & (main$Fuel.Grade == 'E10' | main$Fuel.Grade == 'Vtx 95')
                                   & !is.na(main$Calendar.day)),
                   select = c(Location.ID, Location, Fuel.Grade, Week.end.date, Calendar.day, Value))
colnames(quantity)[colnames(quantity)=='Value'] <- 'quantity'
mod1 <- merge(Loc22529, quantity, by = c('Location.ID','Location','Fuel.Grade','Week.end.date',
                                         'Calendar.day'))
#Check merge
dim(quantity)
dim(Loc22529)
dim(mod1)
summary(quantity[quantity$Calendar.day != as.Date('2012-12-31'),])
#This date has no price data and was dropped
summary(Loc22529)
summary(mod1)

#Shop sales to merge onto modelling data
xtabs(formula = Value ~ Fuel.Grade , data = main,
      subset = main$Item == 'Shop Sales\ninc. GST' & Location.ID == '22529')
#All in Non Master File PLUs

shop.sales <- subset(main, subset = (main$Location.ID == '22529'
                                   & main$Item == 'Shop Sales\ninc. GST'
                                   & (main$Fuel.Grade == 'Non Master File PLUs')
                                   & !is.na(main$Calendar.day)),
                   select = c(Location.ID, Location, Week.end.date, Calendar.day, Value))
colnames(shop.sales)[colnames(shop.sales)=='Value'] <- 'shop.sales'
summary(shop.sales)

mod2 <- merge(mod1, shop.sales, by = c('Location.ID','Location','Week.end.date', 'Calendar.day'))

#Check merge
dim(shop.sales)
dim(mod1)
dim(mod2)
summary(shop.sales[shop.sales$Calendar.day != as.Date('2012-12-31'),])
summary(mod2[mod2$Fuel.Grade == 'E10',])
summary(mod2[mod2$Fuel.Grade == 'Vtx 95',])
#Dates not consistent over fuel types and shop sales...
#This date has no price data and was dropped
summary(mod1)
summary(mod2)
shop.sales[shop.sales$Location.ID == '22529' & shop.sales$Location == 'Site 7' &
             shop.sales$Week.end.date == as.Date('2013-01-13'),]
mod2[mod2$Location.ID == '22529' & mod2$Location == 'Site 7' &
       mod2$Week.end.date == as.Date('2013-01-13'),]

#What to do with weeks without complete data for every day?
#Leave for now but bear in mind when modelling
chk <- as.data.frame(xtabs(formula = ~ Location.ID + Location + Fuel.Grade + Week.end.date,
                           data = mod2))
chk1 <- subset(chk, subset = (chk$Location.ID == '22529'
                              & chk$Location == 'Site 7'
                              & (chk$Fuel.Grade == 'E10' | chk$Fuel.Grade == 'Vtx 95')))
chk1[chk1$Freq < 7,]

main[main$Location.ID == '22529' & (main$Fuel.Grade == 'E10' | main$Fuel.Grade == 'Vtx 95')
     & main$Week.end.date == as.Date('2013-06-30'),]

#Additional variables
mod2$Cal.year <- as.factor(format(mod2$Calendar.day, '%Y'))
mod2$fuel.revenue <- mod2$quantity*mod2$price.Caltex
mod2$dow = factor(weekdays(mod2$Calendar.day),
                  c('Monday','Tuesday','Wednesday','Thursday','Friday','Saturday','Sunday'))
mod2$month = factor(format(mod2$Calendar.day,'%m'))

mod2$level.shift <- factor(rep(0,length(mod2$Location)), c(0,1))
mod2$level.shift[mod2$Fuel.Grade=='E10' & mod2$Calendar.day > as.Date('2014-02-14')] <- 1
mod2$Sat <- factor(rep(0,length(mod2$Location)), c(0,1))
mod2$Sat[mod2$dow == 'Saturday'] <- 1
mod2$Sun <- factor(rep(0,length(mod2$Location)), c(0,1))
mod2$Sun[mod2$dow == 'Sunday'] <- 1

#Price variables
mod2$pricediff.pc.mkt <- (mod2$price.Caltex/mod2$price.mktavg.exCaltex - 1)*100
mod2$pricediff.pc.BP <- (mod2$price.Caltex/mod2$price.BP - 1)*100
mod2$pricediff.pc.Coles <- (mod2$price.Caltex/mod2$price.Coles - 1)*100
mod2$pricediff.pc.711 <- (mod2$price.Caltex/mod2$price.711 - 1)*100

mod2$pricediff.cpl.mkt <- (mod2$price.Caltex - mod2$price.mktavg.exCaltex)
mod2$pricediff.cpl.BP <- (mod2$price.Caltex - mod2$price.BP)
mod2$pricediff.cpl.Coles <- (mod2$price.Caltex - mod2$price.Coles)
mod2$pricediff.cpl.711 <- (mod2$price.Caltex - mod2$price.711)
summary(mod2)

#Merge on profit data by Location, Posting.date = Calendar.day
#Assume UNLEADED.PETROL = E10 and VORTEX = Vtx 95
head(profit)

profit2 <- subset(profit,
                  subset = (Location == 'Site 7' & Posting.date >=as.Date('2013-01-01')
                            & Posting.date <= as.Date('2015-06-18')),
                  select = c(Location, Posting.date, VORTEX, UNLEADED.PETROL, year, week, yyyyww))
colnames(profit2)[colnames(profit2) == 'Posting.date'] <- 'Calendar.day'
colnames(profit2)[colnames(profit2) == 'VORTEX'] <- 'profit.vtx'
colnames(profit2)[colnames(profit2) == 'UNLEADED.PETROL'] <- 'profit.e10'
summary(profit2)

mod3 <-  merge(mod2, profit2, by = c('Location', 'Calendar.day'), all = TRUE)

#Check merge
dim(profit2)
dim(mod2)
dim(mod3)
summary(profit2)
summary(mod2)
summary(mod3)

summary(mod3[is.na(mod3$Fuel.Grade),])
#Profit data starts before other data, subset before merge
summary(mod3[is.na(mod3$week),])
x <- mod3[is.na(mod3$week) & mod3$Fuel.Grade == 'E10','Calendar.day']
y <- weekdays(x)
#Seems a bit random, keep the days with missing data

#Delete profit information that in on inappropriate for the fuel grade
mod3$profit.vtx[mod3$Fuel.Grade == 'E10'] <- NA
mod3$profit.e10[mod3$Fuel.Grade == 'Vtx 95'] <- NA

#Fill in missing week from available data
mod3$Calendar.day[is.na(mod3$week)]
head(mod3[mod3$Fuel.Grade == 'E10' & mod3$yyyyww == '201402',],7)
wk <- sprintf('%02d',(floor(as.numeric(mod3$Calendar.day - as.Date('2012-12-31'))/7)) %% 52 + 1)
summary(mod3[wk != mod3$week & !is.na(mod3$week),]) #All match OK

mod3$week[is.na(mod3$week)] <- wk[is.na(mod3$week)]
#Check year convention
mod3[as.character(mod3$year) != as.character(mod3$Cal.year), c('Calendar.day', 'year')]
mod3$year[is.na(mod3$year)] <- format(mod3$Calendar.day[is.na(mod3$year)],'%Y')
mod3$year[mod3$Calendar.day >= as.Date('2013-12-30')
          & mod3$Calendar.day <= as.Date('2013-12-31')] <- '2014'
mod3$year[mod3$Calendar.day >= as.Date('2014-12-29')
          & mod3$Calendar.day <= as.Date('2014-12-31')] <- '2015'
mod3$yyyyww[is.na(mod3$yyyyww)] <- paste(mod3$year[is.na(mod3$yyyyww)], 
                                         mod3$week[is.na(mod3$yyyyww)], sep = '')

head(mod3)
summary(mod3)

#Add on external data
analysis.data <- merge(mod3, external.data, by = 'Calendar.day', all.x = TRUE)
summary(analysis.data)


#Clean up
rm(list = c('chk', 'chk1', 'Loc22529', 'main', 'mod1', 'mod2', 'profit', 'quantity', 'shop.sales',
            'mod3', 'awe', 'cpi', 'data', 'days', 'profit2', 'rainfall', 'test', 'tmp', 'wk',
            'z'))
