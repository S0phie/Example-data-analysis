#Exploratory analysis
load("~/Documents/Caltex/Analysis workspace.Rdata")
setwd('/Users/Sophie/Documents/Caltex')
detach(analysis.data)
attach(analysis.data)
#Time series plot
plot(Calendar.day[Fuel.Grade == 'E10'], quantity[Fuel.Grade == 'E10'], type = 'l',
     col = 'gray', xlab = 'Sale date', ylab = 'Fuel quantity (L)',
     main = 'E10 fuel quantity sales by sale date')
lines(Calendar.day[Fuel.Grade == 'E10'],
      smooth.spline(quantity[Fuel.Grade == 'E10'], spar = 0.4)$y,
      col='blue')
abline(v = as.Date('2014-02-14'), col = 'red')
par(new=T)
plot(Calendar.day[Fuel.Grade == 'E10'],
      -AWE.step[Fuel.Grade == 'E10'],
      col='green',
      type = 'l')


#Box plots by day of week, split pre and post 2014-02-14
boxplot(quantity[Fuel.Grade == 'E10'] ~ dow[Fuel.Grade == 'E10'] +
          level.shift[Fuel.Grade == 'E10'])
#Flag Saturday and Sundays
boxplot(quantity[Fuel.Grade == 'E10'] ~ Sat[Fuel.Grade == 'E10'] +
          Sun[Fuel.Grade == 'E10'] + level.shift[Fuel.Grade == 'E10'])
#Box plots by month, split pre and post 2014-02-14
boxplot(quantity[Fuel.Grade == 'E10'] ~ month[Fuel.Grade == 'E10'] +
          level.shift[Fuel.Grade == 'E10'])

#Plot quantity vs price
selector = Fuel.Grade == 'E10' & level.shift == 0

library(hexbin)
bin<-hexbin(price.Caltex[selector], quantity[selector],
            xbins=50, xlab = 'Price (cpl)',
            ylab = 'Quantity (L)') 

plot(bin, main="E10 Fuel purchased vs price",
     colramp = colorRampPalette(c('gray','red')))
plot(price.Caltex[selector], quantity[selector])

#Average difference in price
plot(pricediff.pc.mkt[selector], quantity[selector])
plot(pricediff.cpl.mkt[selector], quantity[selector])

#BP
plot(pricediff.pc.BP[selector], quantity[selector])
plot(pricediff.cpl.BP[selector], quantity[selector])

#Coles
plot(pricediff.pc.Coles[selector], quantity[selector])
plot(pricediff.cpl.Coles[selector], quantity[selector])

#711
plot(pricediff.pc.711[selector], quantity[selector])
plot(pricediff.cpl.711[selector], quantity[selector])

selector = Fuel.Grade == 'E10'
init <- glm(quantity ~ month + dow + level.shift + price.Caltex + pricediff.cpl.mkt,
            family = poisson(link='log'), data = analysis.data, subset = selector)
summary(init)

#Plot prices
plot(Calendar.day, price.Caltex, type = 'l', col = 'gray')
lines(Calendar.day, price.711, col = 2)
lines(Calendar.day, price.BP, col = 3)
lines(Calendar.day, price.Coles, col = 4)

plot(c(as.Date('2013-01-01'), as.Date('2015-06-30')), c(-8,8), type = 'n')
points(Calendar.day,pricediff.cpl.BP)
points(Calendar.day,pricediff.cpl.711, col = 'red')
points(Calendar.day,pricediff.cpl.Coles, col = 'blue')

analysis.data$cheapest <- as.factor(as.numeric(analysis.data$price.Caltex <= 
  apply(analysis.data[,c('price.BP', 'price.711', 'price.Coles')], 1, min)))

analysis.data$price.rank <- as.factor(
  apply(analysis.data[,c('price.Caltex', 'price.BP', 'price.711', 'price.Coles')],
        1, rank, ties.method = 'min')['price.Caltex',]
)

boxplot(quantity ~ price.rank + level.shift)
par(mfrow = c(2,2))
plot(c(0,10000), c(0,0.0004), type = 'n')
hist(quantity[price.rank == 1], freq = F, add = T)
plot(c(0,10000), c(0,0.0004), type = 'n')
hist(quantity[price.rank == 2], freq = F, add = T)
plot(c(0,10000), c(0,0.0004), type = 'n')
hist(quantity[price.rank == 3], freq = F, add = T)
plot(c(0,10000), c(0,0.0004), type = 'n')
hist(quantity[price.rank == 4], freq = F, add = T)
#Very similar
summary(analysis.data)
attach(analysis.data)
detach(analysis.data)

subber = Fuel.Grade == 'E10'
par(mfrow = c(2,1))
plot(price.Caltex[subber], profit.e10[subber])
plot(quantity[subber], profit.e10[subber])
par(mfrow = c(1,1))
plot(price.Caltex[subber], (fuel.revenue[subber] - profit.e10[subber])/
       quantity[subber])
analysis.data$nam.pl[Fuel.Grade == 'E10']  <- ((analysis.data$profit.e10)
                         / analysis.data$quantity)[Fuel.Grade == 'E10'] 

analysis.data$nam.pl[Fuel.Grade == 'Vtx 95']  <- ((analysis.data$profit.vtx)
                         / analysis.data$quantity)[Fuel.Grade == 'Vtx 95'] 

subber = Fuel.Grade == 'Vtx 95'
par(mfrow = c(2,1))
plot(price.Caltex[subber], profit.vtx[subber])
plot(quantity[subber], profit.vtx[subber])
par(mfrow = c(1,1))
plot(price.Caltex[subber], (fuel.revenue[subber] - profit.vtx[subber])/
       quantity[subber])
#Linear, can derive a margin per litre, two outlying points for Vortex


library(hexbin)
selector <- nam.pl < 1 & Fuel.Grade == 'E10'
bin<-hexbin(sqrt(nam.pl)[selector], quantity[selector],
            xbins=50, xlab = 'sqrt NAM per Litre (cpL)',
            ylab = 'Quantity (L)') 
plot(bin)

basic <- glm(quantity ~ sqrt(nam.pl), family = poisson, data = analysis.data,
    subset = nam.pl < 1 & Fuel.Grade == 'E10')
summary(basic)
anova(basic)
plot(basic)

