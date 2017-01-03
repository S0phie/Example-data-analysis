#Build models
load("~/Documents/Caltex/Analysis workspace.Rdata")
setwd('/Users/Sophie/Documents/Caltex')
colnames(analysis.data)
#Candidate variables:
#Variables to test
# c("Fuel.Grade", "price.Caltex", "price.BP", "price.711", "price.Coles",
#  "price.mktavg", "price.mktavg.exCaltex", "quantity", "shop.sales",
#  "Cal.year", "fuel.revenue", "dow", "month", "level.shift", "Sat", "Sun",
#  "pricediff.pc.mkt", "pricediff.pc.BP", "pricediff.pc.Coles", "pricediff.pc.711",
#  "pricediff.cpl.mkt", "pricediff.cpl.BP", "pricediff.cpl.Coles",
#  "pricediff.cpl.711", "yyyyww", "am.rainfall", "rain.days", "CPI.lin",
#  "CPI.step", "AWE.lin", "AWE.step", "cheapest", "price.rank","nam.pl") 

fuel <- 'E10'
# E10, Vtx 95
target <- 'quantity'
#quantity, profit.e10, profit.vtx
fam <- 'poisson'
#Poisson for quantity, normal for nam (I guess?)
vars <- c('month', 'level.shift', 'price.mktavg.exCaltex', 'dow', 'month',
          'pricediff.cpl.mkt', 'rain.days', 'CPI.lin', 'AWE.lin', 'nam.pl')

vars <- c('month')
  
mod <- glm(as.formula(target ~ paste(vars, collapse = '+')), family = poisson,
           data = analysis.data,
           subset = Fuel.Grade == 'E10')

mod <- glm(as.formula(quantity ~ month + level.shift), family = poisson,
           data = analysis.data,
           subset = Fuel.Grade == 'E10',
           contrasts = TRUE)

summary(mod)
anova(mod)
plot(mod)
