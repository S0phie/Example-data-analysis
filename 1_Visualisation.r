#Visualisation
setwd('/Users/Sophie/Documents/Caltex')
# library(xtable)

#0. Import and format data
setwd('/Users/Sophie/Documents/Caltex')
data <- read.csv('data_for_visualisation_task.csv')
summary(data)

data$Store.ID <- as.factor(data$Store.ID)
data$Postcode <- as.factor(data$Postcode)

data$E10[data$E10 == 0] <- 'No'
data$E10[data$E10 == 1] <- 'Yes'
data$E10 <- as.factor(data$E10)

data$ULP[data$ULP == 0] <- 'No'
data$ULP[data$ULP == 1] <- 'Yes'
data$ULP <- as.factor(data$ULP)

data$PULP[data$PULP == 0] <- 'No'
data$PULP[data$PULP == 1] <- 'Yes'
data$PULP <- as.factor(data$PULP)

data$VX98[data$VX98 == 0] <- 'No'
data$VX98[data$VX98 == 1] <- 'Yes'
data$VX98 <- as.factor(data$VX98)

data$LPG[data$LPG == 0] <- 'No'
data$LPG[data$LPG == 1] <- 'Yes'
data$LPG <- as.factor(data$LPG)

data$Diesel[data$Diesel == 0] <- 'No'
data$Diesel[data$Diesel == 1] <- 'Yes'
data$Diesel <- as.factor(data$Diesel)

data$Biodiesel[data$Biodiesel == 0] <- 'No'
data$Biodiesel[data$Biodiesel == 1] <- 'Yes'
data$Biodiesel <- as.factor(data$Biodiesel)

data$Vortex.Diesel[data$Vortex.Diesel == 0] <- 'No'
data$Vortex.Diesel[data$Vortex.Diesel == 1] <- 'Yes'
data$Vortex.Diesel <- as.factor(data$Vortex.Diesel)

data$Eftpos[data$Eftpos == 0] <- 'No'
data$Eftpos[data$Eftpos == 1] <- 'Yes'
data$Eftpos <- as.factor(data$Eftpos)

data$OpenAllHours[data$OpenAllHours == 0] <- 'No'
data$OpenAllHours[data$OpenAllHours == 1] <- 'Yes'
data$OpenAllHours <- as.factor(data$OpenAllHours)

data$Disabled.Toilet[data$Disabled.Toilet == 0] <- 'No'
data$Disabled.Toilet[data$Disabled.Toilet == 1] <- 'Yes'
data$Disabled.Toilet <- as.factor(data$Disabled.Toilet)

data$ATM[data$ATM == 0] <- 'No'
data$ATM[data$ATM == 1] <- 'Yes'
data$ATM <- as.factor(data$ATM)

data$BBQ.Gas[data$BBQ.Gas == 0] <- 'No'
data$BBQ.Gas[data$BBQ.Gas == 1] <- 'Yes'
data$BBQ.Gas <- as.factor(data$BBQ.Gas)

data$E.FLEX[data$E.FLEX == 0] <- 'No'
data$E.FLEX[data$E.FLEX == 1] <- 'Yes'
data$E.FLEX <- as.factor(data$E.FLEX)

data$VX95[data$VX95 == 0] <- 'No'
data$VX95[data$VX95 == 1] <- 'Yes'
data$VX95 <- as.factor(data$VX95)

data$Workshop <- as.character(data$Workshop)
data$Workshop[data$Workshop == ''] <- "No"
data$Workshop <- as.factor(data$Workshop)

data$Carwash <- as.character(data$Carwash)
data$Carwash[data$Carwash == ''] <- "No"
data$Carwash <- as.factor(data$Carwash)

data$Starcash[data$Starcash == FALSE] <- 'No'
data$Starcash[data$Starcash == TRUE] <- 'Yes'
data$Starcash <- as.factor(data$Starcash)

summary(data)

data[data$State == '',]
#Store.ID 3210 is missing state - Suburb is Granville so fill in NSW
data$State[data$Store.ID == '3210'] <- 'NSW'
data$State <- factor(as.character(data$State),
                        levels = c('ACT', 'NSW', 'VIC', 'QLD', 'SA', 'WA', 'TAS', 'NT'))

#Export as csv for Google Sheets
# write.csv(data, file='Vis_for_sheets.csv')

###Plots for presentation###
#State
# 
# State.summ <- cbind(as.data.frame(xtabs(Total.Revenue.2015 ~ State, data = data)),
#       as.data.frame(xtabs(Total.Revenue.2014~ State, data = data))[,'Freq'],
#       as.data.frame(xtabs(Total.Revenue.2013 ~ State, data = data))[,'Freq'],
#       as.data.frame(xtabs( ~ State, data = data))[,'Freq'])
# 
# colnames(State.summ) <- c('State', 'Total_Revenue_2015', 'Total_Revenue_2014',
#                           'Total_Revenue_2013', 'Stores')
# State.summ$Average_Revenue_2015 <- State.summ$Total_Revenue_2015/State.summ$Stores
# State.summ$Average_Revenue_2014 <- State.summ$Total_Revenue_2014/State.summ$Stores
# State.summ$Average_Revenue_2013 <- State.summ$Total_Revenue_2013/State.summ$Stores
# 
# State.summ[,c('State','Average_Revenue_2013','Average_Revenue_2014','Average_Revenue_2015')]

# barplot(t(as.matrix(State.summ[,c('Average_Revenue_2013','Average_Revenue_2014',
#                                 'Average_Revenue_2015')]))/1000,
#         names.arg = State.summ$State,
#         beside = T,
#         legend.text = c('2013', '2014', '2015'),
#         col = c('skyblue', 'lightpink', 'lightgreen'),
#         border = NA,
#         xlab = 'State',
#         ylab = 'Average Revenue ($\'000)',
#         ylim = c(0, 2000),
#         main = 'Average Revenue by State\n2013-2015')
# grid()


#1. Summary tables and charts
make.summaries <- function(summvar){
  tmp <- cbind(as.data.frame(xtabs(as.numeric(Total.Revenue.2015) ~ eval(parse(text=summvar)), data = data)),
                      as.data.frame(xtabs(as.numeric(Total.Revenue.2014) ~ eval(parse(text=summvar)), data = data))[,'Freq'],
                      as.data.frame(xtabs(as.numeric(Total.Revenue.2013) ~ eval(parse(text=summvar)), data = data))[,'Freq'],
                      as.data.frame(xtabs( ~ eval(parse(text=summvar)), data = data))[,'Freq'])
  
  colnames(tmp) <- c('x', 'Total_Revenue_2015', 'Total_Revenue_2014',
                            'Total_Revenue_2013', 'Stores')
  tmp$Average_Revenue_2015 <- tmp$Total_Revenue_2015/tmp$Stores
  tmp$Average_Revenue_2014 <- tmp$Total_Revenue_2014/tmp$Stores
  tmp$Average_Revenue_2013 <- tmp$Total_Revenue_2013/tmp$Stores
  
  jpeg(filename = paste(summvar,".jpg", sep = ''))
  barplot(t(as.matrix(tmp[,c('Average_Revenue_2013','Average_Revenue_2014',
                                    'Average_Revenue_2015')]))/1000,
          names.arg = tmp$x,
          beside = T,
          legend.text = c('2013', '2014', '2015'),
          col = c('skyblue', 'lightpink', 'lightgreen'),
          border = NA,
          xlab = summvar,
          ylab = 'Average Revenue ($\'000)',
          ylim = c(0, 2000),
          main = paste('Average Revenue by ', summvar, '\n2013-2015', sep = ''))
  # lines(x = tmp$x, y = tmp$Stores)
  grid()
  dev.off()
}

make.summaries('State')
lines(x = c(1:length(State.summ$State)),
      y = c(State.summ$Stores))

summvars <- colnames(data)[-c(27:31, 5:7, 1, 3)]
for (x in summvars) {
  make.summaries(x)
}

#Fix for brand labels
summvar <- 'Brand'

data$Brand <- as.character(data$Brand)
data$Brand[data$Brand == 'CALTEX SAFEWAY' | data$Brand == 'SAFEWAY CALTEX'] <-
  'CALTEX\nSAFEWAY'
data$Brand[data$Brand == 'CALTEX WOOLWORTHS' | data$Brand == 'WOOLWORTHS CALTEX'] <-
  'CALTEX\nWOOLWORTHS'
# data$Brand[data$Brand == 'INDEPENDENT'] <-
  # 'CALTEX\nWOOLWORTHS'
data$Brand <- as.factor(data$Brand)

tmp <- cbind(as.data.frame(xtabs(as.numeric(Total.Revenue.2015) ~ eval(parse(text=summvar)), data = data)),
             as.data.frame(xtabs(as.numeric(Total.Revenue.2014) ~ eval(parse(text=summvar)), data = data))[,'Freq'],
             as.data.frame(xtabs(as.numeric(Total.Revenue.2013) ~ eval(parse(text=summvar)), data = data))[,'Freq'],
             as.data.frame(xtabs( ~ eval(parse(text=summvar)), data = data))[,'Freq'])

colnames(tmp) <- c('x', 'Total_Revenue_2015', 'Total_Revenue_2014',
                   'Total_Revenue_2013', 'Stores')
tmp$Average_Revenue_2015 <- tmp$Total_Revenue_2015/tmp$Stores
tmp$Average_Revenue_2014 <- tmp$Total_Revenue_2014/tmp$Stores
tmp$Average_Revenue_2013 <- tmp$Total_Revenue_2013/tmp$Stores

jpeg(filename = paste(summvar,".jpg", sep = ''))

barplot(t(as.matrix(tmp[,c('Average_Revenue_2013','Average_Revenue_2014',
                           'Average_Revenue_2015')]))/1000,
        names.arg = tmp$x,
        beside = T,
        legend.text = c('2013', '2014', '2015'),
        col = c('skyblue', 'lightpink', 'lightgreen'),
        border = NA,
        xlab = summvar,
        ylab = 'Average Revenue ($\'000)',
        ylim = c(0, 2000),
        main = paste('Average Revenue by ', summvar, '\n2013-2015', sep = ''),
        # horiz = T,
        cex.names = 0.6)
# lines(x = tmp$x, y = tmp$Stores)
grid()
dev.off()

