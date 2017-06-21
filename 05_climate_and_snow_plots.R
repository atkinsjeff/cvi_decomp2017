##05_climate_and_snow_plots
# Author: Jeff Atkins (jwatkins6@vcu.edu) May, 2017

require(plyr)
require(magrittr)
require(ggplot2)
require(zoo)
require(lubridate)

# Snow data taken from NCSC
snow.depth <- read.csv("data/CV_snowdata_winter_2010_2012.csv", header=TRUE)

snow.depth[snow.depth == "-9999"] = NA

snow.depth$date <- as.POSIXct(snow.depth$date, format="%m/%d/%Y")

snow.depth$snowdepth_mm <- snow.depth$snowdepth_in * 25.4



p.2010 <-ggplot(snow.depth, aes( x = date, y = snowdepth_mm, color = winter)) +
  geom_point(size=0) +
  geom_line(size=1)+
  theme_classic()+
  xlim(as.POSIXct(c("2010-12-01","2011-04-01" )))+
  ylim(c(0,650))+
  ylab("Snow Depth (mm)")+
  xlab("")+
  theme(axis.title.y = element_text(margin=margin(0,20,0,0)), 
        axis.title = element_text(size=20),
        axis.text = element_text(size=16),
        axis.ticks.x = element_blank())+
  theme(legend.justification=c(1,1), legend.position=c(1,1))+
  theme(legend.text = element_text( size = 16))+
  scale_colour_discrete(name  ="")
p.2010

p.2011 <-ggplot(snow.depth, aes( x = date, y = snowdepth_mm, color = winter)) +
  geom_point(size=0) +
  geom_line(size=1)+
  theme_classic()+
  xlim(as.POSIXct(c("2011-12-01","2012-04-01" )))+
  ylim(c(0,650))+
  ylab("Snow Depth (mm)")+
  xlab("")+
  theme(axis.title.y = element_text(margin=margin(0,20,0,0)),
        axis.title = element_text(size=20), 
        axis.text = element_text(size=16),
        axis.ticks.x = element_blank())+
  theme(legend.position="none")

p.2011

p.2012 <-ggplot(snow.depth, aes( x = date, y = snowdepth_mm, color = winter)) +
  geom_point(size=0) +
  geom_line(size=1)+
  theme_classic()+
  xlim(as.POSIXct(c("2012-12-01","2013-04-01" )))+
  ylim(c(0,650))+
  ylab("Snow Depth (mm)")+
  xlab("")+
  theme(axis.title.y = element_text(margin=margin(0,20,0,0)),
        axis.title = element_text(size=20), 
        axis.text = element_text(size=16),
        axis.ticks.x = element_blank())+
  theme(legend.position="none")

p.2012

########
# Air temperature

air.temp <- read.csv("data/NCDC_temps.csv", header=TRUE)

#Now we will convert tenths of a degree C to degrees C
air.temp[,8:12] <- apply(air.temp[,8:12], 1:2, function(x) x * 0.1)
air.temp

#converting data to readable format
air.temp$DATE <- as.POSIXct(air.temp$DATE, format="%m/%d/%Y")
str(air.temp)

#Making ribbon plots of temperature. Hopefully
p.temp11 <- ggplot(air.temp, aes( x = DATE, y = MNTM))+
  geom_ribbon(aes(ymin=MMNT, ymax=MMXT), fill ="light grey", color="light grey")+
  geom_line(color="dark grey", lwd=1)+
  theme_classic()+
  xlim(as.POSIXct(c("2011-10-01","2012-06-01" )))

p.temp11

p.temp12 <- ggplot(air.temp, aes( x = DATE, y = MNTM))+
  geom_ribbon(aes(ymin=MMNT, ymax=MMXT), fill ="light grey", color="light grey")+
  geom_line(color="dark grey", lwd=1)+
  theme_classic()+
  xlim(as.POSIXct(c("2012-10-01","2013-06-01" )))

p.temp12

###Working with the DAVIS data
# Custom Options:	Station name, Geographic location	
# Stations:	GHCND:USC00462211 - DAVIS 3 SE, WV US
# Data Types:	SNOW - Snowfall (mm)
# TMAX - Maximum temperature (tenths of degrees C)
# TMIN - Minimum temperature (tenths of degrees C)
# PRCP - Precipitation (tenths of mm)
# TOBS - Temperature at the time of observation (tenths of degrees C)
# SNWD - Snow depth (mm)


davis.data <- read.csv("data/ncdc_data_2011_2013.csv", header=TRUE)

#converting dates
davis.data$DATE <- strptime(davis.data$DATE, format="%Y%m%d")
davis.data$graphdates <- as.POSIXlt(davis.data$graphdates, format = "%m/%d/%Y")
# davis.data$DATE <- as.Date(davis.data$DATE, format="%Y%m%d")
# davis.data$DATE <- format(davis.data$DATE, format="%m-%d")
# davis.data$DATE <- as.factor(davis.data$DATE, ordered = TRUE)


str(davis.data)
davis.data <- na.omit(davis.data)



#changing units to mm and degrees C (removing the tenths)
davis.data[,10:12] <- apply(davis.data[,10:12] , 1:2, function(x) x * 0.1)
davis.data$PRCP <- davis.data$PRCP * 0.1


p.temp11 <- ggplot(davis.data, aes(x = graphdates, y = TMAX, fill = WINTER))+
  geom_ribbon(aes(ymin=TMAX, ymax=TMIN), alpha = 0.5)+
  #geom_line(color="light grey", lwd=1)+
  theme_classic()+
  #xlim((c(200,150 )))+
  # ylim(c(-10,20))+
  ylab(expression("Air Temperature ("*~degree*"C)"))+
  xlab("")+
  theme(axis.title.y = element_text(margin=margin(0,20,0,0)),
        axis.title = element_text(size=20), 
        axis.text = element_text(size=16),
        axis.ticks.x = element_blank())+
  theme(legend.justification=c(1,0), legend.position=c(1,0))
p.temp11

p.snowfall <- ggplot(davis.data, aes(x = graphdates, y = SNOW, color = WINTER))+
  geom_line(lwd=1)+
  theme_classic()+
  #xlim((c(200,150 )))+
  # ylim(c(-10,20))+
  ylab(expression("Snow Fall (mm)"))+
  xlab("")+
  theme(axis.title.y = element_text(margin=margin(0,20,0,0)),
        axis.title = element_text(size=20), 
        axis.text = element_text(size=16),
        axis.ticks.x = element_blank())+
  theme(legend.justification=c(1,1), legend.position=c(1,1))
p.snowfall


dates <- c(20110101,	20110102,	20110103,	20110104,	20110105,	20110106,	20110107,	20120101,	20120102,	20120103,	20120104,	20120105,	20120106,	20120107)
Tmin <- c(-5,	-4,	-2,	-10,	-2,	-1,	2,	-10,	-11,	-12,	-10,	-5,	-3,	-1)
Tmax <- c(3,	5,	6,	4, 2, 7,	9,	-1,	0,	1,	2,	1,	4,	8)
winter <- c("ONE","ONE","ONE","ONE","ONE","ONE","ONE","TWO","TWO","TWO","TWO","TWO","TWO","TWO")

df <- data.frame(cbind(dates, Tmin, Tmax, winter))
df$dates <- strptime(df$dates, format="%Y%m%d")
df$Tmax <- as.numeric(df$Tmax)
df$Tmin <- as.numeric(df$Tmin)
str(df)

ggplot(df, aes(x = dates, y =Tmax))+
  geom_ribbon(aes(ymin=Tmax, ymax=Tmin), fill ="darkgrey", color="dark grey")

df$yday <- yday(df$dates)
df$year <- year(df$dates)
ggplot(df, aes(yday)) + geom_ribbon(aes(ymax=Tmax, ymin=Tmin, fill=factor(year)), alpha=.5)

# Snow totals
tapply(davis.data$SNOW, davis.data$WINTER, FUN=sum)

x2011 <- subset(davis.data, WINTER == "2011-2012")
x2012 <- subset(davis.data, WINTER == "2012-2013")

#how often they are at zero
sums2011 <- (colSums(x2011==0)/nrow(x2011)*100)
sums2012 <- (colSums(x2012==0)/nrow(x2012)*100)

#how often greater than zero
sums2011g <- (colSums(x2011 > 0)/nrow(x2011)*100)
sums2012g <- (colSums(x2012 > 0)/nrow(x2012)*100)






