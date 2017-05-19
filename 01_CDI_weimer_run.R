##01_CDI_weimer_run
# Author: Jeff Atkins (jwatkins6@vcu.edu) May, 2017

#Previous sections of code taken as below:  

#Code written 2014-015 by Jeff Atkins (jeffatkins@virginia.edu or via Twitter @atkinsjeff)
#This code analyzes data from the MesoWest Weather Station at Bearden Knob (BDKW2) located at the top of the Weimer Run Watershed in David, WV
#This code also analyzes long term trends in precipitation using NCDC data for the Weimer Run Watershed
#Dataset files are uploaded into figshare
#and is in suppor of research outlined in the manuscript Atkins et al. 2014 Biogeosciences (article title final here)

# Station data downloaded from http://mesowest.utah.edu/ for the Bearden Knob Weather Station (BDKW2)
# 
# http://mesowest.utah.edu/html/help/main_index.html#usage
# 
# Column descriptions and units:
# DATETIME - date (mm/dd/yyyy) with time (hh:mm)
# TEMP - air temperature in degrees Celsius
# RELH - Relative humidity (%)
# WIND - hourly wind speed (m/s)
# GUST - peak wind speed gust during hour (m/s)
# DIR - wind speed direction (compass degrees)
# QFLG - Quality of data (OK = good to use; CAUTION = examine for incongruity)
# SOLAR - Incoming solar radiation (w/m^2)
# PRECIPcum - cumulative precipitation (cm)
# PRECIP - precip during past hour (cm)
# PEAK - duplicate column of max wind gust (m/s)
# PEAKDIR - direction of PEAK and GUST columns (compass degrees)
# DWP - dew point (degrees Celsius)
# 


#required packages
require(xts)
require(ggplot2)
require(psych)
library(Hmisc)
library(lmtest)
library(car)
library(gtable)
library(gridExtra)
require(lubridate)
require(plyr)
require(dplyr)
require(tidyr)


# Sources
# http://www.nature.com/nclimate/journal/v4/n7/extref/nclimate2251-s1.pdf
# http://onlinelibrary.wiley.com/doi/10.1111/j.1365-2486.2008.01672.x/full
# http://hyperphysics.phy-astr.gsu.edu/hbase/Kinetic/watvap.html
# https://www.fs.fed.us/pnw/pubs/journals/pnw_2008_adair001.pdf
# http://hydrologie.org/redbooks/a063/063006.pdf

require(plyr)
require(dplyr)
require(tidyr)
require(magrittr)

# NCDC GHCND:USC00462211	DAVIS 3 SE WV US
ncdc <- read.csv("data/934907.csv")

str(ncdc)

# make year, month,  columns
ncdc$year <- substr(ncdc$DATE, 0, 4)
ncdc$mon <- substr(ncdc$DATE, 5, 6)
ncdc$day <- substr(ncdc$DATE, 7, 8)

#mean temp
ncdc$TAVG <- (ncdc$TMAX + ncdc$TMIN) / 2

# diagnostic
plot(ncdc$TAVG)

# making monthlies
ncdc %>%
  group_by( year, mon) %>% 
  summarise( tavg = mean(TAVG, na.rm = TRUE), ppt = sum(PRCP, na.rm = TRUE)) -> monthly
monthly <- data.frame(monthly)

monthly$year <- as.integer(monthly$year)
monthly$mon <- as.integer(monthly$mon)

# yearly sums
# making monthlies
ncdc %>%
  group_by( year) %>% 
  summarise( ppt = sum(PRCP, na.rm = TRUE)) 

# Briong in daylength
df.daylength <- read.csv("data/wv_daylength_2010_2012.csv")
df.daylength$length <- df.daylength$length * 12

#make a lite
ncdc.lite <- ncdc[,c(7,19:22)]
ncdc.lite$year <- as.integer(ncdc.lite$year)
ncdc.lite$mon <- as.integer(ncdc.lite$mon)
ncdc.lite$day <- as.integer(ncdc.lite$day)

df.ncdc <- merge(ncdc.lite, df.daylength)
attach(df.ncdc)
df.days2 <- df.ncdc[order(year, mon, day),]
detach(df.ncdc)
df.days2 <- na.omit(df.days2)

# # calculate saturated water vapor density in g m^-3
df.days2$qtingrams <- 4.8046 * exp(0.0635 * df.days2$TAVG)

# # calculate saturated water vapor density in kPa
df.days2$qt <- 0.611 * exp(  (17.502 * df.days2$TAVG) / (df.days2$TAVG + 240.97) )

# # calculate PET in inches from Harmon, 1963 (http://hydrologie.org/redbooks/a063/063006.pdf)
# df.days$pet <- 0.0065 * df.days$length * df.days$qt

# PET in mm modified from Harmon, 1963
df.days2$pet <- 29.8 * df.days2$length * ( (df.days2$qt/ (df.days2$TAVG + 273.2)))

# apply calibration coeff
df.days2$pet <- df.days2$pet * 1.2
# #change pet to mm
# df.days$pet <- df.days$pet * 25.4

# aggregate monthly pet
df.days2 %>% 
  group_by( year, mon) %>% 
  summarise( pet = sum(pet, na.rm = TRUE)) -> df.pet
df.pet <-   data.frame(df.pet)
# test plot
plot(df.days2$pet, xlab = "day since origin (01-01-2010)", ylab = "PET (mm/day)")
# Just to print total  PET
df.days2 %>% 
  group_by( year) %>% 
  summarise( pet = sum(pet, na.rm = TRUE))
#######
# To bring them all together
df.cdi <- merge(monthly, df.pet)


#convert precip to mm and rename

names(df.cdi)[names(df.cdi) == 'ppt'] <- 'p'
names(df.cdi)[names(df.cdi) == 'tavg'] <- 't'

# Running checks
df.cdi%>% 
  group_by( year) %>% 
  summarise( ppt = sum(p, na.rm = TRUE)) 

# making the intermediates for CDI calculations
# from Adiar et al. 2008, GCB (https://www.fs.fed.us/pnw/pubs/journals/pnw_2008_adair001.pdf)

df.cdi$ft <- 0.5766 * exp(308.56 * ( (1/56.02) - (1 / ((273+df.cdi$t) - 227.13)) ))
df.cdi$fw <- 1 / (1 + 30 * exp(-8.5 * (df.cdi$p / df.cdi$pet)))
df.cdi$cdi <- df.cdi$ft * df.cdi$fw

## Now let's calculate CDI by year
df.cdi%>% 
  group_by(year) %>% 
  summarise( cdi = mean(cdi, na.rm = TRUE)) 


cdi <- mean(df.cdi$cdi)
#k <- -0.5278
k = 0.4042
x <- c(1:730)
x <- data.frame(x)
x$x <- x$x / 365

# calculate mass remaining based on CDI, time, and k
x$mt <- 100  * exp( (-k * cdi * x$x)  )

plot(x$x, x$mt)

#### Three pool model
cel <- 36.79
lig <- 20.94
ls <-  lig/ (cel + lig)
k1 <- 1.893
k2 <- 1.5817
k3 <- 0.0343
b <- 4.271
m1 <- 100 - cel - lig
x$ligx <- (lig * exp(-k3 * cdi *x$x))



x$mt3plus <- (m1  * exp( (-k1 * cdi * x$x))) + (cel * exp( (-k2 * cdi * exp(-b * (x$ligx/lig) * x$x)))) + (lig * exp(-k3 * cdi *x$x))
plot(x$x, x$mt3plus) 

x$mt3 <- (m1  * exp( (-1.0669 * cdi * x$x))) + (cel * exp( (-0.7676 * cdi * x$x))) + (lig * exp(0 * cdi *x$x))

plot(x$x, x$mt3)      







