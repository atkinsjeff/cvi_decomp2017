#04_soil_probe_data.R
# Jeff Atkins (May, 2017) jwatkins6@vcu.edu or via Twitter @atkinsjeff

require(ggplot2)

# Import Soil Probe Data collected with WesternAG probes (2010-2011)
probe.data <- read.csv(file = "./data/soil_probe.csv", head = TRUE)
probe.data$ID <- paste(substr(probe.data$ELEV,1,1), substr(probe.data$VEG,1,1), sep = "")
probe.data$PERIOD <- factor(probe.data$PERIOD, levels= c("JUNE", "AUGUST", "SEPTEMBER"))


probe.N.totals <- aggregate(Total.N ~ ID + PERIOD, FUN=mean, data=probe.data)
probe.N.sd <- aggregate(Total.N ~ ID + PERIOD, FUN=SD, data=probe.data)

probe.NO3.totals <- aggregate(NO3~ ID + PERIOD, FUN=mean, data=probe.data)
probe.NO3.sd <- aggregate(NO3~ ID+ PERIOD, FUN=SD, data=probe.data)


probe.NH4.totals <- aggregate(NH4~ ID + PERIOD, FUN=mean, data=probe.data)
probe.NH4.sd <- aggregate(NH4~ ID + PERIOD, FUN=SD, data=probe.data)

#Looking at data visually for diagnostics
probe.N.totals
p.N.probe <- ggplot(probe.N.totals, aes(x = ID, y = Total.N))+
  geom_bar(stat = "identity")

#subsetting data
probe.june <- subset(probe.data, PERIOD == "JUNE")
probe.aug <- subset(probe.data, PERIOD == "AUGUST")
probe.sep <- subset(probe.data, PERIOD == "SEPTEMBER")

#It's unnormal
shapiro.test(probe.data$Total.N)
hist(probe.june$Total.N)
qqnorm(probe.june$Total.N)

hist(log(probe.june$Total.N))
shapiro.test(log10(probe.june$Total.N))
qqnorm(log(probe.june$Total.N))

#Statisics by month

summary(aov(log(Total.N) ~ ELEV + VEG +ELEV*VEG, data = probe.june))
summary(aov(log(NH4) ~ ELEV + VEG +ELEV*VEG, data = probe.june))
summary(aov(log(NO3) ~ ELEV + VEG +ELEV*VEG, data = probe.june))

summary(aov(log(Total.N) ~ ELEV + VEG +ELEV*VEG, data = probe.aug))
summary(aov(log(NH4) ~ ELEV + VEG +ELEV*VEG, data = probe.aug))
summary(aov(log(NO3) ~ ELEV + VEG +ELEV*VEG, data = probe.aug))

summary(aov(log(Total.N) ~ ELEV + VEG +ELEV*VEG, data = probe.sep))
summary(aov(log(NH4) ~ ELEV + VEG +ELEV*VEG, data = probe.sep))
summary(aov(log(NO3) ~ ELEV + VEG +ELEV*VEG, data = probe.sep))

#looking at differences
june.n <- aov(log(Total.N) ~ ELEV + VEG +ELEV*VEG, data = probe.june)
TukeyHSD(june.n, "VEG", order = TRUE)
plot(TukeyHSD(june.n, "VEG"))

june.NO3 <- aov(log(NO3) ~ ELEV + VEG +ELEV*VEG, data = probe.june)
TukeyHSD(june.NO3, "VEG", order = TRUE)



#Graphin'

nitrate.label = expression(paste("NO"[3]*""^"-"))
ammo.label = expression(paste("NH"[4]*""^"+"))
#First we need the means by ID

#exporting to csv
# write.csv(probe.N.totals, file="data/total_N.csv")
# write.csv(probe.NO3.totals, file="data/total_nitrate.csv")
# write.csv(probe.NH4.totals, file="data/total_ammon.csv")


p.N.all <- ggplot(probe.N.totals, aes(x=ID, y=Total.N, fill=PERIOD)) +
  geom_bar(stat="identity", position=position_dodge(), colour="black")+
  theme_classic()+
  xlab("")+
  ylab("Total N")+
  theme(axis.title.x= element_text(size=20), 
        axis.text.x = element_text(size=(18)))+
  theme(axis.title.y= element_text(size=20), 
        axis.text.y = element_text(size=(18)))+
  theme(legend.justification=c(1,1), legend.position=c(1,1), legend.title=element_blank())
p.N.all

p.NO3.all <- ggplot(probe.NO3.totals, aes(x=ID, y=NO3, fill=PERIOD)) +
  geom_bar(stat="identity", position=position_dodge(), colour="black")+
  theme_classic()+
  xlab("")+
  ylab(nitrate.label)+
  theme(axis.title.x= element_text(size=20), 
        axis.text.x = element_text(size=(18)))+
  theme(axis.title.y= element_text(size=20), 
        axis.text.y = element_text(size=(18)))+
  theme(legend.position="none")
p.NO3.all

p.NH4.all <- ggplot(probe.NH4.totals, aes(x=ID, y=NH4, fill=PERIOD)) +
  geom_bar(stat="identity", position=position_dodge(), colour="black")+
  theme_classic()+
  xlab("")+
  ylab(ammo.label)+
  theme(axis.title.x= element_text(size=20), 
        axis.text.x = element_text(size=(18)))+
  theme(axis.title.y= element_text(size=20), 
        axis.text.y = element_text(size=(18)))+
  theme(legend.position="none")
p.NH4.all
# Let's look and see if there are actually differences though.
probe.june <- subset(probe.data, PERIOD == "JUNE")

probe.june$Nratio <-  probe.june$NH4 / probe.june$NO3



ggplot(probe.june, aes(x = VEG, y = Total.N))+
  geom_bar(stat="identity")

ggplot(probe.june, aes(x = VEG, y = NO3))+
  geom_bar(stat="identity")

ggplot (probe.june, aes(x = VEG, y = NH4))+
  geom_bar(stat="identity")


#August
probe.aug <- subset(probe.data, PERIOD == "AUGUST")

probe.aug$Nratio <-  probe.aug$NH4 / probe.aug$NO3





ggplot(probe.aug, aes(x = VEG, y = Total.N))+
  geom_bar(stat="identity")

ggplot(probe.aug, aes(x = VEG, y = NO3))+
  geom_bar(stat="identity")

ggplot(probe.aug, aes(x = VEG, y = NH4))+
  geom_bar(stat="identity")


#September
probe.sep <- subset(probe.data, PERIOD == "SEPTEMBER")

probe.sep$Nratio <-  probe.sep$NH4 / probe.sep$NO3





ggplot(probe.sep, aes(x = VEG, y = Total.N))+
  geom_bar(stat="identity")

ggplot(probe.sep, aes(x = VEG, y = NO3))+
  geom_bar(stat="identity")

ggplot(probe.sep, aes(x = VEG, y = NH4))+
  geom_bar(stat="identity")


ggplot(probe.aug, aes(x = NH4, y = NO3, color = ELEV))+
  geom_point(size = 4)

ggplot(probe.june, aes(x = NH4, y = NO3, color = ELEV))+
  geom_point(size = 4)

# Looking at nitrate to ammonium differences
p.nitrate <- ggplot(probe.data, aes(x=NO3, y=NH4, color=VEG))+
  geom_point(size=4)+
  ylab(ammo.label)+
  xlab(nitrate.label)+
  theme(axis.title.x= element_text(size=20), 
        axis.text.x = element_text(size=(18)))+
  theme(axis.title.y= element_text(size=20), 
        axis.text.y = element_text(size=(18)))+
  theme(legend.position=c(0.9,0.9))+
  stat_smooth(method = lm, se = FALSE)

p.nitrate
