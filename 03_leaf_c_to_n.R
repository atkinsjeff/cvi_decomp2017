#03_leaf_c_to_n.R
# Jeff Atkins (jwatkins6@vcu.edu)
#Litterbag analysis of carbon to nitrogen from litterbag experiment in Weimer Run, Watershed, Davis, WV on the property of the former Canaan Valley Inst. 
# now Little Canaan WMA

#Experiment ran from October 2011 until October 2013
#Each litter bag contained approx 2 g of dried, fresh litter all from yellow birch (Betula Alleghanesis)

# Most of this script is processing the raw data into means and variances

#Author: Jeff Atkins (jeffatkins@virginia.edu)
#importing necessary libraries
library(ggplot2)
library(plyr)
require(wesanderson)

#custom plot theme
theme_fivethirtyeight2 <- function(base_size = 13, base_family = "") {
  theme_grey(base_size = base_size, base_family = base_family) %+replace%
    theme(
      
      # Base elements which are not used directly but inherited by others
      line =              element_line(colour = '#DADADA', size = 0.75, 
                                       linetype = 1, lineend = "butt"),
      rect =              element_rect(fill = "#F0F0F0", colour = "#F0F0F0", 
                                       size = 0.5, linetype = 1),
      text =              element_text(family = base_family, face = "plain",
                                       colour = "#656565", size = base_size,
                                       hjust = 0.5, vjust = 0.5, angle = 0, 
                                       lineheight = 0.9),
      
      # Modified inheritance structure of text element
      plot.title =        element_text(size = rel(1.5), family = '' , 
                                       face = 'bold', hjust = -0.05, 
                                       vjust = 1.5, colour = '#3B3B3B'),
      axis.text =         element_text(),
      
      # Modified inheritance structure of line element
      axis.ticks =        element_line(),
      panel.grid.major =  element_line(),
      panel.grid.minor =  element_blank(),
      
      # Modified inheritance structure of rect element
      plot.background =   element_rect(),
      panel.background =  element_rect(),
      legend.key =        element_rect(colour = '#DADADA'),
      
      # Modifiying legend.position
      legend.position = 'none',
      
      complete = TRUE
    )
}


#import litterbag .csv file
leaves <-read.csv(file="./data/leaves_dec_2015.csv", head=TRUE, strip.white=TRUE )

#structure of df leaves
str(leaves)
head(leaves)

leavesx <- merge(leaves, litter[, c("ID", "COLLECT", "REMAINper", "k", "days")], by = "ID")

#Adding in initial N, C, CN values
N.low <- mean(2.231, 2.215, 2.339, 2.260)
N.mid <- mean(2.478, 2.503, 2.508, 2.486)
N.high <- mean(2.511, 2.515, 2.381, 2.397)

sd(2.231, 2.215, 2.339, 2.260)
sd(c(2.478, 2.503, 2.508, 2.486))
sd(c(2.511, 2.515, 2.381, 2.397))

C.low <- mean(51.079, 50.782, 51.342, 51.346)
C.mid <- mean(52.697, 52.423, 52.048, 51.676)
C.high <- mean(53.266, 52.676, 52.141, 52.229)


sd(c(51.079, 50.782, 51.342, 51.346))
sd(c(52.697, 52.423, 52.048, 51.676))
sd(c(53.266, 52.676, 52.141, 52.229))

attach(leavesx)
leavesx$initialN[ELEV == "LOW"] <- N.low
leavesx$initialN[ELEV == "MID"] <- N.mid
leavesx$initialN[ELEV == "HIGH"] <- N.high
leavesx$initialC[ELEV == "LOW"] <- C.low
leavesx$initialC[ELEV == "MID"] <- C.mid
leavesx$initialC[ELEV == "HIGH"] <- C.high
detach(leavesx)

#makes CN
leavesx$initialCN <- leavesx$initialC / leavesx$initialN

#finding the difference from before in percent of intiial
leavesx$newN <- (leavesx$N / leavesx$initialN) * 100
leavesx$newC <- (leavesx$C / leavesx$initialC) * 100
leavesx$newCN <- ((leavesx$C / leavesx$N) / initialCN) * 100
leavesx$CN <- leavesx$C / leavesx$N


Nmeans <- aggregate(formula = N~ELEV+VEG+days, data=leavesx, FUN=mean)
Cmeans <- aggregate(formula = C~ELEV+VEG+days, data=leavesx, FUN=mean)
CNmeans <- aggregate(formula = CN~ELEV+COLLECT, data=leavesx, FUN=mean)
CNmeansSD <- aggregate(formula = CN~ELEV+COLLECT, data=leavesx, FUN=SD)
CNmeansN <- aggregate(formula= CN~ELEV+COLLECT, data=leavesx, FUN=length)

CNmeansN <- plyr::rename(CNmeansN, c("CN" = "N"))
CNmeansSD <- plyr::rename(CNmeansSD, c("CN" = "SD"))

plot(Nmeans$days, Nmeans$N)
plot(Cmeans$days, Cmeans$C)

CNall <- merge(CNmeans, CNmeansN)
CNall <- merge(CNall, CNmeansSD)

#making standard error
CNall$SE <- CNall$SD / (sqrt(CNall$N))


CNelev <- CNall


#I ran this through while deleting the ELEV statement instead of recoding all of it to get the VEG
# CNmeans <- aggregate(formula= CN~VEG+COLLECT, data=leavesx, FUN=mean)
# CNmeansSD <- aggregate(formula= CN~VEG+COLLECT, data=leavesx, FUN=sd)
# CNmeansN <- aggregate(formula= CN~VEG+COLLECT, data=leavesx, FUN=length)
# 
# CNmeansN <-rename(CNmeansN, c("CN" = "N"))
# CNmeansSD <- rename(CNmeansSD, c("CN" = "SD"))
# 
# 
# #making a new ID column for this guy
# 
# # CNmeans$ID <- paste(substring(CNmeans$ELEV,1,1), substring(CNmeans$VEG,1,1), substring(CNmeans$COLLECT,1,1), sep="")
# # CNmeans$ID <- as.factor(CNmeans$ID)
# # 
# # CNmeansN$ID <- paste(substring(CNmeansN$ELEV,1,1), substring(CNmeansN$VEG,1,1), substring(CNmeansN$COLLECT,1,1), sep="")
# # CNmeansN$ID <- as.factor(CNmeansN$ID)
# # 
# # CNmeansSD$ID <- paste(substring(CNmeansSD$ELEV,1,1), substring(CNmeansSD$VEG,1,1), substring(CNmeansSD$COLLECT,1,1), sep="")
# # CNmeansSD$ID <- as.factor(CNmeansSD$ID)
# 
# CNall <- merge(CNmeans, CNmeansN)
# CNall <- merge(CNall, CNmeansSD)
# 
# #making standard error
# CNall$SE <- CNall$SD / (sqrt(CNall$N))
# 
# CNveg <- CNall


attach(CNelev)
CNelev$month[COLLECT == 0] <-"Initial"
CNelev$month[COLLECT == 1] <-"1 month"
CNelev$month[COLLECT == 2] <-"8 months"
CNelev$month[COLLECT == 3] <-"12 months"
CNelev$month[COLLECT == 4] <-"20 months"
CNelev$month[COLLECT == 5] <-"24 months"
detach(CNelev)

attach(CNveg)
CNveg$month[COLLECT == 0] <-"Initial"
CNveg$month[COLLECT == 1] <-"1 month"
CNveg$month[COLLECT == 2] <-"8 months"
CNveg$month[COLLECT == 3] <-"12 months"
CNveg$month[COLLECT == 4] <-"20 months"
CNveg$month[COLLECT == 5] <-"24 months"
detach(CNveg)

CNveg$month <- as.factor(CNveg$month)
CNveg$month <-factor(CNveg$month, levels(CNveg$month)[c(1,5,2:4)])

CNelev$month <- as.factor(CNelev$month)
CNelev$month <-factor(CNelev$month, levels(CNelev$month)[c(1,5,2:4)])


p.CNveg <- ggplot(CNveg, aes(x=month, y = CN, shape = VEG))+
  geom_point(size=4)+
  geom_errorbar(aes(ymin=CN - SE, ymax=CN+SE), width=.1)+
  theme_classic()+
  theme(axis.text=element_text(size=16),
        axis.title=element_text(size=20))+
  ylab("Litter C:N Ratio")+
  xlab("")+
  ylim(10,30)+
  theme(legend.justification=c(0,1), legend.position=c(0,1))+
  theme(legend.text = element_text( size = 16))+
  theme(legend.title=element_blank())+
  theme(axis.line.x = element_line(color="black", size = 0.5),
        axis.line.y = element_line(color="black", size = 0.5))

x11()
p.CNveg


p.CNelev <- ggplot(CNelev, aes(x=month, y = CN, shape = ELEV))+
  geom_point(size=4)+
  geom_errorbar(aes(ymin=CN - SE, ymax=CN+SE), width=.1)+
  theme_classic()+
  theme(axis.text=element_text(size=16),
        axis.title=element_text(size=20))+
  ylab("Litter C:N Ratio")+
  xlab("")+
  ylim(10,30)+
  theme(legend.justification=c(0,1), legend.position=c(0,1))+
  theme(legend.text = element_text( size = 16))+
  theme(legend.title=element_blank())+
  theme(axis.line.x = element_line(color="black", size = 0.5),
        axis.line.y = element_line(color="black", size = 0.5))

p.CNelev


#CN by veg and elev
attach(CNveg)
aggregate(CN, by = list(VEG, COLLECT), FUN=mean)
detach(CNveg)

attach(CNelev)
aggregate(CN, by = list(ELEV, COLLECT), FUN=mean)
detach(CNelev)

#importing necessary libraries
library(ggplot2)
library(plyr)
require(wesanderson)


#import litterbag .csv file
leaves <- read.csv(file = "./data/leaves_dec_2015.csv", head=TRUE, strip.white=TRUE )
litter <- read.csv(file = "./data/litter_post_processing.csv")

#structure of df leaves
str(leaves)
head(leaves)

leavesx <- merge(leaves, litter[, c("ID", "COLLECT", "REMAINper", "k", "days")], by = "ID")
#Adding in initial N, C, CN values
N.low <- mean(2.231, 2.215, 2.339, 2.260)
N.mid <- mean(2.478, 2.503, 2.508, 2.486)
N.high <- mean(2.511, 2.515, 2.381, 2.397)

C.low <- mean(51.079, 50.782, 51.342, 51.346)
C.mid <- mean(52.697, 52.423, 52.048, 51.676)
C.high <- mean(53.266, 52.676, 52.141, 52.229)

attach(leavesx)
leavesx$initialN[ELEV == "LOW"] <- N.low
leavesx$initialN[ELEV == "MID"] <- N.mid
leavesx$initialN[ELEV == "HIGH"] <- N.high
leavesx$initialC[ELEV == "LOW"] <- C.low
leavesx$initialC[ELEV == "MID"] <- C.mid
leavesx$initialC[ELEV == "HIGH"] <- C.high
detach(leavesx)

#makes CN
leavesx$initialCN <- leavesx$initialC / leavesx$initialN

#finding the difference from before in percent of intiial
leavesx$newN <- (leavesx$N / leavesx$initialN) * 100
leavesx$newC <- (leavesx$C / leavesx$initialC) * 100
leavesx$newCN <- ((leavesx$C / leavesx$N) / leavesx$initialCN) * 100
leavesx$CN <- leavesx$C / leavesx$N

Nmeans <- aggregate(formula= N~ELEV+VEG+days, data=leavesx, FUN=mean)
Cmeans <- aggregate(formula= C~ELEV+VEG+days, data=leavesx, FUN=mean)
CNmeans <- aggregate(formula= CN~ELEV+VEG+COLLECT, data=leavesx, FUN=mean)
CNmeansSD <- aggregate(formula= CN~ELEV+VEG+COLLECT, data=leavesx, FUN=SD)
CNmeansN <- aggregate(formula= CN~ELEV+VEG+COLLECT, data=leavesx, FUN=length)

CNmeansN <- plyr::rename(CNmeansN, c("CN" = "N"))
CNmeansSD <- plyr::rename(CNmeansSD, c("CN" = "SD"))


#making a new ID column for this guy

CNmeans$ID <- paste(substring(CNmeans$ELEV,1,1), substring(CNmeans$VEG,1,1), substring(CNmeans$COLLECT,1,1), sep="")
CNmeans$ID <- as.factor(CNmeans$ID)

CNmeansN$ID <- paste(substring(CNmeansN$ELEV,1,1), substring(CNmeansN$VEG,1,1), substring(CNmeansN$COLLECT,1,1), sep="")
CNmeansN$ID <- as.factor(CNmeansN$ID)

CNmeansSD$ID <- paste(substring(CNmeansSD$ELEV,1,1), substring(CNmeansSD$VEG,1,1), substring(CNmeansSD$COLLECT,1,1), sep="")
CNmeansSD$ID <- as.factor(CNmeansSD$ID)

# This combines all the mess of math above to create CN ratios w/ standard deviations 
CNall <- merge(CNmeans, CNmeansN)
CNall <- merge(CNall, CNmeansSD)

#making standard error
CNall$SE <- CNall$SD / (sqrt(CNall$N))

# Now doing Nitrogen
NVEGmeans <- aggregate(formula= newN~VEG+days, data=leavesx, FUN=mean)
NVEGmeansSD <-aggregate(formula= newN~VEG+days, data=leavesx, FUN=SD)
#making a new ID column for this guy


#adding zero rows to make plots make sense
NVEGmeans[nrow(NVEGmeans)+1,] <- c("CANOPY",0, 100)
NVEGmeans[nrow(NVEGmeans)+1,] <- c("OPEN",0, 100)
NVEGmeans[nrow(NVEGmeans)+1,] <- c("SHRUB",0, 100)

#however the previous step fucks up the classes. stringsasfactors=false related
NVEGmeans$newN <- as.numeric(NVEGmeans$newN)
NVEGmeans$days <- as.numeric(NVEGmeans$days)

VEGmeansN <- aggregate(formula= REMAINper~VEG+COLLECT, data=litter, FUN=length)
VEGmeansN$REMAINper <- as.numeric(VEGmeansN$REMAINper)


#redoing veg of litter decomp means by days
VEGmeans2 <- aggregate(formula= REMAINper~VEG+days, data=leavesx, FUN=mean)
VEGmeansSD2 <-aggregate(formula= REMAINper~VEG+days, data=leavesx, FUN=SD)

VEGmeans2 <- merge(VEGmeans2, VEGmeansSD2, by=c("VEG", "days") )
VEGmeans2 <- plyr::rename(VEGmeans2, c("REMAINper.x" = "mean_per"))
VEGmeans2 <- plyr::rename(VEGmeans2, c("REMAINper.y" = "sd"))
VEGmeans2$mean_per <- VEGmeans2$mean_per * 100
VEGmeans2$sd <- VEGmeans2$sd * 100

# Merging everything
allveg <- merge(NVEGmeans, VEGmeans2, by = c("days", "VEG"))

origin.o <- c( 0, "OPEN", 100, 100, 0)
origin.c <- c(0, "CANOPY", 100, 100, 0)
origin.s <- c(0, "SHRUB", 100, 100, 0)

allveg <- rbind(allveg, origin.o, origin.c, origin.s)
allveg$days <- as.numeric(allveg$days)
allveg$mean_per <- as.numeric(allveg$mean_per)
allveg$sd <- as.numeric(allveg$sd)
allveg$newN <- as.numeric(allveg$newN)

allveg$VEG <- factor(allveg$VEG, levels=c("OPEN", "CANOPY", "SHRUB"))


###COMBO veg plot
x11()
pNveg <- ggplot(allveg, aes(x=days, y=newN, group=VEG, color = VEG))+
  geom_line(size=1.5)+
  geom_line(data = allveg, aes(x=days, y=mean_per), size=1.5)+
  geom_point()+
  scale_colour_manual(values=cbPalette)+
  theme(axis.text.x = element_text(size=(16), colour="black"))+
  theme(axis.title.y= element_text(size=16),
        axis.title.x = element_text(size=16),
        axis.text.y = element_text(size=(16), colour="black"),
        strip.text.x= element_text(size = (16), colour="black"))+
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        panel.border = element_blank(),
        axis.line = element_line(colour = "black"))+
  # theme(legend.justification=c(-0.1,-0.1), legend.position=c(0,0),
  #       legend.title = element_blank())+
  ylab("ORIGINAL REMAINING (%)")+
  xlab("Days")+
  geom_hline(yintercept = 100)+
  scale_y_continuous(breaks=c(40,60,80,100,120,140,160))+
  theme(legend.position="bottom", legend.title=element_blank(),
        legend.text = element_text( size=20),
        legend.key = element_rect(fill = "white", colour = NA, size = 0.25))+
  guides(linetype = guide_legend(keywidth = 4, keyheight = 1))+
  #   scale_y_continuous(breaks=seq(40,180,20))+
  annotate("text", x=550, y=160, label=("Litter N"), size=10)+
  annotate("text", x=125, y= 45, label=("Litter Mass"), size=10)
pNveg

#diagnostic plot to look at CN by collection
x11()
pNveg
p.CN <- ggplot(CNmeans, aes(x = COLLECT, y = CN))+
  geom_point(size= 4)
p.CN + facet_grid(ELEV~ .)


#############
##ELEVATION

NELEVmeans <- aggregate(formula= newN~ELEV+days, data=leavesx, FUN=mean)
NELEVmeansSD <-aggregate(formula= newN~ELEV+days, data=leavesx, FUN=SD)
#adding zero rows to make plots make sense
NELEVmeans[nrow(NELEVmeans)+1,] <- c("HIGH",0, 100)
NELEVmeans[nrow(NELEVmeans)+1,] <- c("MID",0, 100)
NELEVmeans[nrow(NELEVmeans)+1,] <- c("LOW",0, 100)

#however the previous step fucks up the classes. stringsasfactors=false related
NELEVmeans$newN <- as.numeric(NELEVmeans$newN)
NELEVmeans$days <- as.numeric(NELEVmeans$days)

ELEVmeansN <- aggregate(formula= REMAINper~ELEV+COLLECT, data=litter, FUN=length)
ELEVmeansN$REMAINper <- as.numeric(ELEVmeansN$REMAINper)


#redoing ELEV of litter decomp means by days
ELEVmeans2 <- aggregate(formula= REMAINper~ELEV+days, data=leavesx, FUN=mean)
ELEVmeansSD2 <-aggregate(formula= REMAINper~ELEV+days, data=leavesx, FUN=SD)

ELEVmeans2 <-merge(ELEVmeans2, ELEVmeansSD2, by=c("ELEV", "days") )
ELEVmeans2 <-plyr::rename(ELEVmeans2, c("REMAINper.x" = "mean_per"))
ELEVmeans2 <-plyr::rename(ELEVmeans2, c("REMAINper.y" = "sd"))
ELEVmeans2$mean_per <- ELEVmeans2$mean_per * 100
ELEVmeans2$sd <- ELEVmeans2$sd * 100

allELEV <- merge(NELEVmeans, ELEVmeans2, by = c("days", "ELEV"))
allELEV$ELEV <- factor(allELEV$ELEV, levels=c("LOW", "MID", "HIGH"))
###COMBO ELEV plot
darjeelingmod <- c("#F98400", "#5BBCD6", "#046C9A")

#ORIGINAL PLOT
#pNELEV <- ggplot(allELEV, aes(x=days, y=newN,  group=ELEV))+
# geom_line(size=1.5, aes(linetype=ELEV))+
#   scale_linetype_manual(values=c("dashed","dotted",  "solid"))+
#   geom_point()+
#   geom_line(data = allELEV, aes(x=days, y=mean_per, linetype=ELEV), size=1.5)+
#   geom_point()+
#   theme_bw()+
#   #   theme_fivethirtyeight2()+
#   scale_color_manual(values = darjeelingmod)+
#   theme(panel.grid.major = element_blank(),
#         panel.grid.minor = element_blank())+ 
#   theme(axis.title.x = element_text(size=18),
#         axis.title.y = element_text(size=18),
#         axis.text.y = element_text(size=18),
#         axis.text.x = element_text(size =18))+
#   ylab("ORIGINAL REMAINING (%)")+
#   xlab("Days")+
#   geom_hline(yintercept = 100)+
#   scale_y_continuous(breaks=c(40,60,80,100,120,140,160))+
#   theme(legend.position="bottom", legend.title=element_blank(),
#         legend.text = element_text( size=20))+
#   guides(linetype = guide_legend(keywidth = 4, keyheight = 1))+
#   #   scale_y_continuous(breaks=seq(40,180,20))+
#   annotate("text", x=550, y=160, label=("Litter N"), size=10)+
#   annotate("text", x=125, y= 45, label=("Litter Mass"), size=10)
origin.low <- c( 0, "LOW", 100, 100, 0)
origin.mid <- c(0, "MID", 100, 100, 0)
origin.high <- c(0, "HIGH", 100, 100, 0)

allELEV <- rbind(allELEV, origin.low, origin.mid, origin.high)
allELEV$days <- as.numeric(allELEV$days)
allELEV$mean_per <- as.numeric(allELEV$mean_per)
allELEV$sd <- as.numeric(allELEV$sd)
allELEV$newN <- as.numeric(allELEV$newN)

# Elevation plot
pNELEV <- ggplot(allELEV, aes(x=days, y=newN,  group=ELEV, color = ELEV))+
  geom_line(size=1.5)+
  #scale_linetype_manual(values=c("dashed","dotted",  "solid"))+
  geom_point()+
  geom_line(data = allELEV, aes(x=days, y=mean_per), size=1.5)+
  geom_point()+
  
  #   theme_fivethirtyeight2()+
  scale_colour_manual(values=cbPalette)+
  theme(axis.text.x = element_text(size=(16), colour="black"))+
  theme(axis.title.y= element_text(size=16),
        axis.title.x = element_text(size=16),
        axis.text.y = element_text(size=(16), colour="black"),
        strip.text.x= element_text(size = (16), colour="black"))+
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        panel.border = element_blank(),
        axis.line = element_line(colour = "black"))+
  # theme(legend.justification=c(-0.1,-0.1), legend.position=c(0,0),
  #       legend.title = element_blank())+
  ylab("ORIGINAL REMAINING (%)")+
  xlab("Days")+
  geom_hline(yintercept = 100)+
  scale_y_continuous(breaks=c(40,60,80,100,120,140,160))+
  theme(legend.position="bottom", legend.title=element_blank(),
        legend.text = element_text( size=20))+
  guides(linetype = guide_legend(keywidth = 4, keyheight = 1))+
  #   scale_y_continuous(breaks=seq(40,180,20))+
  annotate("text", x=550, y=160, label=("Litter N"), size=10)+
  annotate("text", x=125, y= 45, label=("Litter Mass"), size=10)

x11()
pNELEV



