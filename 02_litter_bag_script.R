#Litterbag analysis from litterbag experiment in Weimer Run, Watershed, Davis, WV on the property of the former Canaan Valley Inst. 
# now Little Canaan WMA

#Experiment ran from October 2011 until October 2013
#Each litter bag contained approx 2 g of dried, fresh litter all from yellow birch (Betula Alleghanesis)

#importing necessary libraries
library(ggplot2)
library(plyr)
require(wesanderson)


#import litterbag .csv file
litter <-read.csv(file="data/litterbags.csv",head=TRUE, strip.white=TRUE )

#structure of df litter
str(litter)
head(litter)

#Make the COLLECT column a factor so that it can be used for analysis of collection period
#and restructuring COMMENT so that it is listed as characters/text
litter$COLLECT <- as.factor(litter$COLLECT)
litter$COMMENT <- as.character(litter$COMMENT)

#filling in time in years for collection for later analysis
attach(litter)
litter$time[COLLECT == 0] <-0
litter$time[COLLECT == 1] <-0.083333
litter$time[COLLECT == 2] <-0.66667
litter$time[COLLECT == 3] <-1
litter$time[COLLECT == 4] <-1.66667
litter$time[COLLECT == 5] <-2
detach(litter)

attach(litter)
litter$month[COLLECT == 0] <-"Initial"
litter$month[COLLECT == 1] <-"1 month"
litter$month[COLLECT == 2] <-"8 months"
litter$month[COLLECT == 3] <-"12 months"
litter$month[COLLECT == 4] <-"20 months"
litter$month[COLLECT == 5] <-"24 months"
detach(litter)
litter$days <- litter$time * 365        #The litter time column is in portion of a year. 

#what was the mean and error of pre litter weights

mean(litter$PRE)
var(litter$PRE)
range(litter$PRE)     #Need to check lo1-1

#Shapiro-Wilk normality
shapiro.test(litter$REMAINper)
# qqnorm(litter$REMAINper)
# 
# #output shows our data are not normally distrubted so we move to a logit transformation
# shapiro.test(log10(litter$REMAINper))
# # qqnorm(litter$logit)
# 
# #arcsine transformation
# trans.arcsine <- function(x){
#   asin(sign(x) * sqrt(abs(x)))
# }
# litter$arcsine <- trans.arcsine(litter$REMAINper)
# 
# shapiro.test(litter$arcsine)
# # qqnorm(litter$arcsine)
# 
# #two way anova for differences in percent mass loss (litter$REMAINper)
# fitLOST <- aov(litter$REMAINper ~ litter$ELEV + litter$VEG + litter$ELEV:litter$VEG, data=litter)
# fitBOB <- aov(litter$REMAINper ~ litter$ELEV*litter$VEG*litter$COLLECT, data=litter)
# 
# summary(fitLOST)
# summary(fitBOB)

##############
##########################
# Calculating k by years

litter$kt <- log (litter$REMAINper)
litter$k <- ((litter$kt/litter$time))

shapiro.test(litter$k)

#calculating k by days

litter$k.days <- ((litter$kt/litter$days))

no.initial <- subset(litter, COLLECT != 0)
shapiro.test(no.initial$k.days)
hist(no.initial$k.days)

#kruskal.wallis test on all decomp rates
kruskal.test(k ~ VEG, data = no.initial)
kruskal.test(k ~ ELEV, data = no.initial)

#let's look at data over time first

p.mass.loss <- ggplot(litter, aes(y=REMAINper, x=days, color=ELEV))+
  #geom_line(aes(linetype=VEG), sie=1)+
  geom_point(size=3, fill="white")+
  stat_smooth(method="glm", family = binomial)
p.mass.loss

x11()
p.mass.loss <- ggplot(litter, aes(y=REMAINper, x=days, color=VEG))+
  #geom_line(aes(linetype=VEG), sie=1)+
  geom_point(size=3, fill="white")+
  geom_line()
stat_smooth(method="glm", method.args = list(family = gaussian(lin = "log")))+
  stat_function(fun = function(x) 1  * exp( (-0.5278 * 0.605 * litter$days)))
p.mass.loss

# CDI from other script (01_CDI_weimer_run.R)
cdi <- 0.6057899
k <- -0.4042
f <- c(1:730)
f <- data.frame(f)
f$f <- f$f / 365

# calculate mass remaining based on CDI, time, and k
x$mt <- 100  * exp( (k * cdi * x$x)  )

plot(x$x, x$mt)

y-intercept = 1
m.veg <- lm(log(REMAINper) ~ 0 + days, offset=rep(1, length(days)), litter)

# Modelling approach using dplyr
require(plyr)
require(dplyr)

d <- data.frame(state=rep(c('NY', 'CA'), c(10, 10)),
                year=rep(1:10, 2),
                response=c(rnorm(10), rnorm(10)))
fitted_models = litter %>% group_by(ELEV) %>% do(model = lm(log(REMAINper) ~ 0 + days, offset=rep(1, length(litter$days)), data = litter))
# Source: local data frame [2 x 2]
# Groups: <by row>
#
#    state   model
#   (fctr)   (chr)
# 1     CA <S3:lm>
# 2     NY <S3:lm>
fitted_models$model

# Apply coef to each model and return a data frame
ldply(models, coef)

# Print the summary of each model
l_ply(models, summary, .print = TRUE)


mass.loss.model = glm(litter$REMAINper ~ litter$days, family = binomial())
summary(mass.loss.model)

k.days.model = lm(exp(litter$k.days) ~ days, data = litter)
summary(k.days.model)

p.k.rate <- ggplot(no.initial, aes(y=k.days, x=days, color=ELEV))+
  geom_point(size=6, alpha=0.7) +
  scale_colour_manual(values = wes_palette("Darjeeling"))+
  theme(legend.justification=c(1,1), legend.position=c(1,1), 
        legend.text=element_text( size=20))+
  theme(axis.title.x= element_text(size=20), 
        axis.text.x = element_text(size=(18)))+
  theme(axis.title.y= element_text(size=20), 
        axis.text.y = element_text(size=(18)))+
  ylab("k")+
  xlab("Days")
#stat_smooth(method=lm, formula= y ~ log(x))
p.k.rate

p.k.rate.v <- ggplot(no.initial, aes(y=k.days, x=days, color=VEG))+
  geom_point(size=6, alpha=0.7) +
  scale_colour_manual(values = wes_palette("Darjeeling"))+
  theme(legend.justification=c(1,1), legend.position=c(1,1), 
        legend.text=element_text( size=20))+
  theme(axis.title.x= element_text(size=20), 
        axis.text.x = element_text(size=(18)))+
  theme(axis.title.y= element_text(size=20), 
        axis.text.y = element_text(size=(18)))+
  ylab("k")+
  xlab("Days")
#stat_smooth(method=lm, formula= y ~ log(x))
p.k.rate.v

################

#subsetting by collection

collect1 <- subset(litter, COLLECT==1)
collect2 <- subset(litter, COLLECT==2)
collect3 <- subset(litter, COLLECT==3)
collect4 <- subset(litter, COLLECT==4)
collect5 <- subset(litter, COLLECT==5)

shapiro.test(collect1$k)
hist(collect1$k.days)

shapiro.test(collect2$k)
hist(collect2$k.days)

shapiro.test(collect3$k)
hist(collect3$k.days)

shapiro.test(collect4$k)
hist(collect4$k.days)

shapiro.test(collect5$k)
hist(collect5$k.days)

shapiro.test(collect1$REMAINper)
shapiro.test(collect2$REMAINper)
shapiro.test(collect3$REMAINper)
shapiro.test(collect4$REMAINper)
shapiro.test(collect5$REMAINper)

#NOT NORMAL
fitone <- aov(k ~ ELEV + VEG + ELEV*VEG, data=collect1)
summary(fitone)

fittwo <- aov(k ~ ELEV + VEG + ELEV*VEG, data=collect2)
summary(fittwo)

#NOT NORMAL
fitthree <- aov(k ~ ELEV + VEG + ELEV*VEG, data=collect3)
summary(fitthree)

fitfour <- aov(k ~ ELEV + VEG + ELEV*VEG, data=collect4)
summary(fitfour)

#NOT NORMAL
fitfive <- aov(k ~ ELEV + VEG + ELEV*VEG, data=collect5)
summary(fitfive)

#analyzing
kw.k.veg1  <- kruskal.test(k ~ VEG, data = collect1)
kw.k.elev1 <- kruskal.test(k ~ ELEV, data = collect1)

kw.k.veg2  <- kruskal.test(k ~ VEG, data = collect2)
kw.k.elev2 <- kruskal.test(k ~ ELEV, data = collect2)

kw.k.veg3  <- kruskal.test(k ~ VEG, data = collect3)
kw.k.elev3 <- kruskal.test(k ~ ELEV, data = collect3)

kw.k.veg4  <- kruskal.test(k ~ VEG, data = collect4)
kw.k.elev4 <- kruskal.test(k ~ ELEV, data = collect4)

kw.k.veg5  <- kruskal.test(k ~ VEG, data = collect5)
kw.k.elev5 <- kruskal.test(k ~ ELEV, data = collect5)
####Using mass lost percentage

kw.veg1  <- kruskal.test(REMAINper ~ VEG, data = collect1)
kw.elev1 <- kruskal.test(REMAINper ~ ELEV, data = collect1)

kw.veg2  <- kruskal.test(REMAINper ~ VEG, data = collect2)
kw.elev2 <- kruskal.test(REMAINper ~ ELEV, data = collect2)

kw.veg3  <- kruskal.test(REMAINper ~ VEG, data = collect3)
kw.elev3 <- kruskal.test(REMAINper ~ ELEV, data = collect3)

kw.veg4  <- kruskal.test(REMAINper ~ VEG, data = collect4)
kw.elev4 <- kruskal.test(REMAINper ~ ELEV, data = collect4)

kw.veg5  <- kruskal.test(REMAINper ~ VEG, data = collect5)
kw.elev5 <- kruskal.test(REMAINper ~ ELEV, data = collect5)

######Extracting information to make table of mass loss!
kw.list = list() 

kw.list[[1]] = kw.veg1
kw.list[[2]] = kw.veg2
kw.list[[3]] = kw.veg3
kw.list[[4]] = kw.veg4
kw.list[[5]] = kw.veg5
kw.list[[6]] = kw.elev1
kw.list[[7]] = kw.elev2
kw.list[[8]] = kw.elev3
kw.list[[9]] = kw.elev4
kw.list[[10]] = kw.elev5

kw.veg.table <- matrix( c(kw.list[[1]]$p.value,
                          kw.list[[2]]$p.value,
                          kw.list[[3]]$p.value,
                          kw.list[[4]]$p.value,
                          kw.list[[5]]$p.value                   
), ncol=1, byrow=TRUE)
colnames(kw.veg.table) <- c( "VEG p-value")
rownames(kw.veg.table) <- 1:5

kw.elev.table <- matrix( c(kw.list[[6]]$p.value,
                           kw.list[[7]]$p.value,
                           kw.list[[8]]$p.value,
                           kw.list[[9]]$p.value,
                           kw.list[[10]]$p.value                   
), ncol=1, byrow=TRUE)
colnames(kw.elev.table) <- c( "ELEV p-value")
rownames(kw.elev.table) <- 1:5

kw.veg.table
kw.elev.table

######Extracting information to make table of decomp rates
kw.k.list = list() 

kw.k.list[[1]] = kw.k.veg1
kw.k.list[[2]] = kw.k.veg2
kw.k.list[[3]] = kw.k.veg3
kw.k.list[[4]] = kw.k.veg4
kw.k.list[[5]] = kw.k.veg5
kw.k.list[[6]] = kw.k.elev1
kw.k.list[[7]] = kw.k.elev2
kw.k.list[[8]] = kw.k.elev3
kw.k.list[[9]] = kw.k.elev4
kw.k.list[[10]] = kw.k.elev5

kw.k.veg.table <- matrix( c(kw.k.list[[1]]$p.value,
                            kw.k.list[[2]]$p.value,
                            kw.k.list[[3]]$p.value,
                            kw.k.list[[4]]$p.value,
                            kw.k.list[[5]]$p.value                   
), ncol=1, byrow=TRUE)
colnames(kw.k.veg.table) <- c( "VEG p-value")
rownames(kw.k.veg.table) <- 1:5

kw.k.elev.table <- matrix( c(kw.k.list[[6]]$p.value,
                             kw.k.list[[7]]$p.value,
                             kw.k.list[[8]]$p.value,
                             kw.k.list[[9]]$p.value,
                             kw.k.list[[10]]$p.value                   
), ncol=1, byrow=TRUE)
colnames(kw.k.elev.table) <- c( "ELEV p-value")
rownames(kw.k.elev.table) <- 1:5

kw.k.veg.table
kw.k.elev.table
with(litter, tapply(REMAINper, COLLECT, mean))

#making repeated measures ANOVa

fitELEV <- aov(REMAINper ~ ELEV + Error(PLOT/ELEV), data=litter)
summary(fitELEV)

fitVEG <- aov(REMAINper ~ VEG + Error(PLOT/VEG), data=litter)
summary(fitVEG)

fitINTERACT <- aov(REMAINper ~ VEG*ELEV + Error(PLOT/ (ELEV*VEG)), data=litter)
summary(fitINTERACT)



pVEG <- ggplot(litter, aes(x=COLLECT, y=REMAINper, color=ELEV))+
  geom_point(size=6)

############################
#VEGETATION
# trying to make box plots to describe data

df <- data.frame(f1=factor(rbinom))
COLLECTmeans <- aggregate(formula= REMAINper~ELEV+VEG+COLLECT, data=litter, FUN=mean)

VEGmeans <- setNames(aggregate(formula= REMAINper~VEG+COLLECT, data=litter, FUN=mean), c("VEG", "COLLECT", "mean_per"))
VEGmeansSD <-setNames(aggregate(formula= REMAINper~VEG+COLLECT, data=litter, FUN=sd), c("VEG", "COLLECT", "sd"))
VEGmeansN <- setNames(aggregate(formula= REMAINper~VEG+COLLECT, data=litter, FUN=length), c("VEG", "COLLECT", "n"))
VEGmeansN$REMAINper <- as.numeric(VEGmeansN$REMAINper)

VEGmeans <-merge(VEGmeans, VEGmeansSD)

VEGmeans <-merge(VEGmeans, VEGmeansN)

#standard error
VEGmeans$SE <- (VEGmeans$sd/ (log(VEGmeans$n-1)))

VEGmeans1 <- subset(VEGmeans, COLLECT==1)

#making box plots of distribution
pVEG1 <- ggplot(collect1, aes(x=VEG, y=REMAINper))+
  geom_boxplot()+
  scale_x_discrete(limits=c("OPEN", "CANOPY", "SHRUB"))+
  scale_y_continuous(limits= c(0,1), expand=c(0,0))+
  theme_classic()+
  labs(y="MASS LOST (%)")+
  theme(axis.title.x=element_blank())+ 
  theme(axis.title.y= element_text(size=32),
        axis.text.x = element_text(size=(21)),
        axis.text.y = element_text(size=(21)))+
  annotate("text",x=1, y=0.25, label= "1 MONTH", color = "black", size = 10)



pVEG1

pVEG2 <- ggplot(collect2, aes(x=VEG, y=REMAINper))+
  geom_boxplot()+
  scale_x_discrete(limits=c("OPEN", "CANOPY", "SHRUB"))+
  scale_y_continuous(limits= c(0,1), expand=c(0,0))+
  theme_classic()+
  theme_classic()+
  labs(y="")+
  theme(axis.title.x=element_blank())+ 
  theme(axis.text.x = element_text(size=(21)),
        axis.text.y = element_blank())+
  annotate("text",x=1, y=0.25, label= "8 MONTHs", color = "black", size = 10)



pVEG2

pVEG3 <- ggplot(collect3, aes(x=VEG, y=REMAINper))+
  geom_boxplot()+
  scale_x_discrete(limits=c("OPEN", "CANOPY", "SHRUB"))+
  scale_y_continuous(limits= c(0,1), expand=c(0,0))+
  theme_classic()+
  labs(y="")+
  theme(axis.title.x=element_blank())+ 
  theme(axis.text.x = element_text(size=(21)),
        axis.text.y = element_blank())+
  annotate("text",x=1, y=0.25, label= "12 MONTHS", color = "black", size = 10)



pVEG3

pVEG4 <- ggplot(collect4, aes(x=VEG, y=REMAINper))+
  geom_boxplot()+
  scale_x_discrete(limits=c("OPEN", "CANOPY", "SHRUB"))+
  scale_y_continuous(limits= c(0,1), expand=c(0,0))+
  theme_classic()+
  labs(y="")+
  theme(axis.title.x=element_blank())+ 
  theme(axis.text.x = element_text(size=(21)),
        axis.text.y = element_blank())+
  annotate("text",x=1, y=0.25, label= "20 MONTHS", color = "black", size = 10)



pVEG4

pVEG5 <- ggplot(collect5, aes(x=VEG, y=REMAINper))+
  geom_boxplot()+
  scale_x_discrete(limits=c("OPEN", "CANOPY", "SHRUB"))+
  scale_y_continuous(limits= c(0,1), expand=c(0,0))+
  theme_classic()+
  labs(y="")+
  theme(axis.title.x=element_blank())+ 
  theme(axis.text.x = element_text(size=(21)),
        axis.text.y = element_blank())+
  annotate("text",x=1, y=0.25, label= "24 MONTHS", color = "black", size = 10)



pVEG5

pVEGmin <- ggplot(collect5, aes(x=VEG, y=REMAINper))+
  geom_boxplot()+
  scale_x_discrete(limits=c("OPEN", "CANOPY", "SHRUB"))+
  scale_y_continuous(limits= c(0,1), expand=c(0,0))+
  theme_minimal()

pVEGmin

#trying facet approach

#making adjustable labels

month_labeller <- function(var, value){
  value <-as.character(value)
  if (var=="COLLECT") {
    value[value== 1] <- "1 month"
    value[value== 2] <-"8 months"
    value[value== 3] <-"12 months"
    value[value== 4] <-"20 months"
    value[value== 5] <-"24 months"
  }
  return(value)
}

pVEG <- ggplot(no.initial, aes(x=VEG, y=(REMAINper*100)))+
  geom_boxplot()+
  scale_x_discrete(limits=c("OPEN", "CANOPY", "SHRUB"))+
  scale_y_continuous(limits= c(0,100), expand=c(0,0))+
  facet_grid(.~COLLECT)+
  theme_bw()+
  labs(y="MASS REMAINING (%)")+
  labs(x="")+
  theme(axis.text.x = element_text(size=(20), colour="black"))+
  theme(axis.title.y= element_text(size=32), 
        axis.text.y = element_text(size=(24), colour="black"),
        strip.text.x= element_text(size = (24), colour="black"))+
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())

pVEG

# Multiple plot function
#
# ggplot objects can be passed in ..., or to plotlist (as a list of ggplot objects)
# - cols:   Number of columns in layout
# - layout: A matrix specifying the layout. If present, 'cols' is ignored.
#
# If the layout is something like matrix(c(1,2,3,3), nrow=2, byrow=TRUE),
# then plot 1 will go in the upper left, 2 will go in the upper right, and
# 3 will go all the way across the bottom.
#
multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL) {
  require(grid)
  
  # Make a list from the ... arguments and plotlist
  plots <- c(list(...), plotlist)
  
  numPlots = length(plots)
  
  # If layout is NULL, then use 'cols' to determine layout
  if (is.null(layout)) {
    # Make the panel
    # ncol: Number of columns of plots
    # nrow: Number of rows needed, calculated from # of cols
    layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                     ncol = cols, nrow = ceiling(numPlots/cols))
  }
  
  if (numPlots==1) {
    print(plots[[1]])
    
  } else {
    # Set up the page
    grid.newpage()
    pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))
    
    # Make each plot, in the correct location
    for (i in 1:numPlots) {
      # Get the i,j matrix positions of the regions that contain this subplot
      matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))
      
      print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                      layout.pos.col = matchidx$col))
    }
  }
}
x11()
multiplot(pVEG1, pVEG2, pVEG3, pVEG4, pVEG5, cols=5)

############################
#ELEVATION
# trying to make box plots to describe data


ELEVmeans <- setNames(aggregate(formula= REMAINper~ELEV+COLLECT, data=litter, FUN=mean), c("ELEV", "COLLECT", "mean_per"))
ELEVmeansSD <- setNames(aggregate(formula= REMAINper~ELEV+COLLECT, data=litter, FUN=sd), c("ELEV", "COLLECT", "sd"))
ELEVmeansN <- setNames(aggregate(formula= REMAINper~ELEV+COLLECT, data=litter, FUN=length), c("ELEV", "COLLECT", "n"))
ELEVmeansN$REMAINper <- as.numeric(ELEVmeansN$REMAINper)

ELEVmeans <-merge(ELEVmeans, ELEVmeansSD)

ELEVmeans <-merge(ELEVmeans, ELEVmeansN)

#standard error
ELEVmeans$SE <- (ELEVmeans$sd/ (log(ELEVmeans$n-1)))

ELEVmeans1 <- subset(ELEVmeans, COLLECT==1)

#making box plots of distribution
pELEV1 <- ggplot(collect1, aes(x=ELEV, y=REMAINper))+
  geom_boxplot()+
  scale_x_discrete(limits=c("LOW", "MID", "HIGH"))+
  scale_y_continuous(limits= c(0,1), expand=c(0,0))+
  theme_classic()+
  labs(y="MASS LOST (%)")+
  theme(axis.title.x=element_blank())+ 
  theme(axis.title.y= element_text(size=32),
        axis.text.x = element_text(size=(21)),
        axis.text.y = element_text(size=(21)))+
  annotate("text",x=1, y=0.25, label= "1 MONTH", color = "black", size = 10)+
  geom_hline(yintercept = 0.974, size = 1, color = "red")



pELEV1

pELEV2 <- ggplot(collect2, aes(x=ELEV, y=REMAINper))+
  geom_boxplot()+
  scale_x_discrete(limits=c("LOW", "MID", "HIGH"))+
  scale_y_continuous(limits= c(0,1), expand=c(0,0))+
  theme_classic()+
  theme_classic()+
  labs(y="")+
  theme(axis.title.x=element_blank())+ 
  theme(axis.text.x = element_text(size=(21)),
        axis.text.y = element_blank())+
  annotate("text",x=1, y=0.25, label= "8 MONTHs", color = "black", size = 10)+
  geom_hline(yintercept = 0.8075, size = 1, color = "red")



pELEV2

pELEV3 <- ggplot(collect3, aes(x=ELEV, y=REMAINper))+
  geom_boxplot()+
  scale_x_discrete(limits=c("LOW", "MID", "HIGH"))+
  scale_y_continuous(limits= c(0,1), expand=c(0,0))+
  theme_classic()+
  labs(y="")+
  theme(axis.title.x=element_blank())+ 
  theme(axis.text.x = element_text(size=(21)),
        axis.text.y = element_blank())+
  annotate("text",x=1, y=0.25, label= "12 MONTHS", color = "black", size = 10)+
  geom_hline(yintercept = 0.72634, size = 1, color = "red")



pELEV3

pELEV4 <- ggplot(collect4, aes(x=ELEV, y=REMAINper))+
  geom_boxplot()+
  scale_x_discrete(limits=c("LOW", "MID", "HIGH"))+
  scale_y_continuous(limits= c(0,1), expand=c(0,0))+
  theme_classic()+
  labs(y="")+
  theme(axis.title.x=element_blank())+ 
  theme(axis.text.x = element_text(size=(21)),
        axis.text.y = element_blank())+
  annotate("text",x=1, y=0.25, label= "20 MONTHS", color = "black", size = 10)+
  geom_hline(yintercept = 0.587, size = 1, color = "red")



pELEV4

pELEV5 <- ggplot(collect5, aes(x=ELEV, y=REMAINper))+
  geom_boxplot()+
  scale_x_discrete(limits=c("LOW", "MID", "HIGH"))+
  scale_y_continuous(limits= c(0,1), expand=c(0,0))+
  theme_classic()+
  labs(y="")+
  theme(axis.title.x=element_blank())+ 
  theme(axis.text.x = element_text(size=(21)),
        axis.text.y = element_blank())+
  annotate("text",x=1, y=0.25, label= "24 MONTHS", color = "black", size = 10)+
  geom_hline(yintercept = 0.5275, size = 1, color = "red")



pELEV5

pELEVmin <- ggplot(collect5, aes(x=ELEV, y=REMAINper))+
  geom_boxplot()+
  scale_x_discrete(limits=c("LOW", "MID", "HIGH"))+
  scale_y_continuous(limits= c(0,1), expand=c(0,0))+
  theme_minimal()

pELEVmin

pELEV <- ggplot(no.initial, aes(x=ELEV, y=(REMAINper*100)))+
  geom_boxplot()+
  scale_x_discrete(limits=c("LOW", "MID", "HIGH"))+
  scale_y_continuous(limits= c(0,100), expand=c(0,0))+
  facet_grid(.~COLLECT, labeller=month_labeller)+
  theme_bw()+
  labs(y="MASS REMAINING (%)")+
  labs(x="")+
  theme(axis.text.x = element_text(size=(20), colour="black"))+
  theme(axis.title.y= element_text(size=32), 
        axis.text.y = element_text(size=(24), colour="black"),
        strip.text.x= element_text(size = (24), colour="black"))+
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())

pELEV


#######################
#usings means to make composite graphs

#adding some time factors back in after leaving them
#filling in time in years for collection for later analysis
attach(ELEVmeans)
ELEVmeans$time[COLLECT == 0] <-0
ELEVmeans$time[COLLECT == 1] <-0.083333
ELEVmeans$time[COLLECT == 2] <-0.66667
ELEVmeans$time[COLLECT == 3] <-1
ELEVmeans$time[COLLECT == 4] <-01.66667
ELEVmeans$time[COLLECT == 5] <-2
detach(ELEVmeans)

attach(ELEVmeans)
ELEVmeans$month[COLLECT == 0] <-"Initial"
ELEVmeans$month[COLLECT == 1] <-"1 month"
ELEVmeans$month[COLLECT == 2] <-"8 months"
ELEVmeans$month[COLLECT == 3] <-"12 months"
ELEVmeans$month[COLLECT == 4] <-"20 months"
ELEVmeans$month[COLLECT == 5] <-"24 months"
detach(ELEVmeans)

attach(VEGmeans)
VEGmeans$time[COLLECT == 0] <-0
VEGmeans$time[COLLECT == 1] <-0.083333
VEGmeans$time[COLLECT == 2] <-0.66667
VEGmeans$time[COLLECT == 3] <-1
VEGmeans$time[COLLECT == 4] <-01.66667
VEGmeans$time[COLLECT == 5] <-2
detach(VEGmeans)

attach(VEGmeans)
VEGmeans$month[COLLECT == 0] <-"Initial"
VEGmeans$month[COLLECT == 1] <-"1 month"
VEGmeans$month[COLLECT == 2] <-"8 months"
VEGmeans$month[COLLECT == 3] <-"12 months"
VEGmeans$month[COLLECT == 4] <-"20 months"
VEGmeans$month[COLLECT == 5] <-"24 months"
detach(VEGmeans)

ELEVmeans$days <- ELEVmeans$time * 365
VEGmeans$days <- VEGmeans$time * 365

write.csv(VEGmeans, "decomp_veg_means.csv")
write.csv(ELEVmeans, "decomp_elev_means.csv")
######
VEG <- c("single","single", "single","single","single", "single")
COLLECT <- c(0, 1, 2, 3, 4, 5)
mean_per <- c(100, 97.9, 85.1, 78.5, 66.8, 61.65)
sd <- c(NA, NA,NA,NA,NA, NA)
n <- c(NA, NA,NA,NA,NA, NA)
SE <- c(NA, NA,NA,NA,NA, NA)
time <- c(0, 0.083, 0.667, 1, 1.667, 2)
single.pool <- data.frame(VEG, COLLECT, mean_per, sd, n, SE, time )
single.pool$COLLECT <- as.factor(single.pool$COLLECT)
VEGmeans <- merge(VEGmeans, single.pool)

mt3 <- c(100, 96.3, 76.1, 67.9, 56.0, 51.7)

x11()
ggplot(ELEVmeans, aes(x=time, y=(mean_per), group=ELEV, colour=ELEV))+
  geom_line(aes(linetype=ELEV), size=1)+
  geom_point(size=3, fill="white")+
  scale_y_continuous(limits= c(0,1), expand=c(0,0))+
  geom_errorbar(aes(ymin=mean_per-SE, ymax=mean_per+SE), width=.1)+
  #theme_bw()+
  labs(y="Mass Remaining")+
  labs(x="Years")+
  theme(axis.text.x = element_text(size=(16), colour="black"))+
  theme(axis.title.y = element_text(size=16),
        axis.title.x = element_text(size = 16),
        axis.text.y = element_text(size=(16), colour="black"),
        strip.text.x= element_text(size = (16), colour="black"))+
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank())+
  stat_function(fun = function(x)  1  * exp( (k * cdi * x)  ), size = 1, aes(color = "Model (LIDET)"), show.legend = TRUE)+
  theme(legend.position="top")

x11()
ggplot(VEGmeans, aes(x=time, y=(mean_per), group=VEG, colour=VEG))+
  geom_line(aes(linetype=VEG), size=1)+
  geom_point(size=3, fill="white")+
  scale_y_continuous(limits= c(0,1), expand=c(0,0))+
  geom_errorbar(aes(ymin=mean_per-SE, ymax=mean_per+SE), width=.1)+
  #theme_bw()+
  labs(y="MASS REMAINING (%)")+
  labs(x="Years")+
  theme(axis.text.x = element_text(size=(16), colour="black"))+
  theme(axis.title.y= element_text(size=16),
        axis.text.y = element_text(size=(16), colour="black"),
        strip.text.x= element_text(size = (16), colour="black"))+
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank())+
  stat_function(fun = function(x)  1  * exp( (k * cdi * x)  ), size = 1, color = "black")



##############
#interactions

COLLECTmeans <- aggregate(formula= REMAINper~ELEV+VEG+days, data=litter, FUN=mean)

COLLECTmeans$ID <- paste( (substr(COLLECTmeans$ELEV,1,1)), (substr(COLLECTmeans$VEG,1,1)))
cbPalette <- c("#000000", "#999999", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")

p.totals <-ggplot(COLLECTmeans, aes(x=days, y=REMAINper, color = ID))+
  geom_point(size=6, alpha=0.7) +
  scale_colour_manual(values = cbPalette)+
  #   theme(legend.justification=c(1,1), legend.position=c(1,1), 
  #         legend.text=element_text( size=20))+
  theme(axis.title.x= element_text(size=20), 
        axis.text.x = element_text(size=(18)))+
  theme(axis.title.y= element_text(size=20), 
        axis.text.y = element_text(size=(18)))+
  ylab("Mass Loss (%)")+
  xlab("Days")
p.totals +facet_grid(ELEV~VEG) +
  theme(strip.text.x = element_text(size=12),
        strip.text.y = element_text(size=12))


# 
p.CNv <- ggplot(CNmeans, aes(x = COLLECT, y = CN, color = VEG))+
  geom_point(size= 8)

p.CNe <- ggplot(CNmeans, aes(x = COLLECT, y = CN, color = ELEV))+
  geom_point(size= 8)
p.CNe
# p.CN + facet_grid(ELEV~ .)



######################
k.totals <- aggregate(k ~ ELEV + VEG + COLLECT, FUN=mean, data=litter)

k.totals.final <- subset(k.totals, COLLECT ==5)


# Now we want to put decomposition against soil pH to see what that gets us. 
k.5 <- subset(litter, COLLECT == 5)
pH <- read.csv(file='soil_ph.Csv', header=TRUE)

k.pH <- merge(k.5, pH, by=c("PLOT"))

#linear regression and correlation of soil pH. NOT SIGNIFICANT
lm.k.pH <- lm(k ~ pH, data=k.pH)
summary(lm.k.pH)
cor(k.pH$pH, k.pH$k)

p.k.pH <- ggplot(k.pH, aes(x=pH, y=k))+
  geom_point(size=5, shape = 1)+
  theme_classic()+
  xlab("soil pH")+
  ylab("k")+
  theme(axis.title.x= element_text(size=20), 
        axis.text.x = element_text(size=(18)))+
  theme(axis.title.y= element_text(size=20), 
        axis.text.y = element_text(size=(18)))
p.k.pH

###Now we are bringing in some leaves and CN action

leaves5 <- subset(leavesx, COLLECT == 5)
final <- merge(k.pH, leaves5, by = "PLOT")

finalx <- final[, c(1,9,17,20, 23,24,26,27,32)]


#linear regression and correlation of biomass remaining and N NOT SIGNIFICANT
lm.mass.N <- lm(REMAINper.x ~ N, data=final)
summary(lm.mass.N )
cor(final$N, final$REMAINper.x)


p.melillo2 <- ggplot(final, aes( x = N, y = (REMAINper.x * 100)))+
  geom_point(size=5, shape = 1)+
  theme_classic()+
  xlab("N (%)")+
  ylab("Biomass Remaining (%)")+
  theme(axis.title.x= element_text(size=20), 
        axis.text.x = element_text(size=(18)))+
  theme(axis.title.y= element_text(size=20), 
        axis.text.y = element_text(size=(18)))

p.melillo2



p.melillo3 <- ggplot(leavesxx, aes( x = ligN, y = (REMAINper * 100)))+
  geom_point(size=5, shape = 1)+
  theme_classic()+
  xlab("Lignin (%) : N (%)")+
  ylab("Biomass Remaining (%)")+
  theme(axis.title.x= element_text(size=20), 
        axis.text.x = element_text(size=(18)))+
  theme(axis.title.y= element_text(size=20), 
        axis.text.y = element_text(size=(18)))

p.melillo3


