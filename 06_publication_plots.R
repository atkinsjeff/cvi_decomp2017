#07_publication_plots
# Jeff Atkins (jwatkins6@vcu.edu)

#PLOTTING FOR PUBLICATION
require(ggplot2)
# The palette with grey:
cbPalette <- c("#999999", "#D55E00", "#009E73","#E69F00", "#56B4E9",  "#F0E442", "#0072B2",  "#CC79A7")
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


veg <- read.csv("decomp_veg_means.csv")

x11()
p.mass.loss <- ggplot(veg, aes(y=mean_per, x=days, color=VEG))+
  #geom_line(aes(linetype=VEG), sie=1)+
  geom_point(size=3, fill="white")+
  geom_line()
p.mass.loss

factor(veg$VEG)
#reorder factorss
veg$VEG<- factor(veg$VEG, levels(veg$VEG)[c(2, 1, 3, 4, 5)])

x11(width = 5, height = 5)
ggplot(veg, aes(x=days, y=mean_per, group=VEG, colour=VEG))+
  geom_line(aes(linetype=VEG), size=1)+
  geom_point(size=3, fill="white")+
  scale_colour_manual(values=cbPalette)+
  scale_y_continuous(limits= c(0,100), expand=c(0,0))+
  geom_errorbar(aes(ymin=(mean_per- (SE * 100)), ymax=(mean_per+ (SE* 100)), width = 10))+  
  theme_bw()+
  labs(y="MASS REMAINING (%)")+
  labs(x="Days")+
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
  theme(legend.justification=c(-0.1,-0.1), legend.position=c(0,0),
        legend.title = element_blank())

elev <- read.csv("decomp_elev_means.csv")

#reorder factor levels
elev$ELEV <- factor(elev$ELEV, levels(elev$ELEV)[c(2, 3, 1, 4, 5)])

x11(width = 5, height = 5)
ggplot(elev, aes(x=days, y=mean_per, group=ELEV, colour=ELEV))+
  geom_line(aes(linetype=ELEV), size=1)+
  geom_point(size=3, fill="white")+
  scale_colour_manual(values=cbPalette)+
  scale_y_continuous(limits= c(0,100), expand=c(0,0))+
  geom_errorbar(aes(ymin=(mean_per- (SE * 100)), ymax=(mean_per+ (SE * 100)), width = 10))+
  theme_bw()+
  labs(y="MASS REMAINING (%)")+
  labs(x="Days")+
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
  theme(legend.justification=c(-0.1,-0.1), legend.position=c(0,0),
        legend.title = element_blank())
