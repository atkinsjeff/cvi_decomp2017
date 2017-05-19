#08_empirical_model.R
# jeff atkins (jwatkins6@vcu.edu)
# Making elevation model versus single-pool and three-pool model

# Litter fall value of 2 Mg Ha yr^-1 based on Adams 2008 Fernow Exp. Forest 


elev <- read.csv("data/decomp_elev_means.csv")
lf <- 2

#areas in hectares of each elevation level; differs from measured area based on rounding
low.area <- 117.09 
mid.area <- 152.64
high.area <- 99.63

watershed.area <- low.area + mid.area + high.area

#importing elevation data from other
# and then using the litter mass data above
elev$litter.mass.per.ha <- elev$mean_per * lf 
elev$litter.mass.per.ha.SE <- elev$SE * lf

# now let's make it carbon
elev$carbon.per.ha <- elev$litter.mass.per.ha * 0.5
elev$carbon.per.ha.SE <- elev$litter.mass.per.ha.SE * 0.5

# # Now let's get total carbon to add up 
# elev$total.c.SE <- 0

elev$area[elev$ELEV == "LOW"] <- low.area
elev$area[elev$ELEV == "MID"] <- mid.area
elev$area[elev$ELEV == "HIGH"] <-  high.area
elev$area[elev$ELEV == "SINGLE-POOL" | elev$ELEV == "THREE-POOL"] <-  watershed.area

elev$total.c <- elev$carbon.per.ha * elev$area

# Let's take out just the empirical data
df <- elev[which(elev$ELEV == "LOW" | elev$ELEV == "MID" | elev$ELEV == "HIGH"), ]

df %>% 
  group_by(COLLECT) %>%
  summarize(total.c = sum(total.c) ) -> elev.model

elev.model <- data.frame(elev.model)
elev.model$model <- "WATERSHED"

decomp.model <- elev[which(elev$ELEV == "SINGLE-POOL" | elev$ELEV == "THREE-POOL"), ]

# colnames(decomp.model)[2] <- "model"
# 
# combo.meal <- bind_rows(decomp.model, elev.model, by = )