
library(tidyverse)
Lake <- read.csv("LakeData.csv")
percents <- 1:100
lakelist <- Lake$Lake
light <- matrix(NA,nrow = 100, ncol = 17)
light[,1] <- percents
lakelist <- Lake$Lake
lakelist[12] #this is Rainbow? YES

lakelist<-lakelist[-12] # Remove Rainbow


for (i in (1:length(lakelist))) {
  light[,(1+i)] <-(log(100)-log(percents))/Lake[i, "kd.2pt"]
}

light.df <-as.data.frame(light)
colnames(light.df) <- c("percent",as.character(lakelist))
light.long <-light.df %>% pivot_longer(-percent, names_to = "Lake", values_to = "depth")

ggplot(data = light.long, aes(x = percent, y = depth, color = Lake )) +
  geom_line() + 
  scale_x_log10(position = "top", breaks = c(1,2,5,10,20,50,100)) + 
  scale_y_reverse(breaks = c(0,5,10,15,20,25)) +
  theme_cowplot()

# Attentuation plot of all lakes with the kd 2p method
plot(x = percents, (log(100)-log(percents))/Lake[which(Lake$Lake == "Tusco Out-In Pot, Small"), "kd.2pt"],
     ylim = rev(range(0:30)),
     log='x',
     pch = 20)
for (i in (2:length(lakelist))) {
  points(x = percents, (log(100)-log(percents))/Lake[i, "kd.2pt"],
         pch = 20)
}


plot(x = percents, (log(100)-log(percents))/Lake[which(Lake$Lake == "Tusco Out-In Pot, Small"), "kd.below1m"],
     ylim = rev(range(0:50)),
     log='x',
     pch = 20,
     xlab = "% surface light",
     ylab = "lake depth (m)")
for (i in (2:length(lakelist))) {
  points(x = percents, (log(100)-log(percents))/Lake[i, "kd.below1m"],
         pch = 20)
}

