
library(tidyverse)
library(gghighlight)
library(ggrepel)
Lake[,c("Lake","onepercent.kd.2pt")]
Lake <- read.csv("LakeData.csv")
percents <- c(1,100)
lakelist <- Lake$Lake
light <- matrix(NA,nrow = 2, ncol = 17)
light[,1] <- percents
lakelist <- Lake$Lake
lakelist[12] #this is Rainbow? YES

lakelist<-lakelist[-12] # Remove Rainbow

## I can change "kd.2pt" to the below 1m test if I want
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

ggplot(data = light.long, aes(x = percent, y = depth)) +
  geom_line(aes(group = Lake), color = "gray") + 
  scale_x_log10(position = "top", breaks = c(1,2,5,10,20,50,100)) + 
  scale_y_reverse(breaks = c(0,5,10,15,20,25)) +
  labs(x = "percent of surface light (%)",
       y = "depth (m)") +
  theme_cowplot()


###INTENDED FINAL PLOT####
ggplot(data = light.long, aes(x = percent, y = depth, color = Lake)) +
  geom_line() +
  scale_x_log10(position = "top", breaks = c(1,2,5,10,20,50,100)) + 
  scale_y_reverse(breaks = c(0,5,10,15,20,25)) +
  labs(x = "percent of surface light (%)",
       y = "depth (m)") +
  gghighlight(max(depth) > 20 | max(depth)< 8, use_direct_label = FALSE) +
  theme_cowplot() +
  theme(legend.position = "none")

#the next move is to take the above plot. Add points at the max depth (x = 0, y = depth)
ggplot(data = light.long, aes(x = percent, y = depth, color = Lake)) +
  geom_line() + 
  geom_point(data = subset(light.long, percent < 30), aes(x = 1, y = depth), size = .5, color = "black") +
  scale_x_log10(position = "top", breaks = c(1,2,5,10,20,50,100)) + 
  scale_y_reverse(breaks = c(0,5,10,15,20,25)) +
  labs(x = "percent of surface light (%)",
       y = "depth (m)") +
  gghighlight(max(depth) > 20 | max(depth)< 8, use_direct_label = FALSE) +
  theme_cowplot() +
  theme(legend.position = "none") +
  geom_label_repel(data = subset(light.long, percent < 30 & depth < 8),
                  aes(label = Lake),
                  nudge_x = 0.4,
                  nudge_y = 1,
                  segment.color = "black",
                  segment.size  = 0.3) +
  geom_label_repel(data = subset(light.long, percent < 30 & depth > 20),
                  aes(label = Lake),
                  nudge_x = 0.4,
                  nudge_y = 4,
                  segment.color = "black",
                  segment.size  = 0.3) 


###PLOT DO OVER WITH POINT AT 2 INSTEAD OF 1####
Lake <- read.csv("LakeData.csv")
percents <- c(1,2,100)
lakelist <- Lake$Lake
light <- matrix(NA,nrow = 3, ncol = 17)
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


lighta.plot<-ggplot(data = light.long, aes(x = percent, y = depth, color = Lake)) +
  geom_line() + 
  scale_x_log10(position = "top", breaks = c(1,10,50,100)) + 
  scale_y_reverse(breaks = c(0,5,10,15,20,25), expand=c(0,0.0)) +
  labs(x = "percent of surface light (%)",
       y = "depth (m)") +
  gghighlight(max(depth) > 20 | max(depth)< 8, use_direct_label = FALSE) +
  theme_cowplot() +
  theme(legend.position = "none") +
  geom_point(data = subset(light.long, percent == 2), 
             aes(x = 2, y = depth), size = 0, color = NA, alpha = 0) +
  geom_label_repel(data = subset(light.long, percent ==2 & depth < 8),
                   aes(label = Lake)) +
  geom_label_repel(data = subset(light.long, percent ==2 & depth > 16),
                   aes(label = Lake),
                   hjust =1)

ggsave(file='figs/lighta_final_Stiling.png', width=7.5, height=5, dpi=600, plot = lighta.plot)


light.plot<-
ggplot(data = light.long, aes(x = percent, y = depth, color = Lake)) +
  geom_line() + 
  scale_x_log10(position = "top", breaks = c(1,10,50,100)) + 
  scale_y_reverse(breaks = c(0,5,10,15,20,25), expand=c(0,0.0)) +
  labs(x = "percent of surface light (%)",
       y = "depth (m)") +
  gghighlight(max(depth) > 20 | max(depth)< 8, use_direct_label = FALSE) +
  theme_cowplot() +
  theme(legend.position = "none") +
  geom_point(data = subset(light.long, percent == 2), 
             aes(x = 2, y = depth), size = 0, color = NA, alpha = 0) +
  geom_label_repel(data = subset(light.long, percent ==2 & depth < 8),
                   aes(label = Lake),
                   hjust = 0,
                   nudge_x = .25,
                   nudge_y = -1,
                   segment.color = "black",
                   segment.size  = 0.2) +
  geom_label_repel(data = subset(light.long, percent ==2 & depth > 16),
                   aes(label = Lake),
                   hjust = .0,
                   nudge_y = -1,
                   nudge_x = 0.5,
                   segment.color = "black",
                   segment.size  = 0.2)
ggsave(file='figs/light_final_Stiling.png', width=5, height=6, dpi=600, plot = light.plot)

##STORAGE BASE PLOT AREA####
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

