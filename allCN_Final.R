#Created by Rebekah Stiling on 3/13/2020, stilir@uw.edu ####
#Data collected by Rebekah Stiling for MS Thesis at UW SAFS. 
#Figure created as part of Beatiful Graphics taught by Branch Winter 2020
#This is a final script for learning to make isotopebiplots in ggplot, something I currently 
# do in base R. This is the final version.
#===

library(tidyverse)
library(lemon)
library(cowplot)

#In order to use ggplot, I need one dataframe
fish <- read_csv("fishCN.csv")
ter <- read_csv("terCN.csv")
lit <- read_csv("litCN.csv")
pel <- read_csv("pelagCN.csv")

#before putting all of these together, I need a category for the sample type
fish$type <-"fish"
ter$type <-"ter"
lit$type <- "lit"
pel$type <- "pel"

#I can bind the pairs of similar groups 
ft.num <-bind_rows(fish,ter) #"Comment" is numeric and is the mass in mg
lp.com <-bind_rows(lit,pel) #"Comment" is charachter and is a fraction or written description.

#I need to change one of them. I change Comment to filtersize
colnames(lp.com)[6]<-"filtersize"

allCN <-bind_rows(ft.num,lp.com)

#Oops, rainbow is still in there. Remove it.
CN<-allCN %>% filter(Lake != "Rainbow")


##16 plots!####
## This is a plot of each data type with its original value, one for each lake.
ggplot(data = CN, aes(x = d13C_VPDB, y = d15N_air, shape =type)) +
  geom_point(aes(color = type)) +
  scale_color_manual(values = c("#c487a9","#92c5de","#0571b0", "#4d7d53")) +
  scale_shape_manual(values = c(20,3,8,17)) +
  facet_rep_wrap(~ Lake,repeat.tick.labels = FALSE) + ##This is instead of facet_wrap() and it is from {{lemon}}
  labs(x = expression(paste(delta^{13}, "C (\u2030)")),
       y = expression(paste(delta^{15}, "N (\u2030)"))) +
  theme(strip.background = element_rect(fill=NA, color=NA), #this removes the background from the lake names
        axis.line.x = element_line(color = "black"), #I need this to place the black lines on the x axis
        axis.line.y = element_line(color="black"), #I need this to place the black lines on the y axis
        panel.background = element_rect(fill=NA, color=NA),
        panel.grid.major = element_blank(), #remove grid lines
        panel.grid.minor = element_blank(), #remove more grid lines
        panel.border = element_blank(),
        legend.key = element_rect(fill=NA, color=NA),
        legend.title = element_blank()) +
  scale_x_continuous(expand = c(0, 0)) + 
  scale_y_continuous(expand = c(0, 0))

#I resolved the issue with tickmarks using the package {{lemon}}
#however there is still the issue with calculating things. Overall, I think that I will need to 

##One plot, all lakes combined ####
ggplot(data = CN, aes(x = d13C_VPDB, y = d15N_air, shape =type)) +
  geom_point(aes(color = type)) +
  scale_color_manual(values = c("#c487a9","#92c5de","#0571b0", "#4d7d53")) +
  scale_shape_manual(values = c(20,3,8,17)) +
  labs(x = expression(paste(delta^{13}, "C (\u2030)")),
       y = expression(paste(delta^{15}, "N (\u2030)"))) +
  theme(strip.background = element_rect(fill=NA, color=NA), #this removes the background from the lake names
        axis.line.x = element_line(color = "black"), #I need this to place the black lines on the x axis
        axis.line.y = element_line(color="black"), #I need this to place the black lines on the y axis
        panel.background = element_rect(fill=NA, color=NA),
        panel.grid.major = element_blank(), #remove grid lines
        panel.grid.minor = element_blank(), #remove more grid lines
        panel.border = element_blank(),
        legend.key = element_rect(fill=NA, color=NA),
        legend.title = element_blank()) +
  scale_x_continuous(expand = c(0, 0)) + 
  scale_y_continuous(expand = c(0, 0))


##one plot, one lake, mean of data ####
#THIS IS WHAT I HANDED IN Winter 2020####

CN_means <-CN  %>% group_by(Lake, type) %>% summarise(d13Cmean = mean(d13C_VPDB),
                                                    d15Nmean = mean(d15N_air),
                                                    d13Csd = sd(d13C_VPDB),
                                                    d15Nsd = sd(d15N_air)) %>% filter(type != "fish")
CN_means_K2 <- CN_means %>% filter(Lake == "Kulla Kulla")


CN_fish_K2 <- CN %>% filter(type == "fish", Lake == "Kulla Kulla")

Kulla<-
  ggplot(CN_means_K2, aes(x = d13Cmean, y = d15Nmean, color = type)) +
  geom_pointrange(aes(ymin = d15Nmean - 2*d15Nsd, ymax = d15Nmean + 2*d15Nsd), color = "gray", size = .75) +
  geom_pointrange(aes(ymin = d15Nmean - d15Nsd, ymax = d15Nmean + d15Nsd), size =1.25) +
  geom_pointrange(aes(xmin = d13Cmean - 2*d13Csd, xmax = d13Cmean + 2*d13Csd),color = "gray", size = .75) +
  geom_pointrange(aes(xmin = d13Cmean - d13Csd, xmax = d13Cmean + d13Csd), size = 1.25) +
  geom_point(size = 5, shape = 21, fill = "white") +
  scale_color_manual(values = c("#BC8F8F","#92c5de","#0571b0", "#4d7d53"),
                     name = "",
                     labels= c("rainbow trout", "littoral periphyton", "pelagic seston", "terrestrial leaves")) +
  geom_point(data = CN_fish_K2, aes(x = d13C_VPDB-0.4, y = d15N_air-6.4, size = Length_mm),alpha = .9) +
  theme_cowplot() +
  labs(x = expression(paste(delta^{13}, "C (\u2030)")),
       y = expression(paste(delta^{15}, "N (\u2030)"))) +
  guides(size = guide_legend(title = "trout length (mm)",
                             override.aes = list(color="#BC8F8F")),
         color = guide_legend(override.aes = list(size=.5, shape = 19)))+
  theme(legend.position=c(.68,.28)) +
  scale_x_continuous(breaks = c(-30, -26, -22)) + 
  scale_y_continuous(breaks = c(-8, -4, 0, 4))

ggsave(file='figs/Kulla_final_Stiling.png', width=6, height=6, dpi=600, plot = Kulla)

