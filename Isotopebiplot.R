##Plots
library(tidyverse)
library(cowplot)

Bunn <- read_csv("Bunn.csv")
fish <- read_csv("fishCN.csv")
ter <- read_csv("terCN.csv")
lit <- read_csv("litCN.csv")
pel <- read_csv("pelagCN.csv")

#fish only, basic plot
ggplot(data = fish, aes(x = d13C_VPDB, y = d15N_air)) +
  geom_point() +
  theme_minimal() +
  labs(x = expression(paste(delta^{13}, "C (\u2030)")),
       y = expression(paste(delta^{15}, "N (\u2030)")))


#learning to layer several sets of data onto one plot.
shape<-c(20,17,3,8)
ggplot() +
  geom_point(data = fish, aes(x = d13C_VPDB, y = d15N_air, color = "#f4a582"), size = 2, shape = 20, show.legend = TRUE) +
  geom_point(data = ter, aes(x = d13C_VPDB, y = d15N_air, color = "#008837"), size = 2, shape = 17, show.legend = TRUE) +
  geom_point(data = lit, aes(x = d13C_VPDB, y = d15N_air, color = "#92c5de" ), size = 2, shape = 3, show.legend = TRUE) +
  geom_point(data = pel, aes(x = d13C_VPDB, y = d15N_air, color = "#0571b0"), size = 2, shape = 8, show.legend = TRUE) +
  theme_cowplot() +
  labs(x = expression(paste(delta^{13}, "C (\u2030)")),
       y = expression(paste(delta^{15}, "N (\u2030)"))) +
  scale_fill_identity(name = "" , 
                      guide = "legend", 
                      aesthetics = "color", 
                      labels = c("terrestrial","pelagic","littoral","rainbow trout")) +
  theme(legend.position = c(-23,0)) 


ggplot() +
  geom_point(data = fish, aes(x = d13C_VPDB, y = d15N_air, color = "#f4a582"), size = 2, shape = 20) +
  geom_point(data = ter, aes(x = d13C_VPDB, y = d15N_air, color = "#008837"), size = 2, shape = 17) +
  scale_colour_manual(name = "", 
                      labels = c("fish", "terrestrial"),
                      values = c("#f4a582"="#f4a582", "#008837" = "#008837")) +
  scale_shape_manual(name = "", 
                     labels = c("fish", "terrestrial"),
                     values = c(20,17)) +
  theme_cowplot()

scale_colour_manual(name = "Treatment & State",
                    labels = c("Control, Non-F", "Control, Flwr", "Exclosure, Non-F", "Exclosure, Flwr"),
                    values = c("blue", "red", "blue", "red")) +   
scale_shape_manual(name = "Treatment & State",
                     labels = c("Control, Non-F", "Control, Flwr", "Exclosure, Non-F", "Exclosure, Flwr"),
                     values = c(19, 19, 17, 17))



ggplot() +
  geom_point(data = fish, aes(x = d13C_VPDB, y = d15N_air, color = "#f4a582"), size = 2, shape = 20) +
  geom_point(data = ter, aes(x = d13C_VPDB, y = d15N_air, color = "#008837"), size = 2, shape = 17) +
  theme_cowplot()

print(p1 + scale_shape_manual(values = c(20,17)))


ggplot() +
  geom_point(data = fish, aes(x = d13C_VPDB, y = d15N_air, color = "#f4a582"), size = 2, shape = 20)

##right now I'm getting frustrated that I cannot plot from separate dataframes. It seems I first need to 
## gather them into a single dataframe.

#So I gather my data?
##

pALLCN<-ggplot() +
  geom_point(data = fish, aes(x = d13C_VPDB, y = d15N_air, color = "#f4a582"), size = 2, shape = 20) +
  geom_point(data = ter, aes(x = d13C_VPDB, y = d15N_air, color = "#008837"), size = 2, shape = 17) +
  geom_point(data = lit, aes(x = d13C_VPDB, y = d15N_air, color = "#92c5de" ), size = 2, shape = 3) +
  geom_point(data = pel, aes(x = d13C_VPDB, y = d15N_air, color = "#0571b0"), size = 2, shape = 8) +
  theme_cowplot() +
  scale_fill_identity(name = "" , 
                      guide = "legend", 
                      aesthetics = "color", 
                      labels = c("terrestrial","pelagic","littoral","rainbow trout")) +
  labs(x = expression(paste(delta^{13}, "C (\u2030)")),
       y = expression(paste(delta^{15}, "N (\u2030)")))




fish.CN <-fish %>% select(d13C_VPDB,d15N_air, Lake, Identifier.1)
fish.CN$cat<-"fish"
ter.CN <- ter %>% select(d13C_VPDB,d15N_air, Lake, Identifier.1)
ter.CN$cat <- "ter"
lit.CN <-lit %>% select(d13C_VPDB,d15N_air, Lake, Identifier.1)
