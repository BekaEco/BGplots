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
                      labels = c("rainbow trout","terrestrial","littoral","pelagic")) +
  guides(colour = guide_legend(override.aes = list(pch = c(20, 17,3,8), 
                                                   color = c("#f4a582", "#008837","#92c5de","#0571b0")))) +
  theme(legend.position=c(.70,.20))

ggsave()

##The legend is wonky. Can I change it? I will make a simpler plot with only two things to test:
ggplot() +
  geom_point(data = fish, aes(x = d13C_VPDB, y = d15N_air, color = "#f4a582"), size = 2, shape = 20) +
  geom_point(data = ter, aes(x = d13C_VPDB, y = d15N_air, color = "#008837"), size = 2, shape = 17) +
  scale_colour_manual(name = "", 
                      labels = c("fish", "terrestrial"),
                      values = c("#f4a582"="#f4a582", "#008837" = "#008837")) +
  guides(colour = guide_legend(override.aes = list(pch = c(20, 17), color = c("#f4a582", "#008837")))) +
  theme_cowplot()



p1<-ggplot() +
  geom_point(data = fish, aes(x = d13C_VPDB, y = d15N_air, color = "#f4a582"), size = 2, shape = 20) +
  geom_point(data = ter, aes(x = d13C_VPDB, y = d15N_air, color = "#008837"), size = 2, shape = 17) +
  theme_cowplot()

print(p1 + scale_shape_manual(values = c(20,17)))

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

##right now I'm getting frustrated that I cannot plot from separate dataframes. It seems I first need to 
## gather them into a single dataframe.

#So maybe I select just the columns I need and create a new single df?


#base r
par(mai = c(1,1,0.5,.5))
plot(x = fish$d13C_VPDB, y = fish$d15N_air,
     pch = 20,
     col = "#a6611a",
     ylim = c(-12,11),
     xlim = c(-37,-17),
     xlab = expression(paste(delta^{13}, "C (\u2030)")),
     ylab = expression(paste(delta^{15}, "N (\u2030)")))
points(x = ter$d13C_VPDB, y = ter$d15N_air,
       pch = 17,
       col = "#238b45")
points(pel$d13C_VPDB, pel$d15N_air,
       pch = 4,
       col = "#526EFF")
points(x = lit$d13C_VPDB, y = lit$d15N_air,
       pch = 8,
       col = "#66c2a4")
legend(x = -23 , y=-5, 
       legend = c("fish", "terrestrial", "pelagic", "littoral") ,
       pch = c(20,17,4,8),
       col = c("#a6611a", "#238b45", "#526EFF", "#66c2a4"),
       bty = "n")


## test bind_rows

df1 <-data.frame(b = c(1:5), a = c(6:10))
df2 <-data.frame(a = c(11:15), b = c(16:20), c = LETTERS[1:5])
df3 <-data.frame(c= letters[1:3], d = c(6:8))

bind_rows(df1,df2,df3)

binded <-bind_rows(fish,ter, pel)
binded$Identifier.1
binded$d13C_VPDB


##Working on the final version of a basic stable isotope biplot

CNbiplot.all<-ggplot() +
  geom_point(data = fish, aes(x = d13C_VPDB, y = d15N_air, color = "#c487a9"), size = 2, shape = 20, show.legend = TRUE) +
  geom_point(data = ter, aes(x = d13C_VPDB, y = d15N_air, color = "#4d7d53"), size = 2, shape = 17, show.legend = TRUE) +
  geom_point(data = lit, aes(x = d13C_VPDB, y = d15N_air, color = "#92c5de" ), size = 2, shape = 3, show.legend = TRUE) +
  geom_point(data = pel, aes(x = d13C_VPDB, y = d15N_air, color = "#0571b0"), size = 2, shape = 8, show.legend = TRUE) +
  theme_cowplot() +
  labs(x = expression(paste(delta^{13}, "C (\u2030)")),
       y = expression(paste(delta^{15}, "N (\u2030)"))) +
  scale_fill_identity(name = "" , 
                      guide = "legend", 
                      aesthetics = "color", 
                      labels = c("rainbow trout","terrestrial","littoral","pelagic")) +
  guides(colour = guide_legend(override.aes = list(pch = c(20, 17,3,8), 
                                                   color = c("#c487a9", "#4d7d53","#92c5de","#0571b0")))) +
  theme(legend.position=c(.70,.20),
        axis.title.y = element_text(angle = 0))
