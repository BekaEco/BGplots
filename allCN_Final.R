#====================================================
#Created by Rebekah Stiling on 3/13/2020, stilir@uw.edu
#Data collected by Rebekah Stiling for MS Thesis at UW SAFS. 
#Figure created as part of Beatiful Graphics taught by Branch Winter 2020
#This is a final script for learning to make isotopebiplots in ggplot, something I currently 
# do in base R. This is the final version.
#=====================================================================
library(tidyverse)

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
CN<-allCN %>% filter(Lake != "Rainbow")

ggplot(data = CN, aes(x = d13C_VPDB, y = d15N_air, shape =type)) +
  geom_point(aes(color = type)) +
  scale_color_manual(values = c("#c487a9", "#4d7d53","#92c5de","#0571b0")) +
  facet_wrap(~Lake) +
  labs(x = expression(paste(delta^{13}, "C (\u2030)")),
       y = expression(paste(delta^{15}, "N (\u2030)"))) +
  theme()

ggplot(data = CN, aes(x = d13C_VPDB, y = d15N_air, shape =type)) +
  geom_point(aes(color = type)) +
  scale_color_manual(values = c("#c487a9", "#4d7d53","#92c5de","#0571b0")) +
  facet_wrap(~Lake) +
  labs(x = expression(paste(delta^{13}, "C (\u2030)")),
       y = expression(paste(delta^{15}, "N (\u2030)"))) +
  theme()

+
  guides(colour = guide_legend(override.aes = list(pch = c(20, 17,3,8), 
                                                   color = c("#c487a9", "#4d7d53","#92c5de","#0571b0")))) +
  theme(legend.position=c(.70,.20),
        axis.title.y = element_text(angle = 0))

