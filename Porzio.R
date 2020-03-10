library("tidyverse")
library("stringr")
library("cowplot")
library("ggrepel")

# Read in and organize the data ####

kite_ugly<-read_csv("Porzio et al 2011 kite diagrams.csv", skip = 1)
head(kite_ugly)
kite_ugly<-kite_ugly[,1:28] #get rid of empty columns at the end
kite_ugly<-kite_ugly[1:25,] #only keep the first set of numbers, I don't know what the second one is
colnames(kite_ugly)[1] <- "MicroAlgae" #give the microalgae column a meaningful name
kite <- kite_ugly %>% pivot_longer(-MicroAlgae, names_to = "replicate", values_to = "coverage")

#pull the color out of the name
kite <-kite %>% separate(col = MicroAlgae , into = c("algae", "color"), 
                         sep = "[(]", 
                         remove = FALSE, 
                         convert=FALSE)
#get rid of the ")"
kite$color <-gsub( "[)]", "", kite$color)

# Now I want to add info to the df about the zone and replicates (quadrats) and the pH. 
# The paper makes it clear how they organized and nested their study, but 
# all those disinctions are lost in the xlsx spreadsheet

# I need two empty columns
kite$zone <- NA
kite$rep <- NA

for (i in 1:length(kite$replicate)) {
  temp_type <-strsplit(x = kite$replicate[i],split = "") ## This splits the string of two letters into a list of 2 components
  kite[i,c("zone","rep")] <- temp_type[[1]] ##This puts the two components into their own column
}

#I imagine there is a way to do this all at once? one by one for now.
kite$sector<-kite$zone #creat sector column
kite$sector <-gsub(x = kite$sector, pattern = "A",replacement = "1")
kite$sector <-gsub(x = kite$sector, pattern = "B",replacement = "1")
kite$sector <-gsub(x = kite$sector, pattern = "C",replacement = "1")
kite$sector <-gsub(x = kite$sector, pattern = "D",replacement = "2")
kite$sector <-gsub(x = kite$sector, pattern = "E",replacement = "2")
kite$sector <-gsub(x = kite$sector, pattern = "F",replacement = "2")
kite$sector <-gsub(x = kite$sector, pattern = "G",replacement = "3")
kite$sector <-gsub(x = kite$sector, pattern = "H",replacement = "3")
kite$sector <-gsub(x = kite$sector, pattern = "I",replacement = "3")

#create pH column for the sectors, from text in fig2 of the paper
kite$pH<-kite$sector
kite$pH <- gsub(x = kite$pH, pattern = "1",replacement = "8.1")
kite$pH <- gsub(x = kite$pH, pattern = "2",replacement = "7.8")
kite$pH <- gsub(x = kite$pH, pattern = "3",replacement = "6.7")

#I'm just going to rename my data table, just incase I need to go back, plus,
# this is shorter
kit <- kite 

#I want a few of these variables to be factors
kit$sector <- as.factor(kit$sector)
kit$replicate <- as.factor(kit$replicate)
kit$color <-as.factor(kit$color)

#begin plotting ####
ggplot(data=kit, aes(pH, coverage)) +
  geom_point()

#this first plot demonstrates to me that there are wierd things with the data.
#No value should be higher than 100, right? Hmmm
#maybe higher than 100 just means that they filled more than one tray with material?


# So I went back to the table and determined that all three points in sector 1 (Jania rubens)
# are greater than 100% that is different. It is possible the data was converted by 10
# accidentaly, or typos? Or was there really that much more material than the other species?

#the other issue is that the S2-pH7.8 outlier was 158% That is from F1 (Flabellia peiolatica) 
# I'm confused. Maybe it was supposed to be 58% - That is what the graph looks like (it goes to "50"
# in the Kite plot. Regardless, I will continue with this data as is.

#I want to plot it in a way that makes it clear what is in the data set
ggplot(data=kit, aes(zone, coverage)) +
  geom_point()

ggplot(data=kit, aes(replicate, coverage,color)) +
  geom_point() +
  theme_minimal() +
  labs(x = "quadrat",
       y= "coverage (%)")

total.cov <- kit %>% group_by(replicate) %>% summarize(traycov= sum(coverage))
#this should add up to 100% or less for each quadrat. This makes no sense, I must not
# understand how to measure microalgae. Maybe the put each species of algae one at a time
# in their horizontal tray and just measure percent coverage of the tray.

#moving on...
ggplot(data=kit, aes(replicate, coverage, color)) +
  geom_point() +
  theme_minimal() +
  labs(x = "quadrat",
       y= "coverage (%)")

#This paper is interested in diversity and abundance
#number of species
species.num <- kit %>% group_by(replicate) %>% tally(coverage != 0, sort = TRUE)

# what if I plot species on y, site on x, and presence/absence in dot. Dot is the size of
# coverage amount. Color is type of algae.

ggplot(data=kit, aes(replicate, y = reorder(algae, coverage), color = color, size = ifelse(coverage==0, NA, coverage))) +
  geom_point() +
  theme_minimal()

#get the mean coverage for each algae by sector
sect.cov <- kit %>% group_by(sector, algae) %>% summarise(meancov = mean(coverage))

ggplot(data=sect.cov, aes(sector, y = reorder(algae, meancov), size = ifelse(meancov==0, NA, meancov))) +
  geom_point() +
  theme_minimal()

#get the mean coverage for each algae by sector
pH.cov <-kit %>% group_by(pH, algae) %>% summarise(meancov = mean(coverage))

#mean coverage by pH
ggplot(data=pH.cov, aes(pH, y = reorder(algae, meancov), size = ifelse(meancov==0, NA, meancov))) +
  geom_point() +
  theme_minimal()

#3/8/2020 leaving off: I think I need to make a multi panel plot? I can also consider trying
# to make an actual violin plot, or I can just plot the data in a way that communicates a story.
# things to think about.

#3/9/2020 I want to make a matrix or something from the highest to lowest pH
#create what is essentially a heat map? Bubble chart?

#a little more exploring, what really is the difference between the treatments?
pH.8<-kit %>% filter(pH == 8.1)
cov.8 <-pH.8 %>% group_by(algae) %>% summarise(pH8cov = mean(coverage))

pH.6<-kit %>% filter(pH == 6.7)
cov.6 <-pH.6 %>% group_by(algae, color) %>% summarise(pH6cov = mean(coverage))

#I've return to this section to add a rank to my df. I like a few plots, but I want to
# orden them differently
cov.diff <- full_join(cov.6,cov.8, by="algae")
cov.diff$dif <- cov.diff$pH6cov - cov.diff$pH8cov

ranked <- arrange(cov.diff, dif)
ranked$rank <- 1:nrow(ranked)
ranked <- ranked[,c("algae","rank")]

#Now I want to assign this rank value to the large table of microalgae species
kit.rank <- left_join(kit, ranked, by = "algae")

ggplot(cov.diff, aes(x= reorder(algae, dif), y = dif, fill = color))+
  geom_col() +
  coord_flip() +
  theme_minimal() +
  ylab("mean change in coverage\nfrom pH 8.1 to 6.7 ") +
  xlab("Microalgae")
  
#I've decided on three plots ####
#A. All the coverage values, to indicate it doesn't make sense to me

pA <- ggplot(data=kit, aes(replicate, coverage, color)) +
  geom_point() +
  theme_minimal() +
  labs(x = "quadrat",
       y= "coverage (%)")

#B This is similar to the kite plot, reorder and improve
pB <-ggplot(data=kit, aes(replicate, y = reorder(algae, coverage), color = color, size = ifelse(coverage==0, NA, coverage))) +
  geom_point() +
  theme_minimal()

#C. This is the change map
pC <-ggplot(cov.diff, aes(x= reorder(algae, dif), y = dif, fill = color))+
  geom_col() +
  coord_flip() +
  theme_minimal() +
  ylab("mean change in coverage\nfrom pH 8.1 to 6.7 ") +
  xlab("Microalgae")

#This is my confirmed version of a rough-rought draft. 
# Leave this here.
# Now I improve each plot! ####
top_row <- plot_grid(pB,pC, labels = c("A","B"), label_size = 12)
plot_grid(top_row, pA, labels = c("","C"), ncol = 1)

# Returning to plot 1, I need to work on overall presentation and lables. I also 
# want to figure out how to reorder the algae into a ranked chart that 
# mimics chart B.- #going back up to order the df

#New A - each quadrat and coverage, ranked by overall change
pA<-ggplot(data=kit.rank, aes(replicate, y = reorder(algae, rank), color = color, size = ifelse(coverage==0, NA, coverage))) +
  geom_point() +
  theme_minimal() + 
  labs(x = "Quadrat ID", 
       y = "Microalgae species") +
  scale_color_manual(values=c('#4fa59b', '#cc7722', '#b05a64'),
                     name = "",
                     labels= c("Chlorophyta", "Ochrophyta", "Rhodophyta")) +
  scale_size_continuous(name = "coverage (%)")

#New B - change from ph level
pB<-ggplot(cov.diff, aes(x= reorder(algae, dif), y = dif, fill = color))+
  geom_col() +
  coord_flip() +
  theme_minimal() +
  ylab("mean change in coverage\nfrom pH 8.1 to 6.7 ") +
  xlab("Microalgae") +
  scale_fill_manual(values=c('#4fa59b', '#cc7722', '#b05a64'),
                     name = "",
                     labels= c("Chlorophyta", "Ochrophyta", "Rhodophyta"))

#New B.2.0 - change from ph level No labels
pB<-ggplot(cov.diff, aes(x= reorder(algae, dif), y = dif, fill = color))+
  geom_col() +
  coord_flip() +
  theme_minimal() +
  ylab("mean change in coverage\nfrom pH 8.1 to 6.7 ") +
  xlab("") +
  scale_fill_manual(values=c('#4fa59b', '#cc7722', '#b05a64'),
                    name = "",
                    labels= c("Chlorophyta", "Ochrophyta", "Rhodophyta")) +
  theme(legend.position = "none",
        axis.text.y = element_blank())
 
#New C = outlier % coverage values
ggplot(data=kit, aes(x = replicate, y = ifelse(coverage==0, NA, coverage), fill = color)) +
  geom_point(shape = 21, size = 5, alpha = .5) +
  scale_fill_manual(values=c('#4fa59b', '#cc7722', '#b05a64'),
                    name = "",
                    labels= c("Chlorophyta", "Ochrophyta", "Rhodophyta")) +
  theme_minimal() +
  labs(x = "quadrat",
       y= "coverage (%)")

top<-ggplot(data=kit, aes(x = replicate, y = ifelse(coverage==0, NA, coverage), color = color)) +
  geom_point(shape = 16, size = 5, alpha = .5) +
  scale_color_manual(values=c('#4fa59b', '#cc7722', '#b05a64'),
                     name = "",
                     labels= c("", "", "")) +
  scale_y_continuous(limits = c(26,160),
                     breaks = c(50,100, 150)) +
  geom_hline(yintercept = 100, linetype="dashed", color = "gray50", size=1) +
  theme_minimal() +
  labs(x = "",
       y= "")+
  theme(axis.text.x = element_blank(),
        legend.text = element_blank(),
        legend.key = element_blank()) +
  geom_text(data=subset(kit, coverage >= 100),
            aes(replicate,coverage,label=algae))

bot<-ggplot(data=kit, aes(x = replicate, y = ifelse(coverage==0, NA, coverage), color = color)) +
  geom_point(shape = 16, size = 5, alpha = .5) +
  scale_color_manual(values=c('#4fa59b', '#cc7722', '#b05a64'),
                     name = "",
                     labels= c("Chlorophyta", "Ochrophyta", "Rhodophyta")) +
  scale_y_continuous(limits = c(0,25)) +
  theme_minimal() +
  labs(x = "quadrat",
       y= "coverage (%)") 

###Draft for class upload ####
pC<-plot_grid(top, bot, labels = "C",ncol = 1, align = "v")
# this is frustrating. If I remove the legend from the top, then align "v" doesn't work.
# I cannot figure out how to have the legend go away and have the two plots remained stacked.
# blargh!!!!

#This is my confirmed version of a rough draft. Now I improve each plot! I'm totally out of time
# but this is enough to hand in I think...
top_row <- plot_grid(pA,pB, labels = c("A","B"), rel_widths = c(3,1), label_size = 12)
draft.Porzio<-plot_grid(top_row, pC, ncol = 1)

ggsave(filename = "figs\\draft.Porzio.pdf", width = 8, height = 5, units = "in")


#### Post upload - plot improvement section. ####
# A + B
# I want the species to be listed on otherside of the y axis so it is closer to the B plots
# I also want to shade the three different acid conditions

pA.2<-ggplot(data=kit.rank, aes(replicate, y = reorder(algae, rank), color = color, size = ifelse(coverage==0, NA, coverage))) +
  geom_point() +
  theme_minimal() + 
  labs(x = "quadrat ID", 
       y = "") +
  scale_y_discrete(position = "right") +
  scale_color_manual(values=c('#4fa59b', '#cc7722', '#b05a64'),
                     name = "",
                     labels= c("Chlorophyta", "Ochrophyta", "Rhodophyta")) +
  scale_size_continuous(name = "coverage (%)") +
  theme(legend.position = "left") +
  annotate("rect", xmin = 0.5, xmax = 9.5, ymin = 0, ymax = 26, alpha = .2, fill = "#78c679") +
  annotate("rect", xmin = 9.5, xmax = 18.5, ymin = 0, ymax = 26, alpha = .2, fill = "#c2e699") +
  annotate("rect", xmin = 18.5, xmax = 27.5, ymin = 0, ymax = 26, alpha = .2, fill = "#ffffcc") +
  geom_point() + #by putting this on again, it makes them on top of the shading. 
  guides(color = guide_legend(override.aes = list(size=4))) +
  coord_cartesian(clip = "off") +
  annotate(geom = "text", x = 1.5, y= 26.5, label = "pH =", size = 4) +
  annotate(geom = "text", x = 5, y= 26.5, label = "8.1", size = 4) +
  annotate(geom = "text", x = 14, y= 26.5, label = "7.8", size = 4) +
  annotate(geom = "text", x = 23, y= 26.5, label = "6.7", size = 4)

pB.2<-ggplot(cov.diff, aes(y= reorder(algae, dif), x = dif, fill = color))+
  geom_col() +
  theme_minimal() +
  xlab("mean overall change (coverage amount)") +
  ylab("") +
  scale_fill_manual(values=c('#4fa59b', '#cc7722', '#b05a64'),
                    name = "",
                    labels= c("Chlorophyta", "Ochrophyta", "Rhodophyta")) +
  theme(legend.position = "none",
        axis.text.y = element_blank(),
        plot.margin = margin(t=15,r = 0,b =10,l = -30)) 

##NewC

theme_set(theme_minimal())
top.2<-ggplot(data=kit, aes(x = replicate, y = ifelse(coverage==0, NA, coverage), color = color)) +
  geom_point(shape = 16, size = 5, alpha = .5) +
  scale_color_manual(values=c('#4fa59b', '#cc7722', '#b05a64'),
                     name = "",
                     labels= c("", "", "")) +
  scale_y_continuous(limits = c(26,165),
                     breaks = c(50,100, 150)) +
  geom_hline(yintercept = 100, linetype="dashed", color = "gray50", size=1) +
  labs(x = "",
       y= "")+
  theme(axis.text.x = element_blank(),
        legend.text = element_blank(),
        legend.key = element_blank(),
        legend.title = element_blank(),
        legend.position = "left") +
  coord_cartesian(clip = "off") +
  geom_text_repel(data=subset(kit, coverage >= 75),
                  aes(replicate,coverage,label=algae))

bot.2<-ggplot(data=kit, aes(x = replicate, y = ifelse(coverage==0, NA, coverage), color = color)) +
  geom_point(shape = 16, size = 5, alpha = .5) +
  scale_color_manual(values=c('#4fa59b', '#cc7722', '#b05a64'),
                     name = "",
                     labels= c("Chlorophyta", "Ochrophyta", "Rhodophyta")) +
  scale_y_continuous(limits = c(0,25)) +
  labs(x = "quadrat ID",
       y= "coverage (%)") +
  theme(legend.position = "left")


# it turns out that align does not work when only one of the plots has a legend
# plot_grid(top.2, bot.2, labels = "C",ncol = 1, align = "v") 

# by building two little stacks, first the legends, then the plots, 
# then those go together to make the mini-four plot panel.
pC.2<-plot_grid(
  plot_grid(
    ggplot()
    , get_legend(bot.2)
    , ncol = 1)
  , plot_grid(
    top.2 + theme(legend.position = "none")
    , bot.2 + theme(legend.position = "none")
    , ncol = 1
    , align = "v")
  , rel_widths = c(1,7)
)



##Putting all three of these together
  
top_row <- plot_grid(pA.2,pB.2, labels = c("A","B"), 
                     rel_widths = c(3,1), label_size = 12)
draft2.Porzio<-plot_grid(top_row, pC.2, labels =c("","c"), ncol = 1, label_size = 12)

# Things that I am struggling with: ####
# 1. I can't get the lines in plot A and B to line up exactly.align = "h" doesn't help.
# 2. When I slide plot B closer to A, the "B" doesnt move,
# so it isn't in the corner of the plot
# 3. The A, B, and C are slightly different sizes, I could do trial and error, but 
# I would rather have a specific method to ensure they are the same.
# 4. I have to decide whether the horiz and vert lines are valuable
# 5. I also can't get the three plot boundaries to line up exactly with eachother.
# 6. I think there are places where I could manipulate this, but I'm not sure right now.
# 7. Do I need a legend for the pH shading? I think it is OK to communicate that exclusively
# in the 
# I could also add information about species richness changes? That was the other
# main goal of this project. I could sum the species in the quadrants, or I could
# look at each sector. There was a decrease in richness for sure.


