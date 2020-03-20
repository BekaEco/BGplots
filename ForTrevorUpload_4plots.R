##ACTUAL FINAL FOR TREVOR

###Picking up where I left off after submitting these plots to a power point for comments####

# Exact script for Porzio plot
library("tidyverse")
library("stringr")
library("cowplot")

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


#PLOT A####
pA.fin2<-
  ggplot(data=kit.rank, aes(replicate, y = reorder(algae, rank), color = color, size = ifelse(coverage==0, NA, coverage))) +
  geom_point() +
  theme_minimal() + 
  labs(x = "quadrat", 
       y = "") +
  scale_y_discrete(position = "right") +
  scale_color_manual(values=c('#4fa59b', '#e6a836', '#b05a64'),
                     name = "",
                     labels= c("Chlorophyta", "Ochrophyta", "Rhodophyta")) +
  scale_size_continuous(name = "% coverage") +
  theme(plot.margin = margin(t=0,r = 0,b =0,l = -4),
        legend.position = "left",
        # legend.box.margin = margin(t=0,r = -10,b =0,l = 0),
        # legend.justification=c(1, 0), 
        legend.key.width=unit(-1, "lines"), 
        # legend.key.height=unit(1, "lines"), 
        legend.box.spacing = unit(0, "lines"),
        legend.text = element_text(size = 8),
        legend.title = element_text(size =10),
        axis.text.y = element_blank(),
        axis.ticks = element_line(color = "gray"),
        axis.line = element_line(color = "gray"),
        axis.text.x = element_text(size = 8),
        axis.title.x = element_text(size = 10)) +
  annotate("rect", xmin = 0.5, xmax = 9.5, ymin = 0, ymax = 27, alpha = .2, fill = "#78c679") +
  annotate("rect", xmin = 9.5, xmax = 18.5, ymin = 0, ymax = 27, alpha = .2, fill = "#ffffcc") +
  annotate("rect", xmin = 18.5, xmax = 27.5, ymin = 0, ymax = 27, alpha = .2, fill = "#e6a836") +
  geom_point() + #by putting this on again, it put the points on top of the shading. But, I had to have it earlier too.
  guides(color = guide_legend(override.aes = list(size=4))) +
  #coord_cartesian(clip = "off") + #otherwise the pH values are cut off
  annotate(geom = "text", x = 1.5, y= 26.5, label = "pH =", size = 3) +
  annotate(geom = "text", x = 5, y= 26.5, label = "8.1", size = 3) +
  annotate(geom = "text", x = 14, y= 26.5, label = "7.8", size = 3) +
  annotate(geom = "text", x = 23, y= 26.5, label = "6.7", size = 3)

pA.fin2

 
#PLOT B####
pB.fin2 <-ggplot(cov.diff, aes(y= reorder(algae, dif), x = dif, fill = color))+
  geom_col() +
  theme_minimal() +
  xlab("overall change (mean % coverage)") +
  ylab("") +
  annotate("rect", xmin = 0.5, xmax = 9.5, ymin = 0, ymax = 27, alpha = 0, fill = "#78c679") +
  scale_fill_manual(values=c('#4fa59b', '#e6a836', '#b05a64'),
                    name = "",
                    labels= c("Chlorophyta", "Ochrophyta", "Rhodophyta")) +
  theme(legend.position = "none",
        axis.text.y = element_text(hjust=.5,face = "italic", size =8),
        axis.text.x = element_text(size = 8),
        axis.title.x = element_text(size = 10),
        plot.margin = margin(t=0,r = 0,b =0,l = -23),
        axis.ticks = element_line(color = "gray"),
        axis.line = element_line(color = "gray"))

Porzio.final2 <-plot_grid(pA.fin2,pB.fin2, rel_widths = c(3,2.25), align = "h", label_x = c(.1,.3),labels = c("A","B"))

#creates .png file from most recent ggplot object at 600 dots per inch (high) res (was 12 width)
ggsave(file='figs/Porzio_FOUR_final_Stiling.png', width=9.5, height=5, dpi=600, plot = Porzio.final2)
