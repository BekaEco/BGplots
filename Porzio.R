library("tidyverse")
library("stringr")
library("cowplot")


kite_ugly <-read.csv("Porzio_middleout.csv", skip = 1)
head(kite_ugly)
kite_ugly<-kite_ugly[,1:28] #get rid of empty columns at the end
kite_ugly<-kite_ugly[1:25,] #only keep the first set of numbers, I don't know what the second one is

kite <- kite_ugly %>% pivot_longer(-MicroAlgae, names_to = "replicate", values_to = "coverage")

kite<-kite %>% mutate_if(is.factor, as.character) #I need character strings to use separate

#pull the color out of the name
kite <-kite %>% separate(col = MicroAlgae , into = c("algae", "color"), sep = "[(]", remove = FALSE, convert=FALSE)
#get rid of the ")"
kite$color <-gsub( "[)]", "", kite$color)

#Now I want to add info to the df about the zone and rep and the pH

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

#create pH column for the sectors, from text in fig2
kite$pH<-kite$sector
kite$pH <- gsub(x = kite$pH, pattern = "1",replacement = "8.1")
kite$pH <- gsub(x = kite$pH, pattern = "2",replacement = "7.8")
kite$pH <- gsub(x = kite$pH, pattern = "3",replacement = "6.7")

#I'm just going to get rid of the stuff at the bottom, I don't know what they are
kit <- kite %>% filter(MicroAlgae != "Nothing")

#I want sector to go back to being a factor
kit$sector <- as.factor(kit$sector)
kit$replicate <- as.factor(kit$replicate)
kit$color <-as.factor(kit$color)

#begin plotting
ggplot(data=kit, aes(pH, coverage)) +
  geom_point()

#this first plot demonstrates to me that there are problems with the data.
#No value should be higher than 100

#So I went back to the table and determined that all three points in sector 1 (Jania rubens)
# are greater than 100% that is a major problem. It is possible the data was converted by 10
# accidentaly, or typos.

#the other issue is that the S2-pH7.8 outlier was 158% That is from F1 (Flabellia peiolatica) 
# Irrational. Maybe it was supposed to be 58% - That is what the graph looks like.


#So now I have to decide if I keep the irrational stuff and plot it anyway, or
# drop it. I think I want to keep it and plot it in a way that makes it obvious there was 
# a problem!! 
ggplot(data=kit, aes(zone, coverage)) +
  geom_point()

ggplot(data=kit, aes(replicate, coverage,color)) +
  geom_point() +
  theme_minimal() +
  labs(x = "quadrat",
       y= "coverage (%)")

total.cov <- kit %>% group_by(replicate) %>% summarize(traycov= sum(coverage))
#this should add up to 100% or less for each quadrat. This makes no sense.

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






##violin plot or density plot wouled be next?
ggplot(data=kit, aes(x = algae, y = replicate, size = coverage)) +
  geom_density()

#3/8/2020 leaving off: I think I need to make a multi panel plot? 

#3/9/2020 I want to make a correlation matrix from the highest to lowest pH

#create what is essentially a heat map? Bubble chart?

pH.8<-kit %>% filter(pH == 8.1)
cov.8 <-pH.8 %>% group_by(algae) %>% summarise(pH8cov = mean(coverage))

pH.6<-kit %>% filter(pH == 6.7)
cov.6 <-pH.6 %>% group_by(algae, color) %>% summarise(pH6cov = mean(coverage))

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
  
#I've decided on three plots
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



#This is my confirmed version of a rough draft. Now I improve each plot!
top_row <- plot_grid(pB,pC, labels = c("A","B"), label_size = 12)
plot_grid(top_row, pA, labels = c("","C"), ncol = 1)

#Returning to plot 1, I need to work on overall presentation and lables. I also 
# want to figure out how to reorder the algae into a ranked chart that 
# mimics chart B.

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


pC<-plot_grid(top, bot, labels = "C",ncol = 1, align = "v")

#This is my confirmed version of a rough draft. Now I improve each plot!
top_row <- plot_grid(pA,pB, labels = c("A","B"), rel_widths = c(3,1), label_size = 12)
draft.Porzio<-plot_grid(top_row, pC, ncol = 1)

ggsave(filename = "figs\\draft.Porzio.pdf", width = 8, height = 5, units = "in")
