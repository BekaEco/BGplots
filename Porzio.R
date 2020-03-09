library("tidyverse")
library("stringr")


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

total.cov <-kit %>% group_by(replicate) %>% summarize(traycov= sum(coverage))
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
sect.cov <-kit %>% group_by(sector, algae) %>% summarise(meancov = mean(coverage))

ggplot(data=sect.cov, aes(sector, y = reorder(algae, meancov), size = ifelse(meancov==0, NA, meancov))) +
  geom_point() +
  theme_minimal()


#Now I will take the means of the sectors, that is the main issue

ggplot(data = kit, aes(x=sector, y = algae, fill = coverage)) +
  geom_point()


##violin plot or density plot wouled be next?
ggplot(data=kit, aes(x = algae, y = replicate, size = coverage)) +
  geom_density()

#3/8/2020 leaving off: I think I need to make a multi panel plot? 

