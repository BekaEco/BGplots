library("tidyverse")
library("stringr")


kite_ugly <-read.csv("Porzio_middleout.csv", skip = 1)
head(kite_ugly)
kite_ugly<-kite_ugly[,1:28] #get rid of empty columns at the end

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

#begin plotting
colnames(kit)
head(kit)

