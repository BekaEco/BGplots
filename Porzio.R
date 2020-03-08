library("tidyverse")
library("stringr")

kite_ugly <-read.csv("Porzio_middleout.csv", skip = 1)
head(kite_ugly)
kite_ugly<-kite_ugly[,1:28]


kite <- kite_ugly %>% pivot_longer(-MicroAlgae, names_to = "replicate", values_to = "coverage")
kite<-kite %>% mutate_if(is.factor, as.character)

kite$sector <- NA
#kite$type <-NA
head(kite)
kite <-kite %>% separate(col = MicroAlgae , into = c("algae", "color"), sep = "[(]", remove = FALSE, convert=FALSE)
kite$color <-gsub( "[)]", "", kite$color)

