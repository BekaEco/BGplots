library(sp)

fish_CN <- read.csv("fishCN.csv")
ter_CN <- read.csv("terCN.csv")
lit_CN <- read.csv("litCN.csv")
pelag_CN<- read.csv("pelagCN.csv")


#Scenario2
TEF_C.mu <- 0.4 #based on Post 2002
TEF_N.mu <- 6.8 #Postx2  $4.3 is based on Bunn 2013 omnivor
TEF_C.sigma <- 0 #1.3 # 1.3 #The first one is with TEF.sigmas of zero (I can plot error bars if I want in the future)
TEF_N.sigma <- 0  #1.5 # 0.2 #The first one also had N's TEF sd of zero (I can plot error bars if I want in the future)
scenarioID <- "Postx2"


fish_CN$include1sigma <- "exclude" ## only do this once! Otherwise, I'd erase each previous set!
fish_CN$include2sigma <- "exclude"



Lakenames <-as.vector(unique(fish_CN$Lake))
plotname <- paste0("figs/allLakesIsoscapeswithPolygon",scenarioID ,".png")
png(filename = plotname, width = 800, height = 800) #where will this go and what size?
par(oma = c(5,6,1,9), #outer margins, c(bottom, left, top, and right) Room for d13C and d15N labels at the end
    mfrow = c(4,4), ##mfrow is my four rows by four columns 
    mar = c(.5,.5,1.5,.5), #margins, c(bottom,
    cex.axis = 1.75) 

# This loop does two things, first, it plots each lake with a polygon around it. Second, it also
# indicates which points are in or out of the two polygons.


#LOOP####
for(j in 1:(length(Lakenames))){
  
  mixture <- fish_CN[(which(fish_CN$Lake ==  Lakenames[j])),
                     c("d13C_VPDB", "d15N_air", "Identifier.1")]
  
  #Littoral sources - so the results from the rocks
  ##for now, select lake with a number
  
  lakesource_littoral <- lit_CN[which(lit_CN$Lake ==  Lakenames[j]),c("d13C_VPDB", "d15N_air")]
  
  Lit.d13C.mu <- mean(lakesource_littoral$d13C_VPDB)
  Lit.d15N.mu <- mean(lakesource_littoral$d15N_air)
  Lit.d13C.sigma <- sd(lakesource_littoral$d13C_VPDB)
  Lit.d15N.sigma <- sd(lakesource_littoral$d15N_air)
  
  #Pelagic - from the pelagic area
  lakesource_pelagic <- pelag_CN[which(pelag_CN$Lake ==  Lakenames[j]),c("d13C_VPDB", "d15N_air")]
  
  Pelag.d13C.mu <- mean(lakesource_pelagic$d13C_VPDB)
  Pelag.d15N.mu <- mean(lakesource_pelagic$d15N_air)
  Pelag.d13C.sigma <- sd(lakesource_pelagic$d13C_VPDB)
  Pelag.d15N.sigma <- sd(lakesource_pelagic$d15N_air)
  
  #Terrestrial - from the surrounding plants 
  lakesource_terrestrial <- ter_CN[which(ter_CN$Lake ==  Lakenames[j]),c("d13C_VPDB", "d15N_air")]
  
  Ter.d13C.mu <- mean(lakesource_terrestrial$d13C_VPDB)
  Ter.d15N.mu <- mean(lakesource_terrestrial$d15N_air)
  Ter.d13C.sigma <- sd(lakesource_terrestrial$d13C_VPDB)
  Ter.d15N.sigma <- sd(lakesource_terrestrial$d15N_air)
  
  #PLOT####
  plot(x = mixture$d13C_VPDB, y = mixture$d15N_air,
       pch = 20,
       cex = 1.5,
       col = "#a6611a",
       ylim = c(-15,11),
       xlim = c(-37,-17),
       xlab = "", #expression(paste(delta^{13}, "C (\u2030)")),
       ylab =  "", #expression(paste(delta^{15}, "N (\u2030)")),
       bty = "l",
       yaxt = "n",
       xaxt = "n",
       ann = FALSE)
  title(Lakenames[j], line= -1)
  ##This little section is the if/else for the axis lables#
  if(j == 1 || j == 5 || j == 9){
    axis(2, at = seq(-10, 10,5), labels = TRUE, las = 2)
    axis(1, at = seq(-35, 15, 5), labels = FALSE)
  } else if (j == 13){
    axis(2, at = seq(-10, 10,5), labels = TRUE)
    axis(1, at = seq(-35, 15, 5), labels = TRUE)
  } else if (j == 14 || j == 15|| j == 16) {
    axis(2, at = seq(-10, 10,5), labels = FALSE)
    axis(1, at = seq(-35, 15, 5), labels = TRUE)
  } else {
    axis(2, at = seq(-10, 10,5), labels = FALSE)
    axis(1, at = seq(-35, 15, 5), labels = FALSE)
  }
  
  all.verts.x <- c(Ter.d13C.mu,Ter.d13C.mu,Ter.d13C.mu-Ter.d13C.sigma,Ter.d13C.mu+Ter.d13C.sigma,
                   Lit.d13C.mu,Lit.d13C.mu,Lit.d13C.mu-Lit.d13C.sigma,Lit.d13C.mu+Lit.d13C.sigma,
                   Pelag.d13C.mu,Pelag.d13C.mu,Pelag.d13C.mu-Pelag.d13C.sigma,Pelag.d13C.mu+Pelag.d13C.sigma)
  all.verts.y <- c(Ter.d15N.mu-Ter.d15N.sigma,Ter.d15N.mu+Ter.d15N.sigma,Ter.d15N.mu,Ter.d15N.mu,
                   Lit.d15N.mu-Lit.d15N.sigma,Lit.d15N.mu+Lit.d15N.sigma,Lit.d15N.mu,Lit.d15N.mu,
                   Pelag.d15N.mu-Pelag.d15N.sigma,Pelag.d15N.mu+Pelag.d15N.sigma,Pelag.d15N.mu,Pelag.d15N.mu)
  polygon.edges <-chull(x = all.verts.x, y = all.verts.y)
  all.2sigma.verts.x <- c(Ter.d13C.mu,Ter.d13C.mu,Ter.d13C.mu-Ter.d13C.sigma*2,Ter.d13C.mu+Ter.d13C.sigma*2,
                          Lit.d13C.mu,Lit.d13C.mu,Lit.d13C.mu-Lit.d13C.sigma*2,Lit.d13C.mu+Lit.d13C.sigma*2,
                          Pelag.d13C.mu,Pelag.d13C.mu,Pelag.d13C.mu-Pelag.d13C.sigma*2,Pelag.d13C.mu+Pelag.d13C.sigma*2)
  all.2sigma.verts.y <- c(Ter.d15N.mu-Ter.d15N.sigma*2,Ter.d15N.mu+Ter.d15N.sigma*2,Ter.d15N.mu,Ter.d15N.mu,
                          Lit.d15N.mu-Lit.d15N.sigma*2,Lit.d15N.mu+Lit.d15N.sigma*2,Lit.d15N.mu,Lit.d15N.mu,
                          Pelag.d15N.mu-Pelag.d15N.sigma*2,Pelag.d15N.mu+Pelag.d15N.sigma*2,Pelag.d15N.mu,Pelag.d15N.mu)
  polygon.2sigma.edges <-chull(x = all.2sigma.verts.x, y = all.2sigma.verts.y)
  
  polygon(x = c(Ter.d13C.mu,Lit.d13C.mu,Pelag.d13C.mu), 
          y = c(Ter.d15N.mu,Lit.d15N.mu,Pelag.d15N.mu), border = FALSE, 
          col = rgb(red = 190, green = 190, blue = 190, alpha = 100, maxColorValue = 300))
  polygon(x = all.verts.x[polygon.edges], 
          y = all.verts.y[polygon.edges], border = FALSE, 
          col = rgb(red = 190, green = 190, blue = 190, alpha = 100, maxColorValue = 300))
  polygon(x = all.2sigma.verts.x[polygon.2sigma.edges], 
          y = all.2sigma.verts.y[polygon.2sigma.edges], border = FALSE, 
          col = rgb(red = 190, green = 190, blue = 190, alpha = 100, maxColorValue = 300))
  points(x = Ter.d13C.mu, y = Ter.d15N.mu,
         pch = 17,
         col = "#238b45",
         cex = 1.5)
  arrows(x0 = Ter.d13C.mu, y0 = Ter.d15N.mu-Ter.d15N.sigma, 
         x1 = Ter.d13C.mu, y1 = Ter.d15N.mu+Ter.d15N.sigma, 
         code = 3, angle = 90, length = 0.05, col = "#238b45", lwd = 2)
  arrows(x0 = Ter.d13C.mu-Ter.d13C.sigma, y0 = Ter.d15N.mu, 
         x1 = Ter.d13C.mu+Ter.d13C.sigma, y1 = Ter.d15N.mu, 
         code = 3, angle = 90, length = 0.05, col = "#238b45", lwd = 2)
  
  points(Lit.d13C.mu, Lit.d15N.mu,
         pch = 8,
         col = "#66c2a4",
         cex = 1.5)
  arrows(x0 = Lit.d13C.mu, y0 = Lit.d15N.mu-Lit.d15N.sigma, 
         x1 = Lit.d13C.mu, y1 = Lit.d15N.mu+Lit.d15N.sigma, 
         code = 3, angle = 90, length = 0.05, col = "#66c2a4", lwd = 2)
  arrows(x0 = Lit.d13C.mu-Lit.d13C.sigma, y0 = Lit.d15N.mu, 
         x1 = Lit.d13C.mu+Lit.d13C.sigma, y1 = Lit.d15N.mu, 
         code = 3, angle = 90, length = 0.05, col = "#66c2a4", lwd = 2)
  
  points(x = Pelag.d13C.mu, y = Pelag.d15N.mu,
         pch = 4,
         col = "#526EFF",
         cex = 1.5)
  arrows(x0 = Pelag.d13C.mu, y0 = Pelag.d15N.mu-Pelag.d15N.sigma, 
         x1 = Pelag.d13C.mu, y1 = Pelag.d15N.mu+Pelag.d15N.sigma, 
         code = 3, angle = 90, length = 0.05, col = "#526EFF", lwd = 2)
  arrows(x0 = Pelag.d13C.mu-Pelag.d13C.sigma, y0 = Pelag.d15N.mu, 
         x1 = Pelag.d13C.mu+Pelag.d13C.sigma, y1 = Pelag.d15N.mu, 
         code = 3, angle = 90, length = 0.05, col = "#526EFF", lwd = 2)
  ##Make this optional:
  points(x = mixture$d13C_VPDB-TEF_C.mu, y = mixture$d15N_air-TEF_N.mu,
         pch = 20,
         col = "black",
         cex = 1.5)
  arrows(x0 = mixture$d13C_VPDB-TEF_C.mu, y0 = mixture$d15N_air-TEF_N.mu-TEF_N.sigma,
         x1 = mixture$d13C_VPDB-TEF_C.mu, y1 = mixture$d15N_air-TEF_N.mu+TEF_N.sigma,
         code = 3, angle = 90, length = 0, col = "black", lty = 3, lwd = 2)
  arrows(x0 = mixture$d13C_VPDB-TEF_C.mu-TEF_C.sigma, y0 = mixture$d15N_air-TEF_N.mu,
         x1 = mixture$d13C_VPDB-TEF_C.mu+TEF_C.sigma, y1 = mixture$d15N_air-TEF_N.mu,
         code = 3, angle = 90, length = 0, col = "black", lty = 3, lwd = 2)
  
  
  ##Now I need to track which fish to remove
  # library(sp)
  In.out.fish1sig <- point.in.polygon(point.x = mixture$d13C_VPDB-TEF_C.mu, 
                                      point.y = mixture$d15N_air-TEF_N.mu,
                                      pol.x = all.verts.x[polygon.edges], 
                                      pol.y = all.verts.y[polygon.edges])
  
  fish.to.keep1sig <- mixture$Identifier.1[which(In.out.fish1sig == 1)]
  
  
  fish_CN$include1sigma[match(fish.to.keep1sig,fish_CN$Identifier.1)] <- "include"
  
  In.out.fish2sig <- point.in.polygon(point.x = mixture$d13C_VPDB-TEF_C.mu, point.y = mixture$d15N_air-TEF_N.mu,
                                      pol.x = all.2sigma.verts.x[polygon.2sigma.edges], 
                                      pol.y = all.2sigma.verts.y[polygon.2sigma.edges])
  
  fish.to.keep2sig <- mixture$Identifier.1[which(In.out.fish2sig == 1)]
  
  
  fish_CN$include2sigma[match(fish.to.keep2sig,fish_CN$Identifier.1)] <- "include"
  
}

#These two line write the large single label for the plots
mtext(text= expression(paste(delta^{13}, "C (\u2030)")) ,side=1,line=4,outer=TRUE, cex = 1.75)
mtext(text= expression(paste(delta^{15}, "N (\u2030)")) ,side=2,line=3,outer=TRUE, cex = 1.75)
#legend
legend(xpd = NA , "topright", inset = c(-.6,-3.4) ,#xpd = NA; so topright means whole area, a little outside, OK to print outside margin
       legend = c("fish", "fish+TEF", "","terrestrial", "pelagic", "littoral", "","polygon", "1 sd", "2 sd") ,
       pch = c(20, 20,NA,  17,4,8,NA,15,15,15),
       col = c("#a6611a", "black",NA, "#238b45", "#526EFF", "#66c2a4",NA,
               rgb(red = 190, green = 190, blue = 190, alpha = 300, maxColorValue = 300),
               rgb(red = 190, green = 190, blue = 190, alpha = 200, maxColorValue = 300),
               rgb(red = 190, green = 190, blue = 190, alpha = 100, maxColorValue = 300)),
       bty = "n",
       cex= 1.75)


## 11/21/19
## I  need to add shades of gray for the polygons in order to ID what is 1, sd, and 2 sd - notes to self
dev.off()
