# Instructor: Silvia Cordero-Sancho, PhD (corderos@umich.edu)
# EAS 543 FALL 2021
# Date: 09/16/2021
# Exercise 3. (Part 2)
# First order properties


# 1. Library -----------------------------------------------------------------

#library(spatstat)

# 2. Load Data -----------------------------------------------------------

load("EAS543_E02_cleandata.RData")

# 3. Inspecting intensity Quadrat Count -------------------------------------------------

## Let's check the dimensions of (A)

## Check OWIN dim (max & min)

xrange <- (WF2016_km$window$xrange[[2]] - WF2016_km$window$xrange[[1]])
yrange <- (WF2016_km$window$yrange[[2]] - WF2016_km$window$yrange[[1]])

xrange # 191.6 km
yrange # 127.5 km


# QC with approximately 10 x 10 km quadrant (100 km^2)

QC100_2016 <- quadratcount(WF2016_km,
                        nx=round(xrange/10), #19
                        ny=round(yrange/10)) # 13

plot(WF2016_km,
     pch=20,
     main=expression(bold("Quadrat Count \n (Quadrant Area = 100 km"^2*")")))  # \n adds a line break

plot(QC100_2016,
     add=TRUE,
     col="blue",
     cex=.5,
     lty=1)

# QC with approximately 15.81 x 15.81 km quadrant (250 km^2)

QC250_2016 <- quadratcount(WF2016_km,
                          nx=round(xrange/sqrt(250)),
                          ny=round(yrange/sqrt(250)))

plot(WF2016_km,
     pch=20,
     main=expression(bold("Quadrat Count \n (Quadrant Area ~ 250 km"^2*")")))  # \n adds a line break

plot(QC250_2016,
     add=TRUE,
     col="blue",
     cex=.5,
     lty=1)

# QC with approximately 22.36 x 22.36 km quadrant (~500 km2)

QC500_2016 <- quadratcount(WF2016_km,
                          nx=round(xrange/22.36),
                          ny=round(yrange/22.36))

plot(WF2016_km,
     pch=20,
     main= expression(bold("Quadrat Count \n (Quadrant Area ~ 500 km"^2*")")))

plot(QC500_2016,
     add=TRUE,
     col="red",
     cex=.5,
     lty=1)



# 3.1. QC (intensity maps) -----------------------------------------------------
# Calculating intensity maps (using list and anylapply)

??anylapply

col1 <- colorRampPalette(c("#fff7bc","#d95f0e"))


QC_list <- list(QC100_2016,QC250_2016,QC500_2016) #save qc results as a list 

QC_intensity <- anylapply(QC_list,intensity,image=TRUE)
names(QC_intensity) <- c("Q100","Q250","Q500")

class(QC_intensity)

plot(QC_intensity, main="", 
     col=col1(20),
     main.panel=c("100x100","250x250","500x500"),
     mar.panel=c(rep(0.65,4)))
dev.off()

# Otherwise...

# QC100I_2016 <- intensity(QC100_2016, image=TRUE)

# plot(QC100I_2016,
#      col=pal,
#      main=expression(bold("Intensity: Quadrant Area = 100 km"^2*"")))


# 3.2. Comparing QC intensity results -----------------------------------------------

n <- c(length(QC_intensity))

out1 <- array(NA,c(3,2));out1          # 3 rows/ two columns

for (i in 1:n)                         # There are 3 outputs
        {
        int=range(QC_intensity[[i]])
        out1[i,]=int
        }

out2 <- data.frame(NAME=c("Q100","Q250","Q500"),
                   MIN=round(out1[,1],3),
                   MAX=round(out1[,2],3))

out2 #effect of quadrat-size and changes in intensity 

# 4. QC example with a tessellation window -------------------------------------------
?hextess 

HW <- hextess(WF2016_km, 13.88)  # if s = 13.88, the hexagon area is approximately 500 km^2
                                 # s is the side length of the hexagon 
plot(HW, 
     main="QC-Hexagonal Grid (500 km^2)",
     lty=2,
     lwd=1)

plot(WF2016_km,
     pch=20,
     cex=1.5,
     col="red",
          add=TRUE)

# Quadratcount test with an hexagon-grid

QHW <- quadratcount(WF2016_km,tess = HW)

QHWI <-  intensity(QHW, image=TRUE)


plot(QHWI,
     main="QC Intensity (Hexagon Grid 500 km^2)", 
     col = heat.colors(10))

plot(QHW, cex=0.5, 
     main="",
     add=TRUE)  


# 4.1 QC Test -------------------------------------------------------------


QT <- quadrat.test(QC500_2016) # see warning (this is due the quantity of 0s)

QT

# 5. Kernel Smoothed Intensity of Point Pattern  ------------------------------------------------------
?density.ppp


# 5.1. Comparing the effect of edge correction ----------------------------

# Without edge correction

WF2016_den1 <- density.ppp(WF2016_km,
                           edge=FALSE,
                           sigma = bw.diggle,
                           se=TRUE)

class(WF2016_den1)

str(WF2016_den1) # "it stores two "im" objects,one for the kernel density image and the other the se-density image 

# With edge correction

WF2016_denec <- density.ppp(WF2016_km,
                            sigma = bw.diggle,
                            diggle = TRUE, #  Jones-Diggle improved edge correction
                            se=TRUE)

# Plotting

den_W16 <- as.anylist(list(WF2016_den1$estimate,WF2016_denec$estimate))

plot(den_W16,
      main="Wilfire Denisty Maps", 
      col=heat.colors(20, rev=TRUE),
      main.panel=c("Without edge correction","Edge corrected"),
      mar.panel=c(rep(0.65,4)))


dev.off()

# 5.1.1. Density Maps and Standard errors (comparing edge effects) --------

denSE_W16 <- as.anylist(list(WF2016_den1$SE,WF2016_denec$SE))


col2 <- colorRampPalette(c("grey","red"))

plot(denSE_W16,
     main="Denisty Maps (SE)", 
     col=col2(10),
     main.panel=c("Without edge correction","Edge corrected"),
     mar.panel=c(rep(0.65,4)))

dev.off()

# Extract information from "im" objects

se_nc <- round(range(WF2016_den1$SE),3) 

se_c <- round(range(WF2016_denec$SE),3)

(se_compare <- matrix(c(se_nc,se_c),
                      nrow=2,
                      ncol=2,
                      dimnames = list(c("uncorrected","corrected"),c("min","max")),
                      byrow = TRUE))


# Single map (with contour lines)

# Density plot (edge corrected)
plot(WF2016_denec$estimate, 
     col=heat.colors(20, rev=TRUE), 
     main="Wilfire Density Map 2016")
plot(WF2016_km,pch=19,cex=0.5,add=TRUE, col="purple")
plot(WF2016_km$window,add=TRUE)
contour(WF2016_denec$estimate, cex=0.5,add=TRUE, col="grey50")

# SE plot
plot(WF2016_denec$SE, 
     col=col1(20), 
     main="Denisty Map (Standard Error)" )
plot(WF2016_km,pch=19,cex=0.5,add=TRUE, col="black")
plot(WF2016_km$window,add=TRUE)
contour(WF2016_denec$SE, cex=0.5,add=TRUE, col="grey50")


####

rm(list = ls())

###

# END OF CODE--- go to EAS543DEMO_T2_P3.R

