# Instructor: Silvia Cordero-Sancho, PhD (corderos@umich.edu)
# EAS 543 FALL 2021
# Date: 09/16/2021
# Exercise 3. (Part 1)


# 1. Installing and calling packages --------------------------------------------------

# install.packages("xfun")

# the package xfun allows you to call your libraries and install missing ones.

xfun::pkg_attach(c("sf","maptools","sp","spatstat","tmap"), install = TRUE) 


# 2. Importing spatial data from a geodatabase  ---------------------------------

# Today we are learning how to extract spatial data from a geodatabase (*.GDB)

# Remember a geodatabase can storage multiple files. 
# To preview the files, we can use the function 

?st_layers

st_layers("./Ex2_data.gdb")

# The GDB file contain 3 files:
# northernMIPFO a Multi Polygon: this is our study area (A) 
# northernMIPFO_original: the original boundaries of the different protected areas.
# northmiPFO5km_wf2016: a point layers with the x-y location of wildfires in MI for the year 2016

# We will use the same function we used to import the shapefile in Exercise 1 (st_read), 
# but in this case, we are going to indicate the name of the layer we like to extract from the GDB . 
# For this exercise/lab, we are going to maintain the projection as it is


SA_preview <- st_read("./Ex2_data.gdb", 
                      layer = "northernMIPFO_original" )

studyarea <-st_read("./Ex2_data.gdb", 
                    layer = "northernMIPFO")


WF_2016 <- st_read("./Ex2_data.gdb",
                    layer = "northmiPFO5km_wf2016")


# 3.  Preview data {tmap} -----------------------------------------------------------

# Let's preview our data and study area with tmap

bmap.esri <- 'https://server.arcgisonline.com/ArcGIS/rest/services/World_Imagery/MapServer/tile/{z}/{y}/{x}.png'

# EPSG:3078
sa.prev <- 
    tm_basemap(bmap.esri) +
    tm_shape(SA_preview) +
    tm_fill(
        col="NAME",
        alpha = 0.4,
        lwd=2,
        border.col="red",
        leyend.show=FALSE,
        labels = "NAME")+ # legend is not showing park names
    tm_shape(WF_2016) +
    tm_dots(size = 0.1,
            shape=19,
            col="orange")
    
tmap_leaflet(sa.prev)

data1.prev <- 
    tm_basemap(bmap.esri) +
    tm_shape(studyarea) +
    tm_polygons(alpha = 0.05,
                lwd=2,
                col="white",
                border.col="red") +
    tm_shape(WF_2016) +
    tm_dots(size = 0.1,
            shape=19,
            col="orange")

tmap_leaflet(data1.prev)


# 4. Select by location {sf} ---------------------------------------------------

# We have observations outside the four protected regions (our study area)
# Therefore we need to remove those observations

?st_intersects

WFSA_2016 <- 
    st_intersection(studyarea,WF_2016)

dim(WFSA_2016) == dim(WF_2016)

# Let's review the new object

prev2 <- 
tm_shape(studyarea) +
    tm_polygons(col= "#e5f5e0",
                alpha = 0.5,
                lwd=2,
                border.col="#31a354") +
    tm_shape(WFSA_2016) +
    tm_dots(size =0.1,
            shape=19,
            col="red")

tmap_leaflet(prev2)


# 5. Dealing with spatial data {spatstat}  -------------------------------------------

# {spatsat} employs their own format to define spatial objects: ppp and owin
# However, there is not a direct way to transform sf-objects to spatstat format, 
# thus we need to transform the sf object to sp,  and then to spatstat.

# Transforming sf to sp to owin

sa_sp <- as(studyarea,"Spatial") # Transformation from a  sf-object to a sp-object

class(sa_sp)

sa_ow <- as(sa_sp,"owin") # Transformation from a sp-object to a owin one

class(sa_ow)

# We need to assign the units to the owin object. Let's check the projection info:

sa_sp@proj4string

# Units are in meters

unitname(sa_ow) <- c("meter","meters") # attach projection units to owin object

summary(sa_ow)

par(mar=c(rep(.5,4)),oma=c(rep(.25,4)))
plot(sa_ow, main="")

# Let's simplify the window
?simplify.owin

sa_owin <- simplify.owin(sa_ow,2500);plot(sa_owin,main="2500")

dev.off() # Restores margins to original values

# Comparing old and new owins

?anylist

owins <- as.anylist(list(sa_ow,sa_owin))

par(mar=c(rep(1,4)),oma=c(rep(.1,4)))
plot(owins,
     main="Simplifying polygonal",
     main.panel=c("Before","After"))

dev.off() # Restores margins to original values

# Transforming events from sf to sp to ppp

wfsa2016_sp <- as(WFSA_2016,"Spatial") # Transformation from a  sf-object to a sp-object

class(wfsa2016_sp)

WF2016ppp <- as(wfsa2016_sp,
                "ppp")[sa_owin]    # transform point feature to ppp with a defined window of observation 


class(WF2016ppp)

summary(WF2016ppp)


# 6. Define Spatial Point Object {spatstat} ------------------------------------------

# For this exercise we are not going to deal with marks (or attributes)
# we will remove the attributes:

WF2016pppu <- unmark(WF2016ppp)



# 7. Changing "ppp" units from m to km {spatstat} --------------------------------------------

## We are making this change to facilitate the interpretation of our analysis

WF2016_km <- rescale(WF2016pppu,1000,"km")    ## This is a spatstat function, check the properties with ?rescale

summary(WF2016_km)

# 8. Plotting a ppp-object {spatstat} -----------------------------------------------

# Note: tmap does not support ppp-objects

detach(package:tmap)


par(mar=c(4.8,2,2,2),oma=c(2,1,1,1)) # modify plot margins
plot(WF2016_km,
     cols="red",
     pch=19,
     cex=0.5,
     main="")
title(main = "Wilfires (2016)",
      sub = "Pere Marquette S.F., Au Sable S.F., Huron N.F., Mackinac S.F. &  Hartwick Pines S.P. ",
      cex.main=1.3,cex.sub = .75)

dev.off() # Restores margins to original values



# 9. Saving data ----------------------------------------------------------

save(WF2016_km,
     file="EAS543_E02_cleandata.RData",
     compress = TRUE)


## The following line of code will remove all the objects in your environment, 
## Only run line 86 when you are sure that you saved the results you need

rm(list = ls())
detach(package:sf)


###############################
## END OF EAS543DEMO_E1_P1.R ##
###############################
# Go to EAS543DEMO_E2_P2.R
