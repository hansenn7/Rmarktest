# Instructor: Silvia Cordero-Sancho, PhD (corderos@umich.edu)
# EAS 543 FALL 2021
# Date: 09/16/2021
# Exercise 3. (Part 3)
# Second order properties


# 1. Library -----------------------------------------------------------------

#library(spatstat)

# 2. Load Data -----------------------------------------------------------

load("EAS543_E02_cleandata.RData")

# 3. G Function: Distance to the Nearest Event----------------------------------------------

set.seed(42) # we set this value, so all results based on simulations are the same

 
?envelope


Gwf_e <- envelope(WF2016_km,
                  Gest,
                  nsim = 99,
                  correction="best",
                  verbose=FALSE)
plot(Gwf_e,
     main="Wilfires distance relationships: G-Function")

# we can modify the plot x-limits

plot(Gwf_e,
     xlim=c(0,5),
     main="Wilfires distance relationships: G-Function")

# 4. F Function: Empty Space Function -----------------------------------------------------------

Fwf_e <- envelope(WF2016_km,
                  Fest,
                  nsim = 99,
                  correction="best",
                  verbose=FALSE)

plot(Fwf_e,
     main="Wilfires distance relationships:F-Function ")


# 5. Ripley's K Function -----------------------------------------------------------------------

Ke <- envelope(WF2016_km,
               Kest,
               nsim = 39,
               correction="best",
               verbose=FALSE)
plot(Ke,
     main="Ripley's K Function",
     legend=FALSE)

plot(Ke,
     xlim=c(0,5),
     main="Ripley's K Function")

Le <- envelope(WF2016_km,
               Lest,
               nsim = 39,
               correction="best",
               verbose = FALSE)


# 6. Pair Correlation Function -----------------------------------------------

pce_e <- envelope(WF2016_km,
              pcf,
               nsim = 39,
               correction="best",
               verbose=FALSE)
plot(pce_e,
     main="pcf-Function")

# How to plot the second order properties at once


SOP <- anylist(Gwf_e,Fwf_e,Ke,Le)

plot(SOP,
     main="Second Order Properties",
     main.panel=c("G-Function", 
                  "F-Function",
                  "K-Function",
                  "L-function"),
     legend=FALSE)

# Can we compare replicate patterns (e.g., two different years)

load("WF2018_km.RData")


Kest18 <- envelope(WF2018,
                   Kest,
                   nsim = 39,
                   correction="best",
                   verbose=FALSE)


K_wf16.18 <- anylist(Ke,Kest18)

# OR use




plot(K_wf16.18, 
     main="Wilfirest K-function (2016 & 2018)",
     main.panel=c("2016","2018"))

# Comparing patterns with the K function

WF16_18 <- anylist(WF2016_km,WF2018)

# Note: it must include ratio =TRUE. This allows to compare stored information 
# that allows compare multiple patterns

K.16.18 <- lapply(WF16_18, Kest, ratio=TRUE)

K.16.18 


# The function "pool" allows to combine K-function results and it calculate an "average" 

Kpool <- pool(as.anylist(K.16.18))



plot(Kpool,
     cbind(pooliso,pooltheo,loiso,hiiso)~r,
     shade=c("loiso","hiiso"))




### END OF DEMO ###
