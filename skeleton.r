#######################
### skeleton.r (replace with script name)
### Description: Start all scripts with this structure and modify contents as
### needed
#######################

rm(list=ls()) #Clear environment to avoid issues

#########################################
# Load packages
#########################################

library(ncdf4) # file reading/writing
library(tidyverse) # essential packages (incl ggplot2, dplyr, tibble)
library(scales) # scaling ggplot data
library(patchwork) # Plotting multiple panels on one plot page
library(grid); library(gridExtra) #for plotting multiple plots in same figure
library(lubridate)

#########################################
# Define Global Functions
#########################################

#ggplot format function
theme_LK <- function() {
  theme(
    plot.title = element_text(color="black", hjust = 0.5, size=14, face="bold"),
    plot.subtitle = element_text(color="black", hjust = 0.5, size=12),
    axis.line=element_line(size=0.75),
    axis.title.x=element_text(size=15,color="black"),
    axis.text.x=element_text(size=12,color="black"),
    axis.title.y=element_text(size=15,color="black"),
    axis.text.y=element_text(size=12,color="black"),
    #legend.title=element_text(size=12,color="black"),
    legend.title=element_blank(),
    #legend.title.align = 0.5,
    legend.text=element_text(size=10,color="black"),
    legend.background=element_rect(fill = "white", color = "black"),
    legend.key=element_blank(),
    legend.key.size=unit(0.8,"lines"),
    #legend.spacing.y=unit(0.1, "cm"),
    legend.position=c(0.6, 0.88),
    panel.grid.major = element_line(linetype = "dotted", color = "gray"),
    panel.grid.minor=element_blank(),
    panel.background=element_blank(),
    strip.background = element_blank(),
    strip.text = element_text(size = 12, face = "bold"),
    strip.placement = "outside"
  )
}



#########################################
# Define Global Filepaths
#########################################


#########################################
# Source dependent scripts
#########################################


#########################################
# Define Global Variables and constants
#########################################


#########################################
# Define Runtime Options
#########################################

debug_TF <- F #Boolean: should we enter debug routines or no?


#########################################
# Begin main
#########################################

# Do stuff


#########################################
# Plot results
#########################################

# EOF
