#######################
### R_intro.r
### Description: General intro to the packages we use in the LAIR group
###
### Note! This document uses my R "skeleton" script that I made which is a 
###  template script that separates the code into these sections. 
###  This is also included in the zipped file provided.
###
### General tip, if you're confused about the syntax or arguments of a function, 
### e.g. the paste() function, you can all the details of the usage, arguments,
### examples, etc by just typing "?" then the function name (e.g. type ?paste in 
### the console to learn more about the paste() function)
#######################

# Generally I start by clearing the environment to avoid issues
rm(list=ls()) #clear 

#########################################
# Load packages
#########################################

# First, install the packages. 
# Once you've installed the packages you can comment out these lines and you won't need to re-install them
# install.packages("ncdf4") # netCDF file reading/writing
# install.packages("tidyverse") # a "package of packages", includes essential packages (ggplot2, dplyr, tibble)
# install.packages("lubridate") # working effectively
# install.packages("scales") # scaling ggplot data to make maps
# install.packages("patchwork") # Plotting multiple panels on one plot page
# install.packages("grid"); install.packages("gridExtra") # Some other packages for plotting multiple panels on one plot page (not shown in example tutorial here)


# You will need to load packages every time you open a new environment though 
library(ncdf4) # netCDF file reading/writing
library(tidyverse) # a "package of packages", includes essential packages (ggplot2, dplyr, tibble)
library(lubridate) # working effectively
library(scales) # scaling ggplot data to make maps
library(patchwork) # Plotting multiple panels on one plot page
library(grid); library(gridExtra) # Some other packages for plotting multiple panels on one plot page


#########################################
# Define Global Functions
#########################################

# This is a ggplot format function which I will call later
# The elements here are just a start - there are many ggplot theme options
theme_LK <- function() {
  theme(
    plot.title = element_text(color="black", hjust = 0.5, size=14, face="bold"),
    plot.subtitle = element_text(color="black", hjust = 0.5, size=12),
    axis.line=element_line(size=0.75),
    axis.title.x=element_text(size=15,color="black"),
    axis.text.x=element_text(size=12,color="black"),
    axis.title.y=element_text(size=15,color="black"),
    axis.text.y=element_text(size=12,color="black"),
    legend.title=element_blank(),
    legend.text=element_text(size=14,color="black"),
    legend.background=element_rect(fill = "white", color = "black"),
    legend.key=element_blank(),
    legend.key.size=unit(1,"lines"),
    legend.position=c(0.85, 0.88),
    panel.grid.major = element_line(linetype = "dotted", color = "gray"),
    panel.grid.minor=element_blank(),
    panel.background=element_blank(),
    strip.background = element_blank(),
    strip.text = element_text(size = 12, face = "bold"),
    strip.placement = "outside"
  )
}

# Convert a Julian date (in decimal notation) to POSIX time
# I use this to process the data file I provided
julian.to.POSIX <- function(julians,epoch=ISOdatetime(0001,1,1,0,0,0,tz="UTC")) {
  # Convention is that the Julian date of the epoch is 1.0 (not 0!)
  retval <- epoch+86400*(julians-1.0)
  attributes(retval)$tzone <- attributes(epoch)$tzone
  return(retval)
}

#########################################
# Define Global Filepaths
#########################################

# local paths; assumes this is run in the directory where script is located
plot_dir <- "plots/" 
data_dir <- "include/" 

#########################################
# Source dependent scripts
#########################################

# this is where you would run any prerequisite scripts using the source() function

#########################################
# Define Global Variables and constants
#########################################

# none here but usually there will be some

#########################################
# Define Runtime Options
#########################################

# this just is an example but I don't even use it in the main code here
debug_TF <- F #Boolean: should we enter debug routines or no?



#########################################
# Begin main
#########################################

# Load a netcdf file (this is some solar radiation data at an eddy covariance site in the Sierras)
filename <- paste0(data_dir, "US-CZ2.nc") #the paste and paste0 functions are used to concatenate strings together, we use these a lot
data.nc <- nc_open(filename)

# Extract some of the variables 
posix_time <- julian.to.POSIX(ncvar_get(data.nc, "TIME"), epoch = ISOdatetime(2006,
              1, 1, 0,0,0,tz = "UTC")) #convert to POSIX time knowing that the time variable is in "seconds since 01/01/2006"
SOLAR_in <- ncvar_get(data.nc, "SOLAR_IN") #Incoming Solar Radiation (ie "insolation"), units = W m-2

# close the netcdf file
nc_close(data.nc)

# Use tibble package to make a readable data.frame object with your data
data.df <- data.frame(time = posix_time, SOLAR_in = SOLAR_in) %>% as_tibble()

# show a snapshot of the contents of the object in the console
data.df

### Now we will use the dplyr and lubridate packages to average the data by day of year for different times of day

# Note: "%>%" is from the dplyr package and is R's pipe operator. The output of the 
# function preceding %>% is passed along as the first argument to the function that follows

# Also: "mutate" is a dplyr function and lets you add a new column to your data frame

# Also also: the day-of-year is found with the lubridate package function "yday" and
# returns an integer value corresponding to number day of year (e.g. 1 = Jan 1, 365 or 366 = Dec 31)

data.df <- data.df %>% 
  mutate(DoY = lubridate::yday(time), midday = (lubridate::hour(time) > 17 & lubridate::hour(time) < 21)) %>% #get DoY and also make a boolean for "midday" data between ~17-21 UTC hour
  relocate(DoY, .after = time) #place the new column in a more logical order

data.df #show data snapshot in the console

# Group the incoming solar radiation data by day-of-year and rejoin with summary.df
# Note, filter() is another great dplyr function
summary.df <- data.df %>% 
  filter(!is.na(SOLAR_in)) %>% #filter out the NA values, otherwise the "mean" function below won't work
  group_by(DoY) %>% #group by day of year
  summarise_at(vars(SOLAR_in), list(SOLAR_in = mean))

# Do the same but now filter also for midday values and rejoin with the summary.df data frame
summary.df <- data.df %>% 
  filter(!is.na(SOLAR_in)) %>% #filter out the NA values, otherwise the "mean" function below won't work
  filter(midday == TRUE) %>% #filter for the "midday" data
  group_by(DoY) %>% #group by day of year
  summarise_at(vars(SOLAR_in), list(SOLAR_in_midday = mean)) %>%
  left_join(summary.df, by = "DoY") #join with the existing summary.df object to add this column 

summary.df #show data snapshot; this will be the data we plot

# we now have average incoming solar radiation and PAR for every day of the year



#########################################
# Plot results
#########################################

# first let's view the data by plotting with base R
# Note, this will print directly to standard output (Plots panel of RStudio)
ylim <- c(min(summary.df[, 2:3]), max(summary.df[, 2:3])) #first you need to set the y limits
plot(x = summary.df$DoY, y = summary.df$SOLAR_in, type = 'l', lwd = 2, col = 'black', 
     ylim = ylim, ylab = "Incoming Solar Radiation [W m-2]", xlab = "Day of Year") #plot the 24-hour avg vals
lines(x = summary.df$DoY, y = summary.df$SOLAR_in_midday, lwd = 2, col = 'red') #add the midday values
legend("topright", legend = c("24-hour avg", "Midday avg"), col = c("black", "red"), lwd = 2 ) #add legend
# Actually, looks pretty nice

# Sometimes base R plots look good, but we often use ggplot since it has more functionality
# In many ways it's easier to use, too. Here's how to use dplyr pipe operators to make it easy
gg1 <- summary.df %>%
  ggplot() + # Note that ggplot has its own pipe operator which is the + sign
  theme_LK() +
  geom_line(aes(x = DoY, y = SOLAR_in, color = "black"), size = 1) + #note, we put the xdata, ydata and color in the "aes" function which defines global aesthetics that can be used in calls to make the legend, etc
  geom_line(aes(x = DoY, y = SOLAR_in_midday, color = "red"), size = 1) + 
  ylim(ylim) + 
  xlab("Day of Year") +
  ylab("Incoming Solar Radiation [W m-2]") +
  scale_color_identity(breaks = c("black", "red"), 
                       labels = c("24-hour avg", "Midday avg"), guide = "legend")
  
print(gg1)
# This also looks good

# if we wanted to, we could plot the 24-hour and midday avg plots side by side in a multi-panel plot
# first assign some ggplot objects with the individual plots (we will call them gg2 and gg3)
gg2 <- summary.df %>%
  ggplot() + 
  theme_LK() +
  geom_line(aes(x = DoY, y = SOLAR_in), color = "black", size = 1) + #notice I took the color argument out of the aes function, since I won't be making a legend
  ylim(ylim) + 
  xlab("Day of Year") +
  ylab("Incoming Solar Radiation [W m-2]") +
  ggtitle("24-hour avg")

gg3 <- summary.df %>%
  ggplot() + 
  theme_LK() +
  geom_line(aes(x = DoY, y = SOLAR_in_midday), color = "red", size = 1) + 
  ylim(ylim) + 
  xlab("Day of Year") +
  ylab("Incoming Solar Radiation [W m-2]") +
  ggtitle("Midday avg")

# There are a couple ways to do this, a simple one is with the patchwork package
# which lets you use arithmetic operators to configure your panels
# here are some examples

gg23 <- gg2 + gg3 # side by side
print(gg23)

gg32 <- gg2 / gg3 # one on top of the other
print(gg32)

gg123 <- gg23 / gg1 # 2 on top, 1 on bottom
print(gg123)


# If I wanted to save pngs or pdfs of these figures, here's how I'd do that:
filename <- paste0(plot_dir, "plot1.png") 
png(filename, width = 800, height = 400) #this one is a PNG file, dimensions are in pixels
print(gg23)
dev.off()

# Personally I think PDFs look better, but when there's a lot of data/map stuff
# the filesize can be much larger than PNG
filename <- paste0(plot_dir, "plot2.pdf")
pdf(filename, width = 12, height = 12) #this one is a PDF file, dimensions are in inches
print(gg123)
dev.off()

# That's about it for a basic start at some R intro functions!

# EOF
