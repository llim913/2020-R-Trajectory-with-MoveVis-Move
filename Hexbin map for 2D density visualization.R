# Please Ignore, specific to a bug in the gallery
library(pacman)
pacman::p_unload(pacman::p_loaded(), character.only = TRUE)

# Load libraries
library(dplyr)        # data wrangling
library(cartogram)    # for the cartogram
library(ggplot2)      # to realize the plots
library(broom)        # from geospatial format to data frame
library(tweenr)       # to create transition dataframe between 2 states
library(gganimate)    # To realize the animation
library(maptools)     # world boundaries coordinates
library(viridis)      # for a nice color palette


file <- choose.files()
file
df <- read_csv(file,col_names = T)
df <- subset(df, select = -c(X1,RECV_DT,Length,Beam,TRGT_SENSOR_KIND,AREA_ID,SOG_VAL,MSG_ID,LOC_ACCRCY)) 
head(df)

# Get the shape file of Africa
data(wrld_simpl)
asia=wrld_simpl[wrld_simpl$REGION=="Asia",]

# A basic representation
plot(asia)

install.packages(c("rworldmap", "rworldxtra", "RColorBrewer",
                   "maptools", "classInt"))
## Load packages
library('rworldmap')
library('rworldxtra')
library('RColorBrewer')
library('maptools')
library('classInt')


#worldmap <- getMap(resolution = "high")
#dim(worldmap)

# Libraries
library(tidyverse)
library(viridis)
library(hrbrthemes)
library(mapdata)

# Load dataset from github
data <- read.table("https://raw.githubusercontent.com/holtzy/data_to_viz/master/Example_dataset/17_ListGPSCoordinates.csv", sep=",", header=T)

# Get the world polygon
world <- map_data("world")

# plot.
ggplot(df, aes(x=LON_VAL, y=LAT_VAL)) + 
  geom_polygon(data = world, aes(x=long, y = lat, group = group), fill="grey", alpha=0.3) +
  geom_bin2d(bins=100) +
  ggplot2::annotate("text", x = 175, y = 80, label="type", colour = "black", size=4, alpha=1, hjust=1) +
  ggplot2::annotate("segment", x = 100, xend = 175, y = 73, yend = 73, colour = "black", size=0.2, alpha=1) +
  theme_void() +
  ylim(-70, 80) +
  scale_fill_viridis(
    trans = "log", 
    breaks = c(1,7,54,403,3000),
    name="stype", 
    guide = guide_legend( keyheight = unit(2.5, units = "mm"), keywidth=unit(10, units = "mm"), label.position = "bottom", title.position = 'top', nrow=1) 
  )  +
  ggtitle( "" ) +
  theme(
    legend.position = c(0.8, 0.09),
    legend.title=element_text(color="black", size=8),
    text = element_text(color = "#22211d"),
    plot.title = element_text(size= 13, hjust=0.1, color = "#4e4d47", margin = margin(b = -0.1, t = 0.4, l = 2, unit = "cm")),
  ) 



boxLocation<-c(126.474704, 36.902590, 126.910740, 37.205617)
