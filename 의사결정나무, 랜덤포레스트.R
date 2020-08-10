
library('tidyverse')
library('tidymodels')
library('broom')
library('dials')
library('ggplot2')
library('infer')
library('parsnip')
library('recipes')
library('rsample')
library('tibble')
library('tune')
library('workflows')
library('yardstick')


file <- choose.files()
file
df <- read_csv(file, col_names = T)
#df <- subset(df, select = -c(X1,RECV_DT,Length,Beam,TRGT_SENSOR_KIND,AREA_ID,SOG_VAL,MSG_ID,LOC_ACCRCY))
df <- subset(df, select = -c(X1))
head(df)



# Load the library
library(leaflet)
library('tidyverse')

# Background 1: NASA
m <- leaflet() %>% 
  addTiles() %>% 
  setView( lng = 126.740128, lat = 37.01772, zoom = 10.5) %>% 
  addProviderTiles("NASAGIBS.ViirsEarthAtNight2012")
m

pal <- colorFactor("viridis", df$stype)
leaflet(df) %>%
  setView(lng = 126.740128, lat = 37.01772, zoom = 10.5) %>%
  addProviderTiles('CartoDB.Positron') %>%
  addCircles(lng=~LON_VAL, lat=~LAT_VAL, color=~pal(stype))

install.packages('rmapshaper')
library('rmapshaper')

#df2 <- ms_simplify(df)

library('tidyverse')
library('tidymodels')


















