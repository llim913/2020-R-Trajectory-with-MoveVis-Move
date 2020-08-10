file <- choose.files()
file
df <- read_csv(file,col_names = T)
df <- subset(df, select = -c(X1,RECV_DT,Length,Beam,TRGT_SENSOR_KIND,AREA_ID,SOG_VAL,MSG_ID,LOC_ACCRCY)) 
head(df)



# Load the library
library(leaflet)
library('tidyverse')


# Note: if you do not already installed it, install it with:
# install.packages("leaflet")

y <- subset(df1, stype == 'craft')
# Background 1: NASA
m <- leaflet() %>% 
  addTiles() %>% 
  setView( lng = 126.740128, lat = 37.01772, zoom = 10 ) %>% 
  addProviderTiles("NASAGIBS.ViirsEarthAtNight2012")
m

## Background 2: World Imagery
#m <- leaflet() %>% 
  addTiles() %>% 
  setView( lng = 126.740128, lat = 37.01772, zoom = 10  ) %>% 
  addProviderTiles("Esri.WorldImagery")
#m
# save the widget in a html file if needed.
# library(htmlwidgets)
# saveWidget(m, file=paste0( getwd(), "/HtmlWidget/backgroundMapTile.html", width="1000px"))
pal <- colorFactor("viridis", df$stype)
leaflet(df) %>%
  setView(lng = 126.672544, lat = 37.044343, zoom = 11.3) %>%
  addProviderTiles('CartoDB.Positron') %>%
  addCircles(lng=~LON_VAL, lat=~LAT_VAL, color=~pal(stype))

op <- par(mfrow=c(4, 4))


m <- leaflet() %>% 
  addTiles() %>% 
  setView( lng =  126.702544, lat = 37.044343, zoom = 10) %>% 
  addProviderTiles("NASAGIBS.ViirsEarthAtNight2012")
m


m + 
geom_path(
   data = df, 
    aes(x = LON_VAL, y = LAT_VAL,group = SHIP_ID, colour = stype), 
    alpha = 0.5, size = 0.8) +   
  labs(
  x = "", y = "", colour = "Wind \n(m/sec)",
    title = "Typoon Trajectories by Year (1999 - 2010)"
    )

+ 
  facet_wrap(~Season) +  
  # facet_grid(~Month) + 
  theme(
    panel.background = element_rect(fill = "gray10", colour = "gray30"), 
    axis.text.x = element_blank(), 
    axis.text.y = element_blank(), 
    axis.ticks = element_blank(), 
    panel.grid.major = element_blank(), 
    panel.grid.minor = element_blank()
  ) 
















