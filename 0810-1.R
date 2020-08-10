#install.packages("moveVis")
library(moveVis)
library(move)
library(ggplot2)
library(dplyr)
library(tidyr)
library(readr)
library(purrr)
library(tibble)
library(stringr)
library(forcats)
library(raster)

df <- read_csv("C:/Users/GDLSYSTEM/2890/df_day_all.csv", col_names = TRUE) 
head(df)
iris <- subset(df, select = -c(X1,ShipName,AREA_ID,ROT_VAL,HDG_VAL, TRGT_SENSOR_KIND,Length,Beam,MSG_ID,LOC_ACCRCY))
head(iris)
df3 <- iris[,c(6,7,3,1,2,4,5)]
#t <- table(df3$SHIP_ID)
#t
df3 <- df3 %>% filter(SHIP_ID ==  440009580) # = 122542개
head(df3)
df4 <- df3[!duplicated(df3$RECV_DT),]
df4 <- na.omit(df4)
colSums(is.na(df4))
dafa <- move(x=df4$LON_VAL , y=df4$LAT_VAL , 
             time=as.POSIXct(df4$RECV_DT), 
             data=df4, proj=CRS("+proj=longlat +datum=WGS84 +no_defs"), animal=distinct(df4,SHIP_ID))

unique(unlist(timeLag(dafa, units = "mins")))
m <- align_move(m = dafa, res = 1, unit = "mins")
#timeLag(data, unit = "mins")
#unique(timestamps(data))
install.packages(c("leaflet", "mapview"))

# return a mapview map
view_spatial(dafa)
