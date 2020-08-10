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
#iris <- subset(df, select = -c(X1,X3,X4,X7,X10,X11,X13,X14))
#iris <- iris[-1,]
iris <- subset(df, select = -c(X1,ShipName,AREA_ID,ROT_VAL,HDG_VAL, TRGT_SENSOR_KIND,COG_VAL,Length,Beam,MSG_ID,LOC_ACCRCY))
head(iris)

df <- iris[!(iris$SOG_VAL == 0 ), ]
t <- table(df$SHIP_ID)
t
df1 <- df %>% filter(SHIP_ID ==  538006801 ) # = 638개
head(df1)
df1
df3 <- df1[,c(5,6,3,2,1,4)]
df3 <- df3[-which(duplicated(df3$RECV_DT)),]
df3
write.csv(df3, file = 'C:/Users/GDLSYSTEM/Desktop/gkd/drop_77.csv')
df4 <- read_csv("C:/Users/GDLSYSTEM/Desktop/gkd/drop_77.csv", col_names = TRUE) 
df4 <- df4[!duplicated(df4$RECV_DT),]
df4 <- na.omit(df4)
colSums(is.na(df4))
write.csv(df4, file = 'C:/Users/GDLSYSTEM/Desktop/gkd/drop_776.csv')


#R에서 sp package 의 proj4string(), spTransform() 함수를 이용한 좌표계 변환 
library(rgdal)
#setwd('C:/Users/GDLSYSTEM/Desktop/vADM2_TM')
#d <- readOGR("v시군구_TM.shp",encoding = 'utf-8')

from_crs = CRS("+proj=tmerc +lat_0=38 +lon_0=127 +k=1 +x_0=200000 +y_0=500000 +ellps=bessel +units=m")
to_crs = CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs")

#proj4string(d) <- from_crs
#d
#d2 <- spTransform(d, to_crs)
#d2
#plot(d2, axes = T)



###################### 무브 데이터 형태로 만들기
library(move)
library(moveVis)
#(file$RECV_DT, format="%Y-%m-%d %H:%M:%S", tz="UTC")
#data <- move("C:/Users/GDLSYSTEM/Desktop/gkd/drop_776.csv")

## create a move object from non-Movebank data
file <- read.table("C:/Users/GDLSYSTEM/Desktop/gkd/drop_776.csv", header=TRUE, sep=",", dec=".")
file
file$RECV_DT <- strptime(file$RECV_DT, format="%Y-%m-%d %H:%M")
file <- file[!duplicated(file$RECV_DT),]
file <- na.omit(file)
colSums(is.na(file))
data <- move(x=file$LON_VAL , y=file$LAT_VAL , 
             time=as.POSIXct(file$RECV_DT, format="%Y-%m-%d %H:%M:S", tz="UTC"), 
             data=file, proj=CRS("+proj=longlat +datum=WGS84 +no_defs"), animal="SHIP_ID")
             #, sensor="GPS"
#"+proj=longlat +ellps=WGS84"
#+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs
##+proj=longlat +ellps=WGS84 +datum=WGS84"
##+proj=longlat +datum=WGS84 +no_defs
##
##
##
unique(unlist(timeLag(data, units = "mins")))
m <- align_move(m = data, res = 1, unit = "mins")
#timeLag(data, unit = "mins")
#unique(timestamps(data))

frames <- frames_spatial(m, path_colours = "red",map_service = "osm", map_type = "watercolor", alpha = 0.5)
length(frames) # number of frames
frames[[100]] # display one of the frames
animate_frames(frames, out_file = "example_1.gif")
frames <- frames_spatial(move_data, path_colours = "red",
                         map_service = "osm", map_type = "streets", map_res = 0.8)
animate_frames(frames, out_file = "example_1b.gif", width = 700, height = 500, res = 80)




library(moveVis)
library(move)

data("move_data")

# align movement to unique times and regular resolution
m <- align_move(move_data, res = 4, unit = "mins")

## assign some path colours by individual
m.list <- split(m) # split m into list by individual
m.list <- mapply(x = m.list, y = c("red", "green", "blue"), function(x, y){
  x$colour <- y
  return(x)
}) # add colour per individual
m <- moveStack(m.list) # putting it back together into a moveStack

# create frames with mapbox satellite basemap
frames <- frames_spatial(m, map_service = "mapbox", map_type = "satellite",
                         map_token = "YOUR_MAPBOX_TOKEN")
# register at http://www.mapbox.com to get a free mapbox token
# that allows you to do 50.000 map requests per month free of charge

# animate the first 100 frames as example
animate_frames(frames[1:100], out_file = "example_3a.gif")
