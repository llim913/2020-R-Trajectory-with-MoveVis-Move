install.packages("moveVis")
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
iris <- subset(df, select = -c(X1,ShipName,AREA_ID,ROT_VAL,HDG_VAL, TRGT_SENSOR_KIND,Length,Beam,MSG_ID,LOC_ACCRCY))
head(iris)
df1 <- iris %>% filter(SHIP_ID ==  440022680 )
head(df1)
df1
df3 <- df1[,c(6,7,3,1,2,4,5)]

#R에서 sp package 의 proj4string(), spTransform() 함수를 이용한 좌표계 변환 
library(rgdal)
setwd('C:/Users/GDLSYSTEM/Desktop/vADM2_TM')
d <- readOGR("v시군구_TM.shp",encoding = 'utf-8')

from_crs = CRS("+proj=tmerc +lat_0=38 +lon_0=127 +k=1 +x_0=200000 +y_0=500000 +ellps=bessel +units=m")
to_crs = CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs")

proj4string(d) <- from_crs
d
d2 <- spTransform(d, to_crs)
d2
plot(d2, axes = T)

write.csv(df3, file = 'C:/Users/GDLSYSTEM/Desktop/gkd/dfd.csv')

###################### 무브 데이터 형태로 만들기
library(move)
# 안됨 myMoveObject <- move(x="C:/Users/GDLSYSTEM/Desktop/gkd/dfd.csv")
# 의미 없음 as.POSIXct(df1$RECV_DT, format="%Y-%m-%d %H:%M:%S", tz="UTC")
df1

data <- read.csv(system.file("extdata",'C:/Users/GDLSYSTEM/Desktop/gkd/dfd.csv',package="move"))
leroy <- move(x=df3$LON_VAL, y=df3$LAT_VAL , 
              time=as.POSIXct(df3$RECV_DT, format="%Y-%m-%d %H:%M:%OS", tz="UTC"), 
              proj=CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"), 
              data=df3, animal=df3$SHIP_ID)

data('C:/Users/GDLSYSTEM/Desktop/gkd/dfd.csv', package = "moveVis")



library(moveVis)
library(move)

data("move_data", package = "moveVis") # move class object
# if your tracks are present as data.frames, see df2move() for conversion

# align move_data to a uniform time scale
m <- align_move(move_data, res = 4, unit = "mins")

# create spatial frames with a OpenStreetMap watercolour map
frames <- frames_spatial(m, path_colours = c("red", "green", "blue"),
                         map_service = "osm", map_type = "watercolor", alpha = 0.5) %>%
  add_labels(x = "Longitude", y = "Latitude") %>% # add some customizations, such as axis labels
  add_northarrow() %>%
  add_scalebar() %>%
  add_timestamps(m, type = "label") %>%
  add_progress()

frames[[100]] # preview one of the frames, e.g. the 100th frame

# animate frames
animate_frames(frames, out_file = "moveVis.gif")






data(df3, package = "moveVis") # move class object
# if your tracks are present as data.frames, see df2move() for conversion

# align move_data to a uniform time scale
m <- align_move(move_data, res = 4, unit = "mins")

# create spatial frames with a OpenStreetMap watercolour map
frames <- frames_spatial(m, path_colours = c("red", "green", "blue"),
                         map_service = "osm", map_type = "watercolor", alpha = 0.5) %>%
  add_labels(x = "Longitude", y = "Latitude") %>% # add some customizations, such as axis labels
  add_northarrow() %>%
  add_scalebar() %>%
  add_timestamps(m, type = "label") %>%
  add_progress()

frames[[100]] # preview one of the frames, e.g. the 100th frame

# animate frames
animate_frames(frames, out_file = "moveVis.gif")