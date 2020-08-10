file <- read.table("C:/Users/GDLSYSTEM/Desktop/gkd/drop_776.csv", header=TRUE, sep=",", dec=".")
file
#file$RECV_DT <- strptime(file$RECV_DT, format="%Y-%m-%d %H:%M:%S")
file <- file[!duplicated(file$RECV_DT),]
file <- na.omit(file)
colSums(is.na(file))
data <- move(x=file$LON_VAL , y=file$LAT_VAL , 
               time=as.POSIXct(file$RECV_DT), 
             data=file, proj=CRS("+proj=longlat +datum=WGS84 +no_defs"), animal=file$SHIP_ID)
#, sensor="GPS"
#"+proj=longlat +ellps=WGS84"
#+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs
##+proj=longlat +ellps=WGS84 +datum=WGS84"
##+proj=longlat +datum=WGS84 +no_defs - 그나마 잘됨
##
##
##
unique(unlist(timeLag(data, units = "mins")))
m <- align_move(m = data, res = 1, digit = 0, unit = "mins")
#timeLag(data, unit = "mins")
#unique(timestamps(data))

######################### 기존 
length(unique(timestamps(m)))
get_maptypes()
frames <- frames_spatial(m, trace_show = TRUE, equidistant = FALSE,
                         map_service = "osm", map_type = "terrain_bg")

#  frames_spatial(m, path_colours = "red",map_service = "osm", map_type = "watercolor", alpha = 0.5) 인됨
# 
length(frames) # number of frames
frames[[100]] # display one of the frames
animate_frames(frames, out_file = "example_1.gif")
frames <- frames_spatial(move_data, path_colours = "red",
                         map_service = "osm", map_type = "streets", map_res = 0.8)
animate_frames(frames, out_file = "example_1b.gif", width = 700, height = 500, res = 80)


######################### 맵박스 
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



######################### 맵 그리기
install.packages(c("leaflet", "mapview"))

library(moveVis)
library(move)

# return a mapview map
view_spatial(data)


