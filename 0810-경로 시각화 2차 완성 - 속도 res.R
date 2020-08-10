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
#--------------------------------------전처리
file <- read_csv("C:/Users/GDLSYSTEM/2890/df_day_all.csv", col_names = TRUE) 
head(file)
file <- subset(file, select = -c(X1,ShipName,AREA_ID,ROT_VAL,HDG_VAL, TRGT_SENSOR_KIND,Length,Beam,MSG_ID,LOC_ACCRCY))
head(file)
#df1 <- file[c('249025000','311087000','312483000','351034000'),]
df1 <- file %>% filter( SHIP_ID ==  249025000 | SHIP_ID == 351034000| SHIP_ID ==311087000| SHIP_ID ==312483000)#
df <- df1 %>%
  select(LON_VAL,LAT_VAL,RECV_DT,SHIP_ID)
df <- df[!duplicated(df$RECV_DT),]
df <- na.omit(df)
colSums(is.na(df))
#-------------------------------MoveStack 형태로 데이터 바꿔주기
m <- move(x = df$LON_VAL, y = df$LAT_VAL,
          time = df$RECV_DT, animal = df$SHIP_ID,
          proj = "+proj=longlat +datum=WGS84 +no_defs",
          removeDuplicatedTimestamps = FALSE)

lag <- unlist(timeLag(m, unit = "mins"))
median(lag)
sd(lag)
m <- align_move(m, res = 1, digit = 0, unit = "mins")
length(unique(timestamps(m)))
#--------------------------------------하나의 리스트 형태로 만들기(필요한 맵 디자인 고르기)
frames <- frames_spatial(m, trace_show = TRUE, equidistant = FALSE,
                         map_service = "osm", map_type = "terrain_bg")
frames[[3400]]
#--------------------------------------만들어질 시각화에 라벨 부여해주기
frames <- frames %>% add_labels(title = "선박 운용 2019/05~", caption = "Trajectory data: Cheng et al.
    (2019); Fiedler et al. (2013-2019),doi:10.5441/001/1.ck04mn78 Map:
    OpenStreetMap/Stamen; Projection: Geographic, WGS84", 
                                x = "Longitude", y = "Latitude") %>%
  add_timestamps(type = "label") %>% 
  add_progress(colour = "white") %>%
  add_northarrow(colour = "white", position = "bottomleft") %>% 
  add_scalebar(colour = "black", position = "bottomright",
               distance = 600)
#---------------------------------------시각화시키고 기본저장 폴더에 저장하기
animate_frames(frames, width = 800, height = 800,
               out_file = "5차.MOV", end_pause = 1)


sp <- as.data.frame(installed.packages()[,c(1,3,4)])
rownames(sp) <- NULL
sp <- sp[is.na(sp$Priority),1:2,drop=FALSE]
print(sp,row.names=FALSE)
