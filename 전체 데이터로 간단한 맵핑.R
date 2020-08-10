library(ggmap)
library(ggplot2)
library(dplyr)
library(tibble)
library(ggplot2)
# 경도와위도로표시할사각형의좌하, 우상의좌표
boxLocation<-c(126.474704, 36.902590, 126.910740, 37.205617)
# stamen에서 maptype="terrain"으로지도다운로드
krMap<-get_map(location=boxLocation, source="stamen", maptype="terrain")
map <- ggmap(krMap)


#데이터 불러오기 
#shipinfo에서 mmsi가 같은것만 가져와 추린 데이터
file <- choose.files()
file
df <- read.csv(file,header = T)
head(df)
str(df)

df2 = tbl_df(df)
df2
#유효하지 않은 대지속력 데이터 삭제
df2 <- df[!(df2$SOG_VAL == 1023),]
#선수방향 지시불용 데이터 삭제
df2 <- df[!(df2$HDG_VAL == 511),]

map + geom_path(data = df2,aes(x = LON_VAL, y = LAT_VAL, group = SHIP_ID, color = ShipType),
                alpha = 0.5, size = 0.8) + 
  labs(x = "", y = "", colour = "Wind \n(m/sec)",
       title = "ship_id$shiptype") +
  theme(panel.background = element_rect(fill = "gray10", colour = "gray30"), 
        axis.text.x = element_blank(), 
        axis.text.y = element_blank(), axis.ticks = element_blank(), 
        panel.grid.major = element_blank(), panel.grid.minor = element_blank())


ggmap(krMap) +geom_point(data=df2, aes(x = LON_VAL, y = LAT_VAL,  group = SHIP_ID, color = ShipType, size=ShipType))










#사용할 라이브러리
library(ggmap)
library(ggplot2)
library(dplyr)
library(tibble)
library(ggplot2)

# 경도와 위도로 표시할 사각형의 좌하, 우상 좌표
boxLocation<-c(127, 36, 126, 37)
# stamen에서 maptype="terrain"으로지도다운로드
krMap<-get_map(location=boxLocation, source="stamen", maptype="terrain")
map <- ggmap(krMap)




#데이터 불러오기 
file <- choose.files()
file
df <- read.csv(file,header = T)
head(df)
str(df)

df1 = tbl_df(df)
df1
#유효하지 않은 값의 데이터 삭제
df1 <- df[!(df1$지울컬럼 == 지울값),]
#지도 그리기
map + geom_path(data = df1,aes(x = 경도, y = 위도, group = 묶을컬럼, 
                               color =컬럼이나 색을 주면 됨),
                alpha = 0.5, size = 0.8) + 
  labs(x = "", y = "", colour = "Wind \n(m/sec)",
       title = "제목") +
  theme(panel.background = element_rect(fill = "gray10", colour = "gray30"), 
        axis.text.x = element_blank(), 
        axis.text.y = element_blank(), axis.ticks = element_blank(), 
        panel.grid.major = element_blank(), panel.grid.minor = element_blank())


#간단한 버전
ggmap(krMap) +geom_point(data=df1, aes(x = 경도, y = 위도,  group = 묶을컬럼, 
                                       color = 색이나 컬럼, size=숫자나 원하는 컬럼(int)))
















  