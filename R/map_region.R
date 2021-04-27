###4.지도에 혼인 인구 표시

register_google(key='AIzaSyCovjI0rxava9Kchw7aHcos03VtaV_zOO8') # 부여받은 키 등록

df.wr.outlier
#area
list_region <- data.frame(area = c(1:7),
                          region= c("서울특별시",
                                    "경기도",
                                    "경상남도",
                                    "경상북도",
                                    "충청남도",
                                    "충청북도",
                                    "전라남도"))
list_region

df.wr.outlier.2 <- left_join(df.wr.outlier, list_region, id="area")

df.wr.outlier.2$group_marriage <- ifelse(df.wr.outlier.2$marriage == 1 |df.wr.outlier.2$marriage == 2 | df.wr.outlier.2$marriage == 3 | df.wr.outlier.2$marriage == 4, 
                                       "marriaged",
                                       NA)
df.wr.outlier.2 <- subset(df.wr.outlier.2, df.wr.outlier.2$group_marriage == "marriaged")
df.wr.outlier.2 <- df.wr.outlier.2[,c("region", "group_marriage")]
map.region <- df.wr.outlier.2 %>% 
  filter(group_marriage == "marriaged") %>% 
  group_by(region) %>% 
  dplyr::summarise(n=n())

gc <- geocode(enc2utf8(map.region$region)) # 지점의 경도위도
gc

df.city <- data.frame(name=map.region$region, lon= gc$lon,
                      lat=gc$lat)
df.city

cen <- c(mean(df.city$lon), mean(df.city$lat))# 경도위도를 숫자로
map <- get_googlemap(center=cen,
                     maptype="roadmap", 
                     size=c(600,600), 
                     zoom = 7)#지도생성

gmap <- ggmap(map) # 지도 화면에 보이기 및 저장

df.city <- rename(df.city, c(name = "region"))
map.region <- left_join(map.region, df.city, id="region")

#그래프 그리기
gmap+ geom_point(data=map.region, aes(x=lon, y=lat, col=n,size=n, stroke=10), alpha=1) + labs(col="명")  + ggtitle("전국 혼인경험 인구 분포(7구역별)") 