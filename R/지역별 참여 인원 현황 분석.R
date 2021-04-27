###현재 조사 이전 지역별 참여 인원 현황 분석

#2015
cnt_region.2015  <- df.2015 %>%   
  group_by(region) %>% 
  tally()

#2016
cnt_region.2016  <- df.2016 %>%   
  group_by(region) %>% 
  tally()

#2017
cnt_region.2017  <- df.2017 %>%   
  group_by(region) %>% 
  tally()

#2018
cnt_region.2018  <- df.2018 %>%   
  group_by(region) %>% 
  tally()

#2019
cnt_region.2019  <- df.2019 %>%   
  group_by(region) %>% 
  tally()

#2020
cnt_region.2020  <- df.2020 %>%   
  group_by(region) %>% 
  tally()





###데이터 프레임 정리

#연도-지역별 참여 인원 데이터프레임 생성
year.r <- rep(2015, times=7)
df.r.2015<- data.frame(year.r, cnt_region.2015)
year.r <- rep(2016, times=7)
df.r.2016<- data.frame(year.r, cnt_region.2016)
year.r <- rep(2017, times=7)
df.r.2017<- data.frame(year.r, cnt_region.2017)
year.r <- rep(2018, times=7)
df.r.2018<- data.frame(year.r, cnt_region.2018)
year.r <- rep(2019, times=7)
df.r.2019<- data.frame(year.r, cnt_region.2019)
year.r <- rep(2020, times=7)
df.r.2020<- data.frame(year.r, cnt_region.2020)

df.region_cnt<- rbind(df.r.2015, df.r.2016, df.r.2017, df.r.2018, df.r.2019, df.r.2020) #데이터프레임 통함
str(df.region_cnt)

#지역별 총 참여 인원 데이터프레임 생성
total_cnt <- df.region_cnt %>% 
  select(region, n) %>% 
  arrange(region) 

result.total_cnt<- aggregate(n~region, total_cnt, sum)


##그래프 그리기
#(1) 연도-지역별 참여 인원 그래프
ggplot(df.region_cnt,
       aes(x=year.r, y=n ,colour=region, group=region))+
  geom_line()+
  geom_point(size=5, shape=19, alpha=0.5)+
  ggtitle("연간 지역별 참여인원 추이")+
  ylab("n")+
  xlab("year")+
  geom_text(data = df.region_cnt,
            aes(x=year.r, y=n+100),
            size=3,
            col="black",
            label=df.region_cnt$n)

#(2) 지역별 총 참여 인원 그래프
ggplot(result.total_cnt,
       aes(x=region, y=n, colour=region, fill=region))+
  geom_bar(stat = "identity", width = 0.5)+
  ggtitle("지역별 총 참여 인원")+
  theme(plot.title = element_text(color = "black", size = 14, face="bold"),
        axis.text.x = element_text(angle=45))


