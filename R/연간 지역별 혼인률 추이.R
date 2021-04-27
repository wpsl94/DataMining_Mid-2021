####년간 지역별 혼인률 추이

#데이터 불러오기
ma_re.2015 <- df.2015[,c("marriage","region")]
ma_re.2016 <- df.2016[,c("marriage","region")]
ma_re.2017 <- df.2017[,c("marriage","region")]
ma_re.2018 <- df.2018[,c("marriage","region")]
ma_re.2019 <- df.2019[,c("marriage","region")]
ma_re.2020 <- df.2020[,c("marriage","region")]


##각 지역별 혼인경험 보유 인원수 카운트 - (A)
#2015
ma_re.2015 <- ma_re.2015 %>% 
  group_by(region, marriage) %>% 
  select(marriage) %>% 
  filter(marriage == "marriaged") #혼인 경험자 필터링

ma_re.2015 <- ma_re.2015 %>% 
  group_by(region) %>% 
  tally() #혼인 경험자 카운트

#2016
ma_re.2016 <- ma_re.2016 %>% 
  group_by(region, marriage) %>% 
  select(marriage) %>% 
  filter(marriage == "marriaged") #혼인 경험자 필터링

ma_re.2016 <- ma_re.2016 %>% 
  group_by(region) %>% 
  tally() #혼인 경험자 카운트

#2017
ma_re.2017 <- ma_re.2017 %>% 
  group_by(region, marriage) %>% 
  select(marriage) %>% 
  filter(marriage == "marriaged") #혼인 경험자 필터링

ma_re.2017 <- ma_re.2017 %>% 
  group_by(region) %>% 
  tally() #혼인 경험자 카운트

#2018
ma_re.2018 <- ma_re.2018 %>% 
  group_by(region, marriage) %>% 
  select(marriage) %>% 
  filter(marriage == "marriaged") #혼인 경험자 필터링

ma_re.2018 <- ma_re.2018 %>% 
  group_by(region) %>% 
  tally() #혼인 경험자 카운트

#2019
ma_re.2019 <- ma_re.2019 %>% 
  group_by(region, marriage) %>% 
  select(marriage) %>% 
  filter(marriage == "marriaged") #혼인 경험자 필터링

ma_re.2019 <- ma_re.2019 %>% 
  group_by(region) %>% 
  tally() #혼인 경험자 카운트

#2020
ma_re.2020 <- ma_re.2020 %>% 
  group_by(region, marriage) %>% 
  select(marriage) %>% 
  filter(marriage == "marriaged") #혼인 경험자 필터링

ma_re.2020 <- ma_re.2020 %>% 
  group_by(region) %>% 
  tally() #혼인 경험자 카운트




##지역별 설문조사 참여 인원수 카운트 - (B)
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




##백분률 계산 -- round((A/B)*100,2)
#2015
df.m.2015 <- cbind(ma_re.2015,cnt_region.2015)
df.m.2015 <- df.m.2015[,c(1,2,4)]
df.m.2015 <- rename(df.m.2015,
                    c(n = "marriaged",
                      n.1 = "cnt"))
df.m.2015 <- data.frame(df.m.2015, percent = NA)

i <- 1
for(i in 1:7){
  df.m.2015$percent[i]<- (df.m.2015$marriaged[i]/df.m.2015$cnt[i])*100
  df.m.2015$percent<- round(df.m.2015$percent,2)
  i <- i+1
}

#2016
df.m.2016 <- cbind(ma_re.2016,cnt_region.2016)
df.m.2016 <- df.m.2016[,c(1,2,4)]
df.m.2016 <- rename(df.m.2016,
                    c(n = "marriaged",
                      n.1 = "cnt"))
df.m.2016 <- data.frame(df.m.2016, percent = NA)

i <- 1
for(i in 1:7){
  df.m.2016$percent[i]<- (df.m.2016$marriaged[i]/df.m.2016$cnt[i])*100
  df.m.2016$percent<- round(df.m.2016$percent,2)
  i <- i+1
}

#2017
df.m.2017 <- cbind(ma_re.2017,cnt_region.2017)
df.m.2017 <- df.m.2017[,c(1,2,4)]
df.m.2017 <- rename(df.m.2017,
                    c(n = "marriaged",
                      n.1 = "cnt"))
df.m.2017 <- data.frame(df.m.2017, percent = NA)

i <- 1
for(i in 1:7){
  df.m.2017$percent[i]<- (df.m.2017$marriaged[i]/df.m.2017$cnt[i])*100
  df.m.2017$percent<- round(df.m.2017$percent,2)
  i <- i+1
}

#2018
df.m.2018 <- cbind(ma_re.2018,cnt_region.2018)
df.m.2018 <- df.m.2018[,c(1,2,4)]
df.m.2018 <- rename(df.m.2018,
                    c(n = "marriaged",
                      n.1 = "cnt"))
df.m.2018 <- data.frame(df.m.2018, percent = NA)

i <- 1
for(i in 1:7){
  df.m.2018$percent[i]<- (df.m.2018$marriaged[i]/df.m.2018$cnt[i])*100
  df.m.2018$percent<- round(df.m.2018$percent,2)
  i <- i+1
}

#2019
df.m.2019 <- cbind(ma_re.2019,cnt_region.2019)
df.m.2019 <- df.m.2019[,c(1,2,4)]
df.m.2019 <- rename(df.m.2019,
                    c(n = "marriaged",
                      n.1 = "cnt"))
df.m.2019 <- data.frame(df.m.2019, percent = NA)

i <- 1
for(i in 1:7){
  df.m.2019$percent[i]<- (df.m.2019$marriaged[i]/df.m.2019$cnt[i])*100
  df.m.2019$percent<- round(df.m.2019$percent,2)
  i <- i+1
}

#2020
df.m.2020 <- cbind(ma_re.2020,cnt_region.2020)
df.m.2020 <- df.m.2020[,c(1,2,4)]
df.m.2020 <- rename(df.m.2020,
                    c(n = "marriaged",
                      n.1 = "cnt"))
df.m.2020 <- data.frame(df.m.2020, percent = NA)

i <- 1
for(i in 1:7){
  df.m.2020$percent[i]<- (df.m.2020$marriaged[i]/df.m.2020$cnt[i])*100
  df.m.2020$percent<- round(df.m.2020$percent,2)
  i <- i+1
}



###데이터 프레임 정리
year.m <- rep(2015, times=7)
df.m.2015<- data.frame(year.m, df.m.2015)
year.m <- rep(2016, times=7)
df.m.2016<- data.frame(year.m, df.m.2016)
year.m <- rep(2017, times=7)
df.m.2017<- data.frame(year.m, df.m.2017)
year.m <- rep(2018, times=7)
df.m.2018<- data.frame(year.m, df.m.2018)
year.m <- rep(2019, times=7)
df.m.2019<- data.frame(year.m, df.m.2019)
year.m <- rep(2020, times=7)
df.m.2020<- data.frame(year.m, df.m.2020)

df.region_marriage<- rbind(df.m.2015, df.m.2016, df.m.2017, df.m.2018, df.m.2019, df.m.2020) #데이터프레임 통함


#그래프 그리기
ggplot(df.region_marriage,
       aes(x=year.m, y=percent ,colour=region, group=region))+
  geom_line()+
  geom_point(size=5, shape=19, alpha=0.5)+
  ggtitle("연간 지역별 혼인률 추이")+
  ylab("per")

