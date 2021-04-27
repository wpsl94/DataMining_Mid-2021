####년도에 따른 혼인율 경향 분석

#백분률 확인하기
freq(df.2015$marriage)
freq(df.2016$marriage)
freq(df.2017$marriage)
freq(df.2018$marriage)
freq(df.2019$marriage)
freq(df.2020$marriage)


per <- c(68.07, 68.32, 68.53, 68.7, 68.74, 69.26)

#그래프 그리기

year <- 2015:2020 
df.marriage <- data.frame(year,per)

ggplot(data=df.marriage,
       aes(x=year,
           y=per))+
  ggtitle("년도별 혼인률 추이")+
  geom_line()+
  geom_point(size=7, shape=19, alpha=1.0, col="red")+
  geom_text(data=df.marriage,
            aes(x=year,
                y=per+0.08),
            size=3,
            label=per)

