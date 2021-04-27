###3.나이-혼인율
qplot(df.wr.outlier$birth)
df.birth <- df.wr.outlier
df.birth$age <- 2020 - df.birth$birth + 1
summary(df.birth$age)

qplot(df.birth$age)
df.birth <- df.birth[,c("year", "birth","marriage", "group_marriage", "age")]
df.birth$age <- ifelse(df.birth$age > 100, NA, df.birth$age)

#설문조사에 참여한 나이별 인원 확인
df.birth.1 <- df.birth %>% 
  group_by(age) %>% 
  dplyr::summarise(total = n())

#나이별 결혼여부 확인
df.birth.rate <- df.birth %>% 
  group_by(age, group_marriage) %>% 
  dplyr::summarise(n = n())

df.birth.2 <- left_join(df.birth.1, df.birth.rate, id="age")
df.birth.2 <- df.birth.2[,c(1,3,4,2)]

df.birth.2

df.birth.rate_not_marriaged <- df.birth.rate %>% 
  filter(group_marriage == "not marriaged") %>% 
  group_by(age) %>% 
  dplyr::summarise(n = n())


df.birth.rate_marriaged <- df.birth.rate %>% 
  filter(group_marriage == "marriaged") %>% 
  group_by(age) %>% 
  dplyr::summarise(n = n())
  
#그래프 그리기
  ggplot(df.birth.2 ,aes(x=age, y=n, fill=group_marriage))+ geom_bar(stat="identity") + labs(x="나이", y="명", fill="결혼여부")  + 
    ylim(c(0,1700)) + ggtitle("나이-혼인 경험 여부 상관관계")

