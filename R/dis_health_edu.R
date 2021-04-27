###본 데이터를 통해 장애정도/건강수준/교육수준/종교유무/직업/나이 - 혼인률의 관계를 알아보고자 한다.

##1. 장애정도/건강수준/교육수준
df.wr.outlier$group_marriage <- ifelse(df.wr.outlier$marriage == 1 |df.wr.outlier$marriage == 2 | df.wr.outlier$marriage == 3 | df.wr.outlier$marriage == 4, 
                                       "marriaged",
                                       NA)

df.wr.outlier$dis <- ifelse(df.wr.outlier$dis == 0 , "비장애인",
                            ifelse(df.wr.outlier$dis == 1 | df.wr.outlier$dis == 2 | df.wr.outlier$dis == 3 , "장애정도가 심한 장애인",
                                   ifelse(df.wr.outlier$dis == 4 | df.wr.outlier$dis == 5 |df.wr.outlier$dis == 6,"장애정도가 심하지 않은 장애인", "비등록 장애인")))

df.wr.outlier$health <- ifelse(df.wr.outlier$health == 1 , "아주 건강하다",
                               ifelse(df.wr.outlier$health == 2 , "건강한 편이다",
                                      ifelse(df.wr.outlier$health == 3,"보통이다",
                                             ifelse(df.wr.outlier$health == 4, "건강하지 않은 편이다", "건강이 아주 안좋다"))))

df.wr.outlier$edu <- ifelse(df.wr.outlier$edu == 2 , "무학",
                            ifelse(df.wr.outlier$edu == 3, "초졸",
                                   ifelse(df.wr.outlier$edu == 4,"중졸",
                                          ifelse(df.wr.outlier$edu == 5, "고졸",
                                                 ifelse(df.wr.outlier$edu == 6, "전문대졸",
                                                        ifelse(df.wr.outlier$edu == 7, "대졸",
                                                               ifelse(df.wr.outlier$edu == 8, "석사졸", "박사졸")))))))

table(df.wr.outlier$group_marriage)

dis_marriage <- df.wr.outlier %>% 
  filter(!is.na(group_marriage)) %>%
  dplyr::count(dis, group_marriage) %>% 
  group_by(dis) %>% 
  mutate(pct = round((n/52999*100),1))

health_marriage <- df.wr.outlier %>% 
  filter(!is.na(group_marriage)) %>%
  dplyr::count(health,group_marriage) %>% 
  group_by(health) %>% 
  mutate(pct = round((n/52999*100),1))

edu_marriage <- df.wr.outlier %>% 
  filter(!is.na(group_marriage)) %>%
  dplyr::count(edu,group_marriage) %>% 
  group_by(edu) %>% 
  mutate(pct = round((n/52999*100),1))

#그래프 그리기
dis_plot = ggplot(data=dis_marriage,
                  aes(x= dis, y=pct,  fill = dis)) +geom_col() + labs(y="%", fill="단계")  + ggtitle("장애수준-혼인률") + geom_text(aes(label=pct), position = position_stack(vjust=1))

health_plot = ggplot(data=health_marriage,
                     aes(x= health, y=pct, fill = health)) +geom_col() + labs(y="%", fill="단계")  + ggtitle("건강수준-혼인률") + geom_text(aes(label=pct), position = position_stack(vjust=1))

edu_plot = ggplot(data=edu_marriage,
                  aes(x= edu, y=pct, fill = edu)) +geom_col() + labs(y="%", fill="단계")  + ggtitle("교육수준-혼인률") + geom_text(aes(label=pct), position = position_stack(vjust=1))

grid.arrange(dis_plot, health_plot, edu_plot, ncol=1)
