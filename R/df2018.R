library(foreign)
library(dplyr)
library(ggplot2)
library(readxl)

#2018 데이터셋 가공
welfare.2018 <- rename(welfare.2018,
                       sex=h13_g3,#성별
                       area=h13_reg7, #지역코드
                       birth=h13_g4, #태어난 년도
                       edu=h13_g6, #교육수준
                       religion=h13_g11, #종교
                       dis=h13_g9,#장애수준
                       marriage=h13_g10, #혼인상태
                       health=h13_med2, #건강수준
                       code_job=h13_eco9, #직종
                       income_1=h13_pers_income1, #상용근로자 소득
                       income_2=h13_pers_income2, #일용근로자 소득
                       income_3=h13_pers_income3, #자영업자 소득(농림축어업 외)
                       income_4=h13_pers_income4, #부업소득
                       income_5=h13_pers_income5) #농림축어업 소득

income <- coalesce(welfare.2018$income_1, 
                   welfare.2018$income_2, 
                   welfare.2018$income_3, 
                   welfare.2018$income_4, 
                   welfare.2018$income_5)

welfare.2018 <- welfare.2018[,c("sex", "area", "birth", "edu", "religion", "dis", "marriage", "health", "code_job")]
welfare.2018 <- cbind(welfare.2018, income)

welfare.2018[is.na(welfare.2018)] <- 0 #결측치 제거

#sex option rename
welfare.2018$sex <- ifelse(welfare.2018$sex ==1, "male", "female")
table(welfare.2018$sex)
qplot(welfare.2018$sex)

#marriage option revise
welfare.2018$marriage <- ifelse(welfare.2018$marriage == 0 |welfare.2018$marriage == 5 | welfare.2018$marriage == 6, "not marriaged", "marriaged")
table(welfare.2018$marriage)
qplot(welfare.2018$marriage)

#area
list_region <- data.frame(area = c(1:7),
                          region= c("서울",
                                    "수도권(인천/경기)",
                                    "부산/경남/울산",
                                    "대구/경북",
                                    "대전/충남",
                                    "강원/충북",
                                    "광주/전남/전북/제주도"))
list_region
welfare.2018 <- left_join(welfare.2018, list_region, id="area")

welfare.2018 %>%
  head


#edu
welfare.2018$edu <- ifelse(welfare.2018$edu == 1,2, welfare.2018$edu)
table(welfare.2018$edu)
list_edu <- data.frame(edu = c(2:9),
                       edu_lev= c("무학",
                                  "초졸",
                                  "중졸",
                                  "고졸",
                                  "전문대졸",
                                  "대졸",
                                  "석사졸",
                                  "박사졸"))
list_edu
welfare.2018 <- left_join(welfare.2018, list_edu, id="edu")

welfare.2018 %>%
  head

#disorder_장애수준
table(welfare.2018$dis)
View(welfare.2018)
list_dis <- data.frame(dis = c(0:7),
                       disorder = c("비장애인",
                                    "장애정도가 심한 장애인",
                                    "장애정도가 심한 장애인",
                                    "장애정도가 심한 장애인",
                                    "장애정도가 심하지 않은 장애인",
                                    "장애정도가 심하지 않은 장애인",
                                    "장애정도가 심하지 않은 장애인",
                                    "비등록장애인"))
list_dis
welfare.2018 <- left_join(welfare.2018, list_dis, id="dis")

welfare.2018 %>%
  head
table(welfare.2018$disorder)


#health optionS
list_health <- data.frame(health = c(1:5),
                          condition = c("아주 건강하다",
                                        "건강한 편이다",
                                        "보통이다",
                                        "건강하지 않은 편이다",
                                        "건강이 아주 안 좋다"))
list_health
welfare.2018 <- left_join(welfare.2018, list_health, id="health")

welfare.2018 %>%
  head
table(welfare.2018$condition)



#religion
#sex option rename
welfare.2018$religion <- ifelse(welfare.2018$religion ==1, "yes", "no")
table(welfare.2018$religion)
qplot(welfare.2018$religion)



#result DF
df.2018 <- welfare.2018[,c(1,3,7,9:14)]
View(df.2018)

df.2018_religion <- welfare.2015[,c(1,3,5,7,9:14)]
View(df.2018_religion)