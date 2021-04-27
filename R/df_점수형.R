library(foreign)
library(MASS)
library(dplyr)
library(ggplot2)
library(readxl)


###데이터 재가공(점수형으로 변환)
##2015년
df.wr.2015 <- raw_welfare.2015

df.wr.2015 <- rename(df.wr.2015,
                     sex=h10_g3,#성별
                     area=h10_reg7, #지역코드
                     birth=h10_g4, #태어난 년도
                     edu=h10_g6, #교육수준
                     religion=h10_g11, #종교
                     dis=h10_g9,#장애수준
                     marriage=h10_g10, #혼인상태
                     health=h10_med2, #건강수준
                     code_job=h10_eco9, #직종
                     income_1=h10_pers_income1, #상용근로자 소득
                     income_2=h10_pers_income2, #일용근로자 소득
                     income_3=h10_pers_income3, #자영업자 소득(농림축어업 외)
                     income_4=h10_pers_income4, #부업소득
                     income_5=h10_pers_income5) #농림축어업 소득

income <- coalesce(df.wr.2015$income_1, 
                   df.wr.2015$income_2, 
                   df.wr.2015$income_3, 
                   df.wr.2015$income_4, 
                   df.wr.2015$income_5)

df.wr.2015 <- df.wr.2015[,c("sex", "area", "birth", "edu", "religion", "dis", "marriage", "health", "code_job")]
df.wr.2015 <- cbind(df.wr.2015, income)

df.wr.2015[is.na(df.wr.2015)] <- 0 #결측치 제거

#df.wr.2015$marriage <- ifelse(df.wr.2015$marriage == 1 | df.wr.2015$marriage == 2|df.wr.2015$marriage == 3|df.wr.2015$marriage == 4 ,2, 0)


year <- rep(2015, times=nrow(df.wr.2015))
df.wr.2015 <- data.frame(year, df.wr.2015)



##2016년
df.wr.2016 <- raw_welfare.2016
df.wr.2016 <- rename(df.wr.2016,
                     sex=h11_g3,#성별
                     area=h11_reg7, #지역코드
                     birth=h11_g4, #태어난 년도
                     edu=h11_g6, #교육수준
                     religion=h11_g11, #종교
                     dis=h11_g9,#장애수준
                     marriage=h11_g10, #혼인상태
                     health=h11_med2, #건강수준
                     code_job=h11_eco9, #직종
                     income_1=h11_pers_income1, #상용근로자 소득
                     income_2=h11_pers_income2, #일용근로자 소득
                     income_3=h11_pers_income3, #자영업자 소득(농림축어업 외)
                     income_4=h11_pers_income4, #부업소득
                     income_5=h11_pers_income5) #농림축어업 소득

income <- coalesce(df.wr.2016$income_1, 
                   df.wr.2016$income_2, 
                   df.wr.2016$income_3, 
                   df.wr.2016$income_4, 
                   df.wr.2016$income_5)

df.wr.2016 <- df.wr.2016[,c("sex", "area", "birth", "edu", "religion", "dis", "marriage", "health", "code_job")]
df.wr.2016 <- cbind(df.wr.2016, income)

df.wr.2016[is.na(df.wr.2016)] <- 0 #결측치 제거
year <- rep(2016, times=nrow(df.wr.2016))
df.wr.2016 <- data.frame(year, df.wr.2016)

##2017년
df.wr.2017 <- raw_welfare.2017
df.wr.2017 <- rename(df.wr.2017,
                     sex=h12_g3,#성별
                     area=h12_reg7, #지역코드
                     birth=h12_g4, #태어난 년도
                     edu=h12_g6, #교육수준
                     religion=h12_g11, #종교
                     dis=h12_g9,#장애수준
                     marriage=h12_g10, #혼인상태
                     health=h12_med2, #건강수준
                     code_job=h12_eco9, #직종
                     income_1=h12_pers_income1, #상용근로자 소득
                     income_2=h12_pers_income2, #일용근로자 소득
                     income_3=h12_pers_income3, #자영업자 소득(농림축어업 외)
                     income_4=h12_pers_income4, #부업소득
                     income_5=h12_pers_income5) #농림축어업 소득

income <- coalesce(df.wr.2017$income_1, 
                   df.wr.2017$income_2, 
                   df.wr.2017$income_3, 
                   df.wr.2017$income_4, 
                   df.wr.2017$income_5)

df.wr.2017 <- df.wr.2017[,c("sex", "area", "birth", "edu", "religion", "dis", "marriage", "health", "code_job")]
df.wr.2017 <- cbind(df.wr.2017, income)

df.wr.2017[is.na(df.wr.2017)] <- 0 #결측치 제거
year <- rep(2017, times=nrow(df.wr.2017))
df.wr.2017 <- data.frame(year, df.wr.2017)


##2018년
df.wr.2018 <- raw_welfare.2018
df.wr.2018 <- rename(df.wr.2018,
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

income <- coalesce(df.wr.2018$income_1, 
                   df.wr.2018$income_2, 
                   df.wr.2018$income_3, 
                   df.wr.2018$income_4, 
                   df.wr.2018$income_5)

df.wr.2018 <- df.wr.2018[,c("sex", "area", "birth", "edu", "religion", "dis", "marriage", "health", "code_job")]
df.wr.2018 <- cbind(df.wr.2018, income)

df.wr.2018[is.na(df.wr.2018)] <- 0 #결측치 제거
year <- rep(2018, times=nrow(df.wr.2018))
df.wr.2018 <- data.frame(year, df.wr.2018)



##2019년
df.wr.2019 <- raw_welfare.2019
df.wr.2019 <- rename(df.wr.2019,
                     sex=h14_g3,#성별
                     area=h14_reg7, #지역코드
                     birth=h14_g4, #태어난 년도
                     edu=h14_g6, #교육수준
                     religion=h14_g11, #종교
                     dis=h14_g9,#장애수준
                     marriage=h14_g10, #혼인상태
                     health=h14_med2, #건강수준
                     code_job=h14_eco9, #직종
                     income_1=h14_pers_income1, #상용근로자 소득
                     income_2=h14_pers_income2, #일용근로자 소득
                     income_3=h14_pers_income3, #자영업자 소득(농림축어업 외)
                     income_4=h14_pers_income4, #부업소득
                     income_5=h14_pers_income5) #농림축어업 소득

income <- coalesce(df.wr.2019$income_1, 
                   df.wr.2019$income_2, 
                   df.wr.2019$income_3, 
                   df.wr.2019$income_4, 
                   df.wr.2019$income_5)

df.wr.2019 <- df.wr.2019[,c("sex", "area", "birth", "edu", "religion", "dis", "marriage", "health", "code_job")]
df.wr.2019 <- cbind(df.wr.2019, income)

df.wr.2019[is.na(df.wr.2019)] <- 0 #결측치 제거
year <- rep(2019, times=nrow(df.wr.2019))
df.wr.2019 <- data.frame(year, df.wr.2019)
