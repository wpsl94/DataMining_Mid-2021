#패키지 설치
#install.packages("foreign") #foreign 패키지 설치


#라이브러리
library(foreign) #SPSS 파일 불러오기
library(MASS) #반드시 먼저 실행
library(dplyr) #전처리
library(ggplot2)#시각화
library(readxl) #엑셀파일 불러오기

#데이터 불러오기
raw_welfare.2015 <- read.spss(file="Koweps_2015.sav", reencode='utf-8',
                              to.data.frame = T)
raw_welfare.2016 <- read.spss(file="Koweps_2016.sav", reencode='utf-8',
                              to.data.frame = T)
raw_welfare.2017 <- read.spss(file="Koweps_2017.sav", reencode='utf-8',
                              to.data.frame = T)
raw_welfare.2018 <- read.spss(file="Koweps_2018.sav", reencode='utf-8',
                              to.data.frame = T)
raw_welfare.2019 <- read.spss(file="Koweps_2019.sav", reencode='utf-8',
                              to.data.frame = T)
raw_welfare.2020 <- read.spss(file="Koweps_2020.sav", reencode='utf-8',
                              to.data.frame = T)

#복사본 만들기
welfare.2015 <- raw_welfare.2015
welfare.2016 <- raw_welfare.2016
welfare.2017 <- raw_welfare.2017
welfare.2018 <- raw_welfare.2018
welfare.2019 <- raw_welfare.2019
welfare.2020 <- raw_welfare.2020



