---
title: "Data Mining(2021)-중간고사 대체 과제"
author: "72200117 Kim Hee Ju"
date: '2021 4 26 '
output:
  html_document: 
    toc: yes
    keep_md: yes
---


---
*title: "Data Mining(2021)-MidTerm Report"*
*author: "72200117 Kim Hee Ju"*
*due date: '2021 4 28 '*

------

<br>  

#### **시작하기 전에..**  

><details><summary></summary>현재 R Markdown 내부 오류 중 Knitr 라이브러리와 관련된 이슈가 발생하였습니다.Chunk가 실행이 되는 도중 Object를 찾지 못하는 에러가 발생했고, 이로 인해 현재 제 다양한 라이브러리와 참조변수가 많이 짜여져 있는 제 코드를 MArkdown v2에서는 Chunk단위로 실행시키기가 어려워 Data의 구성과 함께 설명드리며 결과를 보여드리기 불가한 점 미리 양해부탁드립니다.  <br>
>해당 이슈를 해결하기 위해해 문서를 찾아보는 중에, Markdown v1이후 kntir 패키지를 더 이상 따로 지원을 하지 않아 발생하는 오류 중 하나라고 하였습니다.   
>만약 비슷한 이슈가 있는 학생이 있거나, 솔루션을 아신다면 조언 부탁드립니다.   
>본 이슈를 찾아보면서 참고했던 링크를 함께 첨부해드립니다.  </details>

 * <https://rmarkdown.rstudio.com/authoring_migrating_from_v1.html>    
 * <https://community.rstudio.com/t/knitting-issue-object-not-found/23448>    
 * <https://github.com/yihui/knitr/issues/445>  

<br>
<br> <br>
<br>  


## **Topic : 혼인 요소에 영향을 미치는 요인 분석 및 시각화**     

> 본 과제의 데이터로 사용된 자료는 한국보건사회연구원에서 가구 경제활동을 연구해 정책 지원에 반영할 목적으로 발간하는 조사 자료인 **한국복지패널데이터**를 2015년부터 2020년, 총 6개년의 자료를 추적한 후, 이를 취합 및 가공한 것으로 사용하였였다. 이를 통해 현재 혼인과 관련된 시간적, 지역적 현황/추이를 살펴보고, 연관성이 있어 보이는 요소를 탐색하였다.  

 * 다운로드 링크   <https://www.koweps.re.kr:442/main.do>  
  
   <br>
### **1. 데이터 분석 준비 - 타입, 패키지, 라이브러리**  

<br>


| sav           | 행 개수 | 변수 개수 |
| :------------ | :------ | :-------- |
| *Koweps_2015* | 16664   | 1155      |
| *Koweps_2016* | 15989   | 1155      |
| ...           | ...     | ...       |

해당 데이터는 세 종류의 파일(SPSS, SAS, STATA)로 제공되었으며, `SPSS` 전용 파일을 선택하였다.  
이번 데이터 분석을 위해 사용된 라이브러리는 다음과 같다.  
<br>

```{r load_libraries, warning=FALSE, message=FALSE}
library("foreign") #SPSS 파일 불러오기

library("MASS") #dplyr 전처리 오류 발생 이전 반드시 실행시켜주어야 함 

library("dplyr") #전처리
library("ggplot2") #시각화
library("readxl") #엑셀불러오기 
library("descr") #관측치 탐색
library("plyr") #데이터 분할, 적용, 조합
library("gridExtra") #ggplot2 다중 분할 및 게시

```

  <br><br>
    
### **2.데이터 전처리 및 분석 절차**
<br>
해당 데이터들은 설문조사를 기반으로 한 데이터이며, 해 마다 일부 문항들에 대해 문항내용 및 답변이 바뀌기 때문에 사전에 제공되는 `Codebook`을 통해 변수를 확인/검수 해야한다. 이를 토대로 이번 시각화 및 분석에 필요한 변수들을 선별한 후, 분석 방향에 맞추어 정리한다. 

![EditCodebook](https://user-images.githubusercontent.com/19165180/116006274-7ae95b80-a645-11eb-9a19-d9629f3598e1.jpg)

<br>

이번 시각화 및 분석에 사용할 변수는 다음과 같이 선별하였다. 
<br>

```r
#2019 데이터셋 가공
welfare.2019 <- rename(welfare.2019,
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
```
<br>

여기서 소득과 같은 부분은 각각 분리가 되어 있어 병합하여 하나의 변수로 활용하기 위해 다음과 같이 작업하였다.  


```r
income <- coalesce(welfare.2019$income_1, 
                   welfare.2019$income_2, 
                   welfare.2019$income_3, 
                   welfare.2019$income_4, 
                   welfare.2019$income_5)
```

![welfare.2019](https://user-images.githubusercontent.com/19165180/116211329-a885f000-a77e-11eb-9acd-5253c6233386.jpg)

<br>해당 작업은 2015년 파일부터 총 2020년 파일까지 모두 진행해 주었으며, 2020년도엔 종교 여부를 물어보는 문항이 빠졌다는 특이사항이 있어 종교가 포함이 된 데이터셋은 따로 생성하였다.   
<br>
<br>

***
<br>
<br>

### **3.전체 데이터 시각화-hist()**
<br> 각 데이터들의 독자적인 성질을 확인하기 전에, 현재 들어있는 데이터셋을 통합해본 후, 각 변수들이 어떻게 분포하고 있는지를 확인해보았다. 우선 설문조사에 참여한 전체인원들에 대한 확인을 위해 2015~2020년 모든 데이터셋을 통합하여 히스토그램을 작성하였다. 

<br>

+ **year**  : 각 년마다 사람들이 설문조사에 참여한 인원 수를 확인할 수 있었음. 감소 추세

+ **sex** : 남(=1), 여(=2)을 나타내며, 여자가 더 많이 참여하였음.

+ **area** : 7개 광역시/도를 기준으로 참여한 인원수를 확인할 수 있었음.

+ **birth** : 출생년도에 대한 분포도를 알 수 있으며, 중장년층이 많이 참여한 설문이었음.

+ **edu, dis, health** : 교육수준/장애수준/건강수준에 대한 단계별로 응답한 분포임.

+ **religion** : 종교의 유(=1), 무(=2)를 나타내며, 가지지 않은 자가 더 많았음.

+ **marriage** : 혼인, 사별, 이혼, 미혼 등 여러 형태의 모습을 나타내고 있으며, 혼인 경험을 1회라고 가졌던 자에 한해 모두 혼인률에 포함시켰음.

+ **code_job** : 직업별 부여된 코드를 통해 어느 직군 사람들이 참여하였는지 알 수 있었음. 경제활동을 하지 않는 미성년자 및 노년층이 모두 포함되어 있기 때문에 `code_job = 0`인 `무직`상태가 가장 수가 높았음.      

  <br/>

그러나 데이터를 편집하는 중에 `Income(소득)` 데이터를 다루는 부분에서 이상치를 처리하는데에 있어 디테일한 처리가 어려웠고, `boxplot()`을 통해 분석을 하고 싶었으나 제대로된 plot이 나타나지 않아 분석이 불가하였다.   

이에 연봉에 대한 데이터는 차후 연봉을 활용할 **`'직업-연봉-혼인률 상관관계 분석'`**외에 사용하지 않을 예정이므로 법정 혼인불가 나이인 만 16세 미만의 인원을 제외한 인원들의 연봉을 히스토그램으로 나타내어 하단의 그래프로 나타내었다.   

```r
###0. 전체 히스토그램 확인

###혼인율 결정 요인 데이터 분석자료 생성

##all hist
df.wr <- rbind(df.wr.2015, df.wr.2016, df.wr.2017, df.wr.2018, df.wr.2019)
df.wr.outlier <- df.wr

#법적 혼인 불가 미성년자 제거
df.wr.outlier$birth <- ifelse(df.wr.outlier$birth > 2002 , NA, df.wr.outlier$birth) 
df.wr.outlier <- df.wr.outlier[which(!df.wr.outlier$birth %in% NA),]

#boxplot(income~year, data=df.wr.outlier, main="연도별 임금 현황")$stats #이상치 처리를 위한 통계 확인
#df.wr.outlier$income <- ifelse(df.wr.outlier$income < 0 | df.wr.outlier$income > 2500 , NA, df.wr.outlier$income)


par(mfrow=c(3,4))
for(i in 1:10){
  hist(df.wr[,i], main=colnames(df.wr)[i], col="yellow")
}

##hist Income~region
hist(df.wr$income, breaks=1000, xlim = c(-1000, 10000), col="yellow")

par(mfrow=c(1,1))


```
<br>

![Result_Hist](https://user-images.githubusercontent.com/19165180/116013040-fb6d8380-a668-11eb-8c84-806fcddfd968.jpeg)
<br>을 보았을 때, 아무리 법정 혼인불가 인원을 제했음에도 불구하고, 연 소득이 0~1000(만원)미만의 인구가 많은 까닭은, 경제활동 가능 인구에서 제외된 노년층들도 모두 포함이 되어 있기 때문이다.   

그러나 혼인률에는 이미 결혼을 했으나 이혼을 한 자, 사별을 한 자 등 현재 배우자가 없어도 혼인을 했었던 경험이 있으면 모두 포함을 시키기 때문에 해당 데이터들은 모두 유효하다고 판단하였다. 

<br><br>


### **4.연간 혼인률 추세 분석  **  

<br>

각 요인별 혼인률과의 상관관계를 분석하기 전, 현재 혼인률이 어떤 추세를 보이고 있는지 살펴보았다. 현재 2015년도부터 2020년까지 2%이내의 뚜렷한 상승폭은 아니지만, 상승하고 있는 추세를 보이고 있음을 알 수 있었다. 2019년에서 2020년 사이에 급격하게 기울기가 증가함을 알 수 있었다.  

```r
freq(df.2019$marriage) #Frequency Percent 동시에 계산해서 보여줌

freq(df.2020$marriage)
```
<center><img src="https://user-images.githubusercontent.com/19165180/116211780-234f0b00-a77f-11eb-9b29-e53fc297f83e.jpg" alt="freq" style="zoom:67%;" /></center>

<center>
<img src="https://user-images.githubusercontent.com/19165180/116211782-23e7a180-a77f-11eb-8957-baafacba4e4c.jpg" alt="df2019" style="zoom:67%;" /> 
<img src="https://user-images.githubusercontent.com/19165180/116211776-22b67480-a77f-11eb-99e2-48b54de0e0bd.jpg" alt="df2020" style="zoom:67%;" />
</center>
<br>

```r
####년도에 따른 혼인율 경향 분석

...
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

```

![Result_year](https://user-images.githubusercontent.com/19165180/116013839-2f4aa800-a66d-11eb-8189-755496a49694.jpeg)
<br><br>  

### **5.지역에 따른 시각화 및 분석**  

<br>

혼인률과의 상관성을 따지기위해 우선 가장 먼저 지역과 관련하여 시각화를 시도했다. 

지역별 데이터를 처리하기 위해 다음과 같이 전처리를 하였다. 

```r
#area : 각 지역이 현재 Code로 분류되어 있어 알아보기 쉽게 문자표기도 함께 기입

list_region <- data.frame(area = c(1:7),
                          region= c("서울",
                                    "수도권(인천/경기)",
                                    "부산/경남/울산",
                                    "대구/경북",
                                    "대전/충남",
                                    "강원/충북",
                                    "광주/전남/전북/제주도"))
list_region
welfare.2015 <- left_join(welfare.2015, list_region, id="area")
```
<br>
<br>  

#### (1) 연도-지역별 설문 참여 그래프    

각 년도마다 지역에서 참여한 인원 수를 우선 시각화 하였다. 매년 설문조사 참여 인원수가 감소추세였기 때문에, 전국적으로 인원들이 감소추세를 함께 따라가고 있는 경향을 보이고 있다. 이는 두 가지 요인으로 추정된다.

1. *가구수를 함께 조사하는 설문조사의 특성상, **출산률**의 하락으로 인한 영향*

2. *2019년 **`광주/전남/전북/제주도`**의 급격한 감소를 보이는 기울기를 토대로 보았을 때, **COVID-19** 등으로 인한 설문 조사 진행의 어려움으로 인한 영향*    

```r
ggplot(df.region_cnt,aes(x=year.r, y=n ,colour=region, group=region))+
  geom_line()+geom_point(size=5, shape=19, alpha=0.5)+
  ggtitle("연간 지역별 참여인원 추이")+
  ylab("n")+xlab("year")+
  geom_text(data = df.region_cnt,
            aes(x=year.r, y=n+100),
            size=3, col="black", label=df.region_cnt$n)
```
<br>

![Result_year.region](https://user-images.githubusercontent.com/19165180/116013803-f9a5bf00-a66c-11eb-95f4-1bbd33427d2f.jpeg)
<br>  

#### (2) 지역별 총 설문 참여 그래프 

년도와 상관없이 현재 총 설문조사에 참여한 인원의 현황을 파악하였다.   

총 참여인원의 순위는 다음과 같다.

| 1    | 수도권(인천/경기)     |
| ---- | --------------------- |
| 2    | 광주/전남/전북/제주도 |
| 3    | 부산/경남/울산        |
| 4    | 서울                  |
| 5    | 대구/경북             |
| 6    | 대전/충남             |
| 7    | 강원/충북             |

```r
ggplot(result.total_cnt,
       aes(x=region, y=n, colour=region, fill=region))+
  geom_bar(stat = "identity", width = 0.5)+
  ggtitle("지역별 총 참여 인원")+
  theme(plot.title = element_text(color = "black", size = 14, face="bold"),
        axis.text.x = element_text(angle=45))
```
<br>

![Result_tot.region](https://user-images.githubusercontent.com/19165180/116013849-3ffb1e00-a66d-11eb-8a11-90409c3e5f96.jpeg)
<br>  

#### (3) 연도-지역별 혼인률 추이 그래프    

앞서 `연도-지역별 설문 참여`데이터를 기반으로 해당 지역의 혼인률은 얼마나 되는지 확인하였다.   

단계는 다음과 같다.  <br>

우선 지역별 혼인 경험을 보유하고 있는 인원수를 모두 세어보기 위해 혼인 경험자를 필터링 하고, 이후 필터링된 데이터를 카운트 하였다. 

```R
#지역별 혼인 경험 보유 인원수 Count
ma_re.2019 <- ma_re.2019 %>% 
  group_by(region, marriage) %>% 
  select(marriage) %>% 
  filter(marriage == "marriaged") #혼인 경험자 필터링

ma_re.2019 <- ma_re.2019 %>% 
  group_by(region) %>% 
  tally() #혼인 경험자 카운트
```

<br> 이후 지역별로  혼인 문항 설문조사에 참여한 인원수를 모두 카운팅 한다. 

```R
#지역별 혼인 설문조사 참여 인원수 Count
cnt_region.2019  <- df.2019 %>%   
  group_by(region) %>% 
  tally()
```

<br> 상단에 구한 값들을 토대로 백분률을 구하여 혼인률을 산정한다. 

```r
#백분률 계산 -- round((A/B)*100,2)
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

year.m <- rep(2019, times=7)
df.m.2019<- data.frame(year.m, df.m.2019)
```

<br> 그래프를 그리면 다음과 같이 나타난다. 

```r
#그래프 그리기
ggplot(df.region_marriage,
       aes(x=year.m, y=percent ,colour=region, group=region))+
  geom_line()+
  geom_point(size=5, shape=19, alpha=0.5)+
  ggtitle("연간 지역별 혼인률 추이")+
  ylab("per")
```

![Result_rate.region](https://user-images.githubusercontent.com/19165180/116013870-5b662900-a66d-11eb-92f9-048978076ca3.jpeg)<br>

<br>  

#### (4) 지역별 혼인 경험 인구 분포    

지역별 혼인 경험 인구 분포를 지도에 `ggmap()` 과  `geom_point()`를 통해서 지도 상에 혼인률 분포를 시각화 하여 나타내 주었다.  원의 크기와 색의 명도를 통해 단계를 구분지어 주었다. 다만, 원본 데이터의 지역구분이 '시/도'단위였기 때문에 더 상세한 분포가 아닌 큰 단위로 나타내어 정확한 분포를 나타낸다고 보기는 어렵다고 판단된다.   

<br>

```r
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

df.wr.outlier.2 <- left_join(df.wr.outlier, list_region, id="area")

df.wr.outlier.2$group_marriage <- ifelse(df.wr.outlier.2$marriage == 1 |
                                         df.wr.outlier.2$marriage == 2 | 
                                         df.wr.outlier.2$marriage == 3 | 
                                         df.wr.outlier.2$marriage == 4, "marriaged",NA)
df.wr.outlier.2 <- subset(df.wr.outlier.2, df.wr.outlier.2$group_marriage == "marriaged")
df.wr.outlier.2 <- df.wr.outlier.2[,c("region", "group_marriage")]

map.region <- df.wr.outlier.2 %>% 
  filter(group_marriage == "marriaged") %>% 
  group_by(region) %>% 
  summarise(n=n())

gc <- geocode(enc2utf8(map.region$region)) # 지점의 경도위도

df.city <- data.frame(name=map.region$region, lon= gc$lon,
                      lat=gc$lat)

cen <- c(mean(df.city$lon), mean(df.city$lat))# 경도위도를 숫자로
map <- get_googlemap(center=cen,
                     maptype="roadmap", 
                     size=c(600,600), 
                     zoom = 8)#지도생성

gmap <- ggmap(map) # 지도 화면에 보이기 및 저장

df.city <- rename(df.city, c(region = "name"))
map.region <- left_join(map.region, df.city, id="region")

#그래프 그리기
gmap+ geom_point(data=map.region, aes(x=lon, y=lat, col=n,size=n, stroke=10), alpha=1) + labs(col="명")  + ggtitle("전국 혼인경험 인구 분포(7구역별)") 
```
<br>

![Result_map](https://user-images.githubusercontent.com/19165180/116013780-e72b8580-a66c-11eb-8e0b-b9250f13cb53.jpeg)
<br>
<br>  

### **6.장애/교육수준/건강상태 별 혼인률 추이**  

<br>

혼인률을 산정하는 요소 중에 **장애수준, 교육수준, 건강상태** 별 혼인률 추이를 확인하도록 하였다. 혼인률을 산정하기 위해서 법정 혼인가능 나이인 만 16세 미만인 인구는 제외한 인구들에 대해서 비율을 계산하였다. 

* 장애수준은 2015-2019 년도까지는 총 8단계로 이루어져 있으나, 장애인 등급 제도가 폐지됨에 따라 2020년 기준 4 단계로 축소되어 2020년 기준에 맞추었음.

* 건강수준은 총 5단계의 응답으로 구성되어 있음.

* 교육수준은 미취학과 무학을 동일 취급하여 총 9단계에서 8단계로 조정하였음. 


```r
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
```

<br>
![Result_RHE](https://user-images.githubusercontent.com/19165180/116013731-93b93780-a66c-11eb-876c-384a9f9e6071.jpeg)
<br><br>

### **7.종교 유무에 따른 혼인률 비교**
<br>

종교 유무에 따른 혼인률을 비교하였다.  혼인상태의 두 변수를 전처리한 후, 집단 별 빈도를 구하여 혼인 경험 보유자들에 해당하는 값만 추출하여 혼인률 표를 만들고, 이를 이용하여 그래프를 만들었다. 

```r
###2. 종교 유무에 따른 혼인율
religion_marriage <- df.wr.outlier
df.wr.outlier$group_marriage <- ifelse(df.wr.outlier$marriage == 1 |df.wr.outlier$marriage == 2 | df.wr.outlier$marriage == 3 | df.wr.outlier$marriage == 4, 
                                       "marriaged",
                                       "not marriaged")

religion_marriage <- df.wr.outlier %>% 
  filter(!is.na(group_marriage)) %>% 
  dplyr::count(religion, group_marriage) %>% 
  group_by(religion) %>% 
  mutate(pct = round(n/68733*100,1))

religion_marriage$religion <- ifelse(religion_marriage$religion == 1, "yes", "no")
religion_marriage

#그래프 그리기
ggplot(religion_marriage,
       aes(x=group_marriage, y=pct, color=religion, fill=religion))+
  geom_col(position = "dodge")
```
<br>
![Result_religion](https://user-images.githubusercontent.com/19165180/116013759-bfd4b880-a66c-11eb-825c-3509ba47918a.jpeg)
<br><br>

### **8.직업-임금-혼인유무 시각화 및 분석 **
<br>

혼인률과 직접적인 연관이 있을 것이라고 생각한 직업과 관련하여 다양한 시각화 작업을 진행해 보았다. 처음에는 임금에 따른 혼인률을 분석하려 하였으나, 임금에 대한 전처리 과정에서 이상치 분석 및 정제를 통한 결과들이 분석을 하기에 어려움이 있어 다음과 같이 시각화 및 분석 작업을 진행하였다.

> * 각 직군별 응답자 수 인사이트를 확인함
> * 각 직군별 임금(연봉) 추이를 확인함
> * 직군별 혼인률에 대한 인사이트를 확인함
> * 이를 기반으로 임금과 혼인률에 대한 상관성을 유추함

<br>

```r
##직업별 응답자 상위 10, 하위 10
job_marriage.top <- df.wr.outlier %>% 
  filter(!is.na(job) & group_marriage == "marriaged") %>% 
  group_by(job) %>% 
  dplyr::summarise(n = n()) %>% 
  #mutate(pct = round((n/n*100),1)) %>% 
  arrange(desc(n)) %>% 
  head(10)


job_marriage.bot <- df.wr.outlier %>% 
  filter(!is.na(job) & group_marriage == "marriaged") %>% 
  group_by(job) %>% 
  dplyr::summarise(n = n()) %>% 
  arrange(n) %>% 
  head(10)
```
<br>
![Result_job.vote](https://user-images.githubusercontent.com/19165180/116013899-9700f300-a66d-11eb-9e64-e3079aa61c42.jpeg)
<br>

<br>

```r
##직업-연봉 상위 10, 하위 10
job_income <- df.wr.outlier %>% 
  filter(!is.na(job)&!is.na(income)) %>% 
  group_by(job) %>% 
  dplyr::summarise(mean_income = mean(income))

head(job_income)

#top10 income-job
income.top <- job_income %>% 
  arrange(desc(mean_income)) %>% 
  head(10)

#bottom10 income-job
income.bot <- job_income %>% 
  arrange(mean_income) %>% 
  head(10)
```

<br>
![Result_income](https://user-images.githubusercontent.com/19165180/116013903-98cab680-a66d-11eb-8ac2-4398af2a2a48.jpeg)
<br>

<br>
```r
##직업-혼인율 비율 구하기

#직업별 총인원 구하기
job_total <- df.wr.outlier %>% 
  filter(!is.na(job)) %>% 
  group_by(job) %>% 
  dplyr::summarise(n=n()) %>% 
  arrange(desc(n))

job_total

#직업별 혼인경험 유/무 인원 구하기
job_marriage <- df.wr.outlier %>% 
  filter(!is.na(job)) %>% 
  group_by(job, group_marriage) %>% 
  dplyr::summarise(ans = n())

job_rate <- left_join(job_total, job_marriage, id="job")

#비율 구하기
i <- 1
no <- nrow(job_rate)
for(i in 1:no){
  job_rate$pnt[i]<- round((job_rate$ans[i]/job_rate$n[i])*100,1)
}
job_rate

fin.job_rate<- job_rate %>% 
  filter(group_marriage =="marriaged") %>% 
  group_by(job) 
  
fin.job_rate <- fin.job_rate[,c(1,5,4,2)]
fin.job_rate <- arrange(fin.job_rate, desc(pnt))

top30 <- fin.job_rate[c(1:15),]

fin.job_rate <- arrange(fin.job_rate, pnt)
bot30 <- fin.job_rate[c(1:15),]
```
<br>
![Result_job.rate](https://user-images.githubusercontent.com/19165180/116013900-98322000-a66d-11eb-8e3b-e7d97427334c.jpeg)
<br><br>

### **9. 나이-혼인 정도 시각화 및 분석**  

<br>

마지막으로 나이에 따른 혼인경험 유무에 대해 시각화를 진행하였다.  설문조사에 참여한 나이별로 인원들을 확인하였고, 이에 대해 결혼여부를 확인하였다. 이후 결혼 여부에 대한 응답자를 히스토그램으로 나타내어 두 결과를 중첩한 형태의 모습으로 제시하여 그 혼인 경험 유무의 비중이 연령대 별로 얼마나 많이 차지하고 있는지를 확인할 수 있도록 하였다.

```r
#설문조사에 참여한 나이별 인원 확인
df.birth.1 <- df.birth %>% 
  group_by(age) %>% 
  dplyr::summarise(total = n())

#나이별 결혼여부 확인
df.birth.rate <- df.birth %>% 
  group_by(age, group_marriage) %>% 
  dplyr::summarise(n = n())
```
<br>
![Result_age](https://user-images.githubusercontent.com/19165180/116013902-98322000-a66d-11eb-9f43-48be697e5319.jpeg)

<br>
<br>  

### Conclusion  
시각화를 진행하면서 다양한 데이터들을 전처리, 시각화, 분석을 진행해보았다.   

처음 분석 및 시각화 설계는 다음과 같이 진행하려고 하였다.   

<br>

* 시계열에 따른 혼인률 변화 시각화 및 분석
* 지역별 혼인율 편차 변화 시각화 및 분석
* 그렇다면 혼인율에 영향을 미치는 요인은 무엇인가?
* 각 문항별 변수간의 산점도를 기반으로 하여 상관성 분석을 통해 혼인율에 가장 큰 영향을 미치는 요인을 알아보고  이와 관련하여 특정 변수에 대한 추가적인 시각화를 진행 

<br>그러나 생각보다 단순히 `pairs()`함수를 사용한다고 해서, 그리고 위에서 일부 언급한 것과 같이 임금과 같은 변수의 경우 이상치 처리가 까다로워 분석을 할 수 있는 상관관계를 나타내고 있는 수치 혹은 그래프가 나타나지 않았다. 

![pairs_fail](https://user-images.githubusercontent.com/19165180/116204161-6dcc8980-a777-11eb-8552-0764da1f4064.png)



특히 임금`Income`을 시각화하는 것에 가장 어려움을 겪었다. 각 년도별 임금에 대해서 혼인 불가 연령을 제외한 상태에서 이상치 분석 및 `boxplot()`을 실행시켜 보았는데, 다음과 같은 결과가 나왔다. 

<center><img src="https://user-images.githubusercontent.com/19165180/116204163-6efdb680-a777-11eb-810f-19fcd89ae0a0.png" style="zoom:75%;" /></center>

<center>
<img width="234" alt="income_box_fail2" src="https://user-images.githubusercontent.com/19165180/116204164-6efdb680-a777-11eb-8e77-53d912f8e158.png">
<img width="480" alt="income_box_fail" src="https://user-images.githubusercontent.com/19165180/116204166-6f964d00-a777-11eb-9821-6a16a17fd1e8.png">
</center>
`pairs()`를 통해서 판단하였을 때, 회귀분석 등의 솔루션도 현재 데이터의 상태로는 무의미할 것으로 판단이 되고, 아직 정제를 어떤 방향으로 해야할지, 어떤 기법들을 써야할지 아직 방향을 잡지 못하여 이번에 R을 처음 접하면서 현재 할 수 있는 것들을 기반으로 진행을 해 보았다.  

다양한 시각화 방법을 통해 현재 주어진 데이터 셋 내에서 조합할 수 있는 시각화를 진행해 보았으며, 이를 토대로 확인한 결과 다음과 같은 결과를 도출해 내었다.   <br>

> * 혼인률 자체는 증가를 하는 추세이나, 현재 혼인률을 이혼, 사별 등의 혼인 경험 여부를 모두 포함하였기 때문에 혼인이 지속되어 가족 구성원이 이루어지는지 여부는 알 수 없음
> * 전반적으로 설문 참여 인원이 줄어드는 추세이며, 젊은 층 보다 노년층의 인구 분포가 더 많이 분포해 있어 혼인률이 높게 잡혀있을 수 있어, 각 연령대의 표본이 동일해야 정확한 통계가 나올것으로 추정
> * 직군별 모집단의 수도 편차가 커서 혼인률을 책정하기 어렵다고 판단하나, 본 시각화에 따르면 소득수준이 높은 편일수록 대체로 혼인률이 낮고, 소득수준이 낮은 직군이 혼인률이 높다고 나타남
> * 종교를 가지고 있는 집단이 가지고 있지 않은 집단에 비해 혼인률이 높은 것으로 확인 됨
> * 지역별 혼인률 편차가 수도권에 비해 비수도권지역이 월등히 높은 것으로 확인됨
>   * 지역별 참여인원이 높은 곳이 상대적으로 혼인률이 낮게 계산된 것으로 보임. 추가적으로 직군-지역별 상관성을 따졋을 때 **`지역-직군-임금`**에 따른 상관관계가 나타난다면 더욱 뚜렷한 분석이 가능할 것으로 확인됨
> * 혼인에 있어 건강수준, 장애수준은 '건강한편/비장애인'의 경우가 월등히 높았으며, 교육수준은 정비례하지는 않았음.
>   * **교육수준-직군-임금-지역** 간의 상관관계성을 따져 보고 혼인률을 살펴보면 더욱 상세한 결과가 나올 것 이라고 판단이 됨<br>

<br>

<br>

#### R codes 

본 과제의 소스코드는 `Github`에 모두 공개되어 있으며, 소스코드 및 사용한 데이터셋을 모두 확인할 수 있습니다.

   <br>

* **Github URL : <https://github.com/wpsl94/DataMining_Mid-2021>**

  <br>

  <br>

  <br>

  <br>

  <br>

  