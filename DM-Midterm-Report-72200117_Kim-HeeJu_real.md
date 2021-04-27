---
title: "Data Mining(2021)-중간고사 대체 과제"
author: "72200117 Kim Hee Ju"
date: '2021 4 26 '
output:
  html_document: 
    toc: yes
    keep_md: yes
  word_document: default
---


​    


---
<br>  
    
## **전달드릴 말씀**  

>현재 R Markdown 내부 오류 중 **Knitr 라이브러리**와 관련된 이슈로 인해 `**Chunk가 실행이 되는 도중 Object를 찾지 못하는 에러**`가 발생하고 있습니다.이로 인해 현재 코드를 Chunk단위로 실행시켜 Data의 구성과 함께 설명드리며 결과를 보여드리려고 했으나, 불가한 점 미리 양해부탁드립니다.해당 이슈는 어떻게 해결할 수 있는지 문서를 찾아보는 중인데 아직 찾지 못하였습니다. 만약 비슷한 이슈가 있는 학생이 있거나, 솔루션을 아신다면 조언 부탁드립니다. 본 이슈를 찾아보면서 참고했던 링크를 함께 첨부해드립니다.  

 * <https://community.rstudio.com/t/knitting-issue-object-not-found/23448>    
 * <https://github.com/yihui/knitr/issues/445>  

<br>
<br>  

<br>  


## **Topic : 혼인 요소에 영향을 미치는 요인 분석 및 시각화 **


​       
> 본 과제의 데이터로 사용된 자료는 한국보건사회연구원에서 가구 경제활동을 연구해 정책 지원에 반영할 목적으로 발간하는 조사 자료인 **한국복지패널데이터**를 2015년부터 2020년, 총 6개년의 자료를 추적한 후, 이를 취합 및 가공한 것으로 사용하였였다. 이를 통해 현재 혼인과 관련된 시간적, 지역적 현황/추이를 살펴보고, 연관성이 있어 보이는 요소를 탐색하였다.  

**다운로드 링크 <https://www.koweps.re.kr:442/main.do>  **

   <br>

### **1. 데이터 분석 준비 - 타입, 패키지, 라이브러리**


  sav | 행 개수 | 변수 개수  
  :-----|:-----|:-----
  *Koweps_2015* | 16664 | 1155
  *Koweps_2016* | 15989 | 1155
  ...  |...|...


해당 데이터는 세 종류의 파일(SPSS, SAS, STATA)로 제공되었으며, `SPSS` 전용 파일을 선택하였다.  
이번 데이터 분석을 위해 사용된 라이브러리는 다음과 같다.  
<br>


```r
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

해당 작업은 2015년 파일부터 총 2020년 파일까지 모두 진행해 주었으며, 2020년도엔 종교 여부를 물어보는 문항이 빠졌다는 특이사항이 있었다. 
<br>
<br>

***
<br>
<br>

### **3.전체 데이터 시각화-hist()**
<br>
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
<br>
<br>


### **4.연간 혼인률 추세 분석  **  

<br>
```r
```
<br>
![Result_year](https://user-images.githubusercontent.com/19165180/116013839-2f4aa800-a66d-11eb-8189-755496a49694.jpeg)
<br>  

### **5.지역에 따른 시각화 및 분석**  

<br>
```r
```
<br>
<br>  

#### (1) 연도-지역별 설문 참여 그래프  

```r
```
<br>
![Result_year.region](https://user-images.githubusercontent.com/19165180/116013803-f9a5bf00-a66c-11eb-95f4-1bbd33427d2f.jpeg)
<br>  

#### (2) 지역별 총 설문 참여 그래프  

```r
```
<br>
![Result_tot.region](https://user-images.githubusercontent.com/19165180/116013849-3ffb1e00-a66d-11eb-8a11-90409c3e5f96.jpeg)
<br>  

#### (3) 연도-지역별 혼인률 추이 그래프  

```r
```
<br>
![Result_rate.region](https://user-images.githubusercontent.com/19165180/116013870-5b662900-a66d-11eb-92f9-048978076ca3.jpeg)
<br>  

#### (4) 지역별 혼인률 분포  

```r
```
<br>
![Result_map](https://user-images.githubusercontent.com/19165180/116013780-e72b8580-a66c-11eb-8e0b-b9250f13cb53.jpeg)
<br>
<br>  

### **6.장애/교육수준/건강상태 별 혼인률 추이**  

<br>
```r
```
<br>
![Result_RHE](https://user-images.githubusercontent.com/19165180/116013731-93b93780-a66c-11eb-876c-384a9f9e6071.jpeg)
<br>

### **7.종교 유무에 따른 혼인률 비교**
<br>
```r
```
<br>
![Result_religion](https://user-images.githubusercontent.com/19165180/116013759-bfd4b880-a66c-11eb-825c-3509ba47918a.jpeg)
<br>

### **8.직업-혼인유무 시각화 및 분석 **
<br>

<br>
```r
```
<br>
![Result_job.vote](https://user-images.githubusercontent.com/19165180/116013899-9700f300-a66d-11eb-9e64-e3079aa61c42.jpeg)
<br>

<br>
```r
```
<br>
![Result_job.rate](https://user-images.githubusercontent.com/19165180/116013900-98322000-a66d-11eb-8e3b-e7d97427334c.jpeg)
<br>  

### **9.직업-임금 시각화 및 분석**  

<br>
```r
```
<br>
![Result_income](https://user-images.githubusercontent.com/19165180/116013903-98cab680-a66d-11eb-8ac2-4398af2a2a48.jpeg)
<br>  

### **10. 나이-혼인 정도 시각화 및 분석**  

<br>
```r
```
<br>
![Result_age](https://user-images.githubusercontent.com/19165180/116013902-98322000-a66d-11eb-9f43-48be697e5319.jpeg)

<br>
<br>  

# Conclusion  
> 