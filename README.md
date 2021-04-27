# DataMining_Midterm Project-2021

---

*Title: "Data Mining(2021)-MidTerm Project"*  
*Author: "72200117 Kim Hee Ju"*  
*Due date: '2021 4 28 '*  

---

<br>  

#### **시작하기 전에..**  

><details><summary></summary>현재 R Markdown 내부 오류 중 Knitr 라이브러리와 관련된 이슈가 발생하였습니다.Chunk가 실행이 되는 도중 Object를 찾지 못하는 에러가 발생했고, 이로 인해 현재 제 다양한 라이브러리와 참조변수가 많이 짜여져 있는 제 코드를 MArkdown v2에서는 Chunk단위로 실행시키기가 어려워 Data의 구성과 함께 설명드리며 결과를 보여드리기 불가한 점 미리 양해부탁드립니다.  <br>
>해당 이슈를 해결하기 위해해 문서를 찾아보는 중에, Markdown v1이후 kntir 패키지를 더 이상 따로 지원을 하지 않아 발생하는 오류 중 하나라고 하였습니다.   
>만약 비슷한 이슈가 있는  있거나, 솔루션을 아신다면 조언 부탁드립니다.   
>본 이슈를 찾아보면서 참고했던 링크를 함께 첨부해드립니다.  </details>

 * <https://rmarkdown.rstudio.com/authoring_migrating_from_v1.html>    
 * <https://community.rstudio.com/t/knitting-issue-object-not-found/23448>    
 * <https://github.com/yihui/knitr/issues/445>  

<br>
<br> 
<br>
<br>  


## **Topic : 혼인 요소에 영향을 미치는 요인 분석 및 시각화**     

> 본 과제의 데이터로 사용된 자료는 한국보건사회연구원에서 가구 경제활동을 연구해 정책 지원에 반영할 목적으로 발간하는 조사 자료인 **한국복지패널데이터**를 2015년부터 2020년, 총 6개년의 자료를 추적한 후, 이를 취합 및 가공한 것으로 사용하였였다. 이를 통해 현재 혼인과 관련된 시간적, 지역적 현황/추이를 살펴보고, 연관성이 있어 보이는 요소를 탐색하였다.  

 * 다운로드 링크   <https://www.koweps.re.kr:442/main.do>  


<br>
<br> 
<br>
<br>  
