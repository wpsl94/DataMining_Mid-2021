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





