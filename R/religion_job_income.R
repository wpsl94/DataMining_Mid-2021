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


###3. 직업별 통계 및 직업-혼인률 상위/하위 10위

#job table
list_job <- read_excel("Koweps_Codebook.xlsx", col_names = T, sheet =4)
dim(list_job)
head(list_job)

df.wr.outlier <- left_join(df.wr.outlier, list_job, id="code_job")
df.wr.outlier %>% 
  filter(!is.na(job)) %>% 
  select(code_job, job) %>% 
  head(10)


##job-income
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

income.top
income.bot


#그래프 그리기
top10 = ggplot(data = income.top, aes(x=reorder(job, mean_income), y=mean_income, fill=mean_income)) + geom_col() + coord_flip()+ labs(x="직종", y="연봉(만원)", fill="만 원")  + 
  ylim(c(0,20000)) + ggtitle("상위 10위 직업-연봉") 
bot10 = ggplot(data = income.bot, aes(x=reorder(job, -mean_income), y=mean_income, fill=mean_income)) + geom_col() + coord_flip()+ labs(x="직종", y="연봉(만원)", fill="만 원") +
  ylim(c(0,20000)) + ggtitle("하위 10위 직업-연봉") 

grid.arrange(top10, bot10,ncol=1)




##job-marriage


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


job_marriage.top
job_marriage.bot


#그래프 그리기
job.top = ggplot(data = job_marriage.top, aes(x=reorder(job, n), y=n, fill = n)) + geom_col() + coord_flip()+ labs(x="직종", y="명", fill="명") + ylim(c(0,6000)) + ggtitle("상위 10위 직업-응답자")
job.bot = ggplot(data = job_marriage.bot, aes(x=reorder(job, -n), y=n, fill = n)) + geom_col() + coord_flip()+ labs(x="직종", y="명", fill="명") + ylim(c(0,100)) + ggtitle("하위 10위 직업-응답자")

grid.arrange(job.top, job.bot,ncol=1)





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

#그래프 그리기
top.30 = ggplot(data = top30 , aes(x=reorder(job, pnt), y=pnt, fill= pnt))+ geom_col() + coord_flip()+ labs(x="직종", y="%", fill="%") + ggtitle("상위 15위 직업-혼인률")
bot.30 = ggplot(data = bot30 , aes(x=reorder(job, pnt), y=pnt, fill= pnt))+ geom_col() + coord_flip()+ labs(x="직종", y="%", fill="%") + ggtitle("하위 15위 직업-혼인률")
grid.arrange(top.30, bot.30, ncol=1)


