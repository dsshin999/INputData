#데이터를 받아서 50대, 60대 70대, 80대 이상으로 데이터를 정렬하고 보행수의 차이를 비교군과 실험군으로 나타내주는 그래프
library(dplyr)
dat<-read.csv("/Users/YejinKim/Desktop/data 3.csv",header=T,stringsAsFactors = F)
print("Enter kinds of illness")
a<-c("A","B","C")
experimental<-filter(dat, char=="experimental")
experimental.a<-filter(experimental, illness==a[1]) #질병1
experimental.b<-filter(experimental, illness==a[2]) #질병2
experimental.c<-filter(experimental, illness==a[3]) #질병3

comparison<-filter(dat, char=="comparison")
comparison.a<-filter(comparison, illness==a[1]) #질병1
comparison.b<-filter(comparison, illness==a[2]) #질병2
comparison.c<-filter(comparison, illness==a[3]) #질병3


#실험군 데이터들을 나이대별로 평균내어 테이블로 저장
aver.ex.a<-summarise(experimental.a, aver_bf.eq=mean(bf.eq5d), aver_bf.gh=mean(bf.gh), aver_bf.mr=mean(bf.mr), aver_bf.vt=mean(bf.vt), aver_bf.st=mean(bf.strides), aver_af.eq=mean(af.eq5d), aver_af.gh=mean(af.gh), aver_af.mr=mean(af.mr), aver_af.vt=mean(af.vt), aver_af.st=mean(af.strides))
aver.ex.b<-summarise(experimental.b, aver_bf.eq=mean(bf.eq5d), aver_bf.gh=mean(bf.gh), aver_bf.mr=mean(bf.mr), aver_bf.vt=mean(bf.vt), aver_bf.st=mean(bf.strides), aver_af.eq=mean(af.eq5d), aver_af.gh=mean(af.gh), aver_af.mr=mean(af.mr), aver_af.vt=mean(af.vt), aver_af.st=mean(af.strides))
aver.ex.c<-summarise(experimental.c, aver_bf.eq=mean(bf.eq5d), aver_bf.gh=mean(bf.gh), aver_bf.mr=mean(bf.mr), aver_bf.vt=mean(bf.vt), aver_bf.st=mean(bf.strides), aver_af.eq=mean(af.eq5d), aver_af.gh=mean(af.gh), aver_af.mr=mean(af.mr), aver_af.vt=mean(af.vt), aver_af.st=mean(af.strides))

#대조군 데이터를 나이대별로 평균내어 테이블로 저장
aver.cp.a<-summarise(comparison.a, aver_bf.eq=mean(bf.eq5d), aver_bf.gh=mean(bf.gh), aver_bf.mr=mean(bf.mr), aver_bf.vt=mean(bf.vt), aver_bf.st=mean(bf.strides), aver_af.eq=mean(af.eq5d), aver_af.gh=mean(af.gh), aver_af.mr=mean(af.mr), aver_af.vt=mean(af.vt), aver_af.st=mean(af.strides))
aver.cp.b<-summarise(comparison.b, aver_bf.eq=mean(bf.eq5d), aver_bf.gh=mean(bf.gh), aver_bf.mr=mean(bf.mr), aver_bf.vt=mean(bf.vt), aver_bf.st=mean(bf.strides), aver_af.eq=mean(af.eq5d), aver_af.gh=mean(af.gh), aver_af.mr=mean(af.mr), aver_af.vt=mean(af.vt), aver_af.st=mean(af.strides))
aver.cp.c<-summarise(comparison.c, aver_bf.eq=mean(bf.eq5d), aver_bf.gh=mean(bf.gh), aver_bf.mr=mean(bf.mr), aver_bf.vt=mean(bf.vt), aver_bf.st=mean(bf.strides), aver_af.eq=mean(af.eq5d), aver_af.gh=mean(af.gh), aver_af.mr=mean(af.mr), aver_af.vt=mean(af.vt), aver_af.st=mean(af.strides))


aver.cp<-rbind(aver.cp.a,aver.cp.b,aver.cp.c) #aver.cp에 모든 나이별 데이터가 한 테이블에 묶임
aver.cp$illness<-a
aver.cp
aver.ex<-rbind(aver.ex.a,aver.ex.b,aver.ex.c)
aver.ex$illness<-a

char<-c("comparison", "comparison", "comparison")
aver.cp$char<-char#대조군에 대조군임을 알려주는 열 추가
char<-c("experimental", "experimental","experimental")
aver.ex$char<-char
total<-rbind(aver.cp, aver.ex)#대조군 데이터와 실험군 데이터가 한 테이블에 묶임

total

diff.st<-total$aver_af.st-total$aver_bf.st
diff.eq5d<-total$aver_af.eq-total$aver_bf.eq
diff.mr<-total$aver_af.mr-total$aver_bf.mr
total$diff.st<-diff.st
total$diff.eq<-diff.eq5d
total$diff.mr<-diff.mr


ggplot(data=total, aes(x=illness)) + #가로축을 나이대로
  geom_bar(width=0.3, stat="identity", position="dodge", aes(y=diff.st, fill=char)) + #바의 넓이는 1.5, 이후 데이터는 이전 데이터보다 오른쪽에 위치하도록 2만큼 움직임
  ylab("difference of exercising") + #y축 이름 정함
  theme_bw() #뒷배경을 하얗게

ggplot(data=total, aes(x=illness)) + #가로축을 나이대로
  geom_bar(width=0.3, stat="identity", position="dodge", aes(y=diff.eq, fill=char)) + #바의 넓이는 1.5, 이후 데이터는 이전 데이터보다 오른쪽에 위치하도록 2만큼 움직임
  ylab("difference of eq5d") + #y축 이름 정함
  theme_bw() #뒷배경을 하얗게

ggplot(data=total, aes(x=illness)) + #가로축을 나이대로
  geom_bar(width=0.3, stat="identity", position="dodge", aes(y=diff.mr, fill=char)) + #바의 넓이는 1.5, 이후 데이터는 이전 데이터보다 오른쪽에 위치하도록 2만큼 움직임
  ylab("difference of metal restriction") + #y축 이름 정함
  theme_bw() #뒷배경을 하얗게