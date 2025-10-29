#데이터에서 크기 2인 표본추출
salary=c(45,50,50,55,55,60,60,65,70,90)#연봉데이터
com2=combn(salary,2)#연봉데이터에서 2개씩 선택해서 데이터로 생성,com2에 저장
print(com2)

#표본의 크기2로 추출된 데이터의 평균을 구한 뒤 막대그래프 작성 
com2t=t(com2)#데이터의 전치치
z=rowMeans(com2t)#전치된 데이터의 행평균 계산
barplot(table(z),xlab="연봉평균",ylab="도수",main="연봉평균의 분포")

#표분평균의 기댓값 구하기
meanofz=mean(z)#z의 평균
print(meanofz)

#표분의 크기를 늘려 정규분포에 가까워지는지 확인하기 
#sample size=2
data50=c(1:50)#데이터
com2=combn(data50,2)#데이터에서 2개씩 선택하여 데이터 생성
com2t=t(com2)#데이터의 전치
z2=rowMeans(com2t)#전치된 데이터 행평균 계산
meanofz2=mean(z2)#기댓값을 계산해 저정
print(meanofz2)
barplot(table(z2),xlab="평균2",ylab="도수",main="표본크기2의 도수분포표")

#sample size=3
com3=combn(data50,3)#데이터에서 3개씩 선택하여 데이터 생성
com3t=t(com3)#데이터의 전치
z3=rowMeans(com3t)#전치된 데이터 행평균 계산
meanofz3=mean(z3)##기댓값을 계산해 저정
print(meanofz3)
barplot(table(z3),xlab="평균3",ylab="도수",main="표본크기3의 도수분포표")

#sample size=5
com5=combn(data50,5)#데이터에서 5개씩 선택하여 데이터 생성
com5t=t(com5)#데이터의 전치
z5=rowMeans(com5t)#전치된 데이터 행평균 계산
meanofz5=mean(z5)##기댓값을 계산해 저정
print(meanofz5)
barplot(table(z5),xlab="평균5",ylab="도수",main="표본크기5의 도수분포표")

#표본추출방법: 단순무작위추출법
sampling=c(10:100)
print(sampling)
sample(x=sampling,size=10)

#표본추출방법: 층화추출법
install.packages("sampling")
library(sampling)
sam2=strata(data=iris,stratanames=c("Species"),size=c(3,3,3),method='srswor')#iris데에터 사용하여 표본추출,Species층을 기준으로
sample2=getdata(data=iris,m=sam2)#추출된 데이터 저장
print(sample2)
