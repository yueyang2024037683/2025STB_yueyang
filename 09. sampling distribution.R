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
