#1Step.RStudio에서04. 2025STB_survey.xlsx 파일의데이터를import

# 2Step.(데이터분석) survey의 Gender 1개의 인자를 가지고 도수분포표를 작성
table(survey$Gender)

# 3Step.(데이터분석) survey의 Gender 1개의 인자를 가지고 상대도수분포표를 작성
ECN <- table(survey$Gender)#ECN에 성별의 도수분포표를 대입
prop.table(ECN)#상대 도수로 표시

# 4Step.(데이터분석) survey의 Gender와 Grade 2개의 인자를 가지고 교차표를 작성
table(survey$Gender, survey$Grade)

# 5Step.(데이터분석) survey의 Nationality 1개의 인자를 가지고 막대그래프를 작성
barplot(table(survey$Nationality),xlab="Nationality", ylab="Count",main="Distribution of Nationality", col="yellow")

# 6Step.(데이터분석) survey의 residential area 1개의 인자를 가지고 (가로)막대그래프를 작성
barplot(table(survey$`Residential area`),xlab="Residential area", ylab="Count", main="Distribution of Residential area", col=terrain.colors(12), horiz=TRUE)

# 7Step.(데이터분석) survey의 Gender와 Grade2개의 인자를 가지고 막대그래프를 작성
entry <- table(survey$Gender, survey$Grade)
barplot(entry, legend=TRUE, beside=TRUE, main="Gender vs Grade")

# 8Step.(데이터분석) survey의 Grade 1개의 인자를 가지고 파이차트를 작성
pie(table(survey$Grade), main="Grade Distribution", col=terrain.colors(12))

# 9Step.(데이터분석) survey의 Age인자를 가지고 히스토그램을 작성
hist(survey$Age, col=terrain.colors(12), main="Age Distribution", xlab="Age")

# 10Step.(데이터분석) survey의 Grade별Age를 비교하는 박스플롯을 작성, 그리고 Grade별Age에 대한 기술통계분석을 실시하여 각 박스플롯을 비교 설명

#survey의 Grade 박스플롯 작성1
boxplot(survey$Grade, main="survey's Grade distributions boxplot within my class",  col="blue")

#survey의 Age 박스플롯 작성2
boxplot(survey$Age, main="survey's Age distributions boxplot within my class", col="green")

#Grade별Age에 대한 기술통계:
summary(survey) 
summary(survey, na.rm=T)

#박스플롯 작성3:병렬 비교 
boxplot(Age ~ Grade, data=survey, xlab="Grade", ylab="Age", main="Age distribution by Grade", col=c("lightblue","lightgreen","lightpink","lightyellow"))

# 11Step.(데이터분석) survey의 Grade를 X값으로 Age를 Y값으로 하는 산점도 
#산점도1:
plot(x=survey$Grade, y=survey$Age, xlab="Grade", ylab="Age", main="survey's Grade and Age distributions plot within my class",col="blue", pch=19)

#산점도2
plot(x=survey$Grade, y=survey$Age, xlab="Grade", ylab="Age", main="survey's Grade and Age distributions plot within my class", pch=24, bg="yellow", cex=1.5)

#산점도3
plot(x=survey$Grade, y=survey$Age, xlab="Grade", ylab="Age", main="survey's Grade and Age distributions plot within my class",col="green",type = "h")
