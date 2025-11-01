#실습에 필요한 packages를 라이브러리에 등록
library(dplyr)
library(ggplot2)

#데이터를 새객체 foodshop으로 불러오기
#비어있는 셀은 결측치 처리/파라미터 문자형으로 변환
foodshop <- read.csv("Gyeonggi_food_2025.csv", na="", stringsAsFactors = F)

#데이터 구조 확인
str(foodshop)

#분석변수 추출 및 변수이름 변경
foodshop <- foodshop %>%
  rename(open_date=인허가일자, status=상세영업상태명, 
         close_date=폐업일자, name=사업장명, type=업태구분명,
         address=소재지전체주소) %>%
  select("name","type","status","open_date","close_date", "address")

#추출된 데이터 구조 확인
str(foodshop)

#날짜데이터를 분석용 데이터로 변경
#1.YYYYMMDD형식으로 변경
foodshop$open_date <- gsub("-","",foodshop$open_date)
foodshop$close_date <- gsub("-","",foodshop$close_date)
#2.문자형 데이터를 정수형 데이터로 변환
foodshop$open_date <- as.integer(foodshop$open_date)
foodshop$close_date <- as.integer(foodshop$close_date)
#3.변경된 데이터구조 확인
str(foodshop)

#.파생변수 만들기
#1.status변수
table(foodshop$status)
#영업상태가 영업/폐업이 아닌 것을 제외
foodshop <- foodshop %>% 
  filter(status == '영업' | status == '폐업') %>%
  select(name,type,status,open_date,close_date,address)
#처리결과 확인
table(foodshop$status)

#2.type변수
table(foodshop$type)

#3.open_date변수
range(foodshop$open_date, na.rm = T)
table(is.na(foodshop$open_date))#결측치 검사
#na값제외
foodshop<-foodshop%>%filter(open_date!= '') %>%select(name,type,status,open_date,close_date,address)
foodshop$open_year<-substr(foodshop$open_date,1,4)#인허가년도 변수 생성

#4.close_date변수
range(foodshop$close_date, na.rm = T)
table(is.na(foodshop$close_date))#결측치 검사
#na값제외
foodshop<-foodshop%>%filter(close_date!= '') %>%select(name,type,status,open_date,close_date,address)
foodshop$close_year<-substr(foodshop$close_date,1,4)#인허가년도 변수 생성

#5.address변수
foodshop$city<-substr(foodshop$address,4,8)#시 정보를 분리하여 변수 생성
table(foodshop$city)#이상치 확인
foodshop$city <- ifelse (foodshop$city%in%c(",106호","6번지","도 밀양시","도 영암군","별시 강남","별시 관악","별시 금천","별시 노원","별시 마포","별시 용산","별시 은평", "별자치도 ","사회","역시 계양","역시 미추","역시 남동","역시 서구"),NA,foodshop$city)#이상치제거
table(foodshop$city)#이상치 확인

#최종 확인
str(foodshop)









