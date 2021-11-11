### support 데이터 전처리 (행복e음 복지서비스현황)  ###
# 엑셀 전처리 데이터 불러오기
sp = read.csv('C:/Users/User/Desktop/Dataset/Support.csv')
head(sp)
sum(is.na(sp))

# rbind를 위해 결측값 제거
sup <- na.omit(sp)
summary(sup)

# 서비스분류 분할하여 새로 생성
서비스세부 <- data.frame(do.call('rbind',
                            strsplit(sp$서비스분류,
                                     split=' > ',
                                     fixed = TRUE)))

install.packages("reshape")
library(reshape)
서비스세부 <- rename(서비스세부,
                     c(X1 = "대분류",
                       X2 = "소분류"))

# 데이터프레임 생성 및 저장
support <- data.frame(sup$No,sup$제공주체구분,sup$시설기관구분,sup$제공서비스명,
                 sup$서비스유형,sup$상세등록구분,서비스세부)

write.csv(support, 'C:/Users/User/Desktop/Dataset/Support_2.csv')



### resource 데이터 전처리 (복지자원조사 결과자료) ###
# 엑셀 전처리 데이터 불러오기
rs = read.csv('C:/Users/User/Desktop/Dataset/Resource.csv')
head(rs)
sum(is.na(rs))

# 대분류 데이터 정제
대분류 = gsub("1.|2.|3.|4.|5.|6.|7.|7. |8.|9.","", rs$대분류)
print(대분류)

# 소분류 데이터 정제
소분류 = ifelse(rs$중분류 == 1.1, "직업상담 및 알선",
                ifelse(rs$중분류 == 1.2, "직업능력개발 및 직업교육",
                       ifelse(rs$중분류 == 1.3, "자활 및 일자리사업",
                              ifelse(rs$중분류 == 1.4, "창업지원",
                                     ifelse(rs$중분류 == 1.5, "직업유지 및 자립지원", 
                                            ifelse(rs$중분류 == 1.6, "구직관련 비용지원",
                                                   ifelse(rs$중분류 == 2.1, "주거환경 개선",
                                                          ifelse(rs$중분류 == 2.2, "거처마련 및 이주지원",
                                                                 ifelse(rs$중분류 == 2.3, "주거관련 비용지원",
                                                                        ifelse(rs$중분류 == 3.1, "가사 지원",
                                                                               ifelse(rs$중분류 == 3.2, "식사(식품) 지원", 
                                                                                      ifelse(rs$중분류 == 3.3, "활동(이동) 지원",
                                                                                             ifelse(rs$중분류 == 3.4, "위생(이미용) 지원",
                                                                                                    ifelse(rs$중분류 == 3.5, "생활용품 지원",
                                                                                                           ifelse(rs$중분류 == 3.6, "일상생활관련 비용지원",
                                                                                                                  ifelse(rs$중분류 == 3.7, "복합지원",
                                                                                                                         ifelse(rs$중분류 == 4.1, "질병예방 및 건강관리",
                                                                                                                                ifelse(rs$중분류 == 4.2, "검진·진단 및 치료",
                                                                                                                                       ifelse(rs$중분류 == 4.3, "재활치료",
                                                                                                                                              ifelse(rs$중분류 == 4.4, "산전 후 관리",
                                                                                                                                                     ifelse(rs$중분류 == 4.5, "의약품 의약외품 및 보장구 지원",
                                                                                                                                                            ifelse(rs$중분류 == 4.6, "보건의료관련 비용지원",
                                                                                                                                                                   ifelse(rs$중분류 == 5.1, "정신건강 교육",
                                                                                                                                                                          ifelse(rs$중분류 == 5.2, "심리검사 및 진단",
                                                                                                                                                                                 ifelse(rs$중분류 == 5.3, "정신·심리 상담",
                                                                                                                                                                                        ifelse(rs$중분류 == 5.4, "정서발달 및 치유지원",
                                                                                                                                                                                               ifelse(rs$중분류 == 5.5, "정신질환자 치료 및 사회복귀 지원",
                                                                                                                                                                                                      ifelse(rs$중분류 == 5.6, "정신건강관련 비용지원",
                                                                                                                                                                                                             ifelse(rs$중분류 == 6.1, "장기 시설보호", 
                                                                                                                                                                                                                    ifelse(rs$중분류 == 6.2, "단기 시설보호",
                                                                                                                                                                                                                           ifelse(rs$중분류 == 6.3, "주·야간보호",
                                                                                                                                                                                                                                  ifelse(rs$중분류 == 6.4, "간병 및 돌봄서비스", 
                                                                                                                                                                                                                                         ifelse(rs$중분류 == 6.5, "장제서비스",
                                                                                                                                                                                                                                                ifelse(rs$중분류 == 6.6, "돌봄·요양 관련 비용지원",
                                                                                                                                                                                                                                                       ifelse(rs$중분류 == 7.1, "양육상담 및 부모교육",
                                                                                                                                                                                                                                                              ifelse(rs$중분류 == 7.2, "보육 및 양육지원",
                                                                                                                                                                                                                                                                     ifelse(rs$중분류 == 7.3, "인지발달 및 학습지원",
                                                                                                                                                                                                                                                                            ifelse(rs$중분류 == 7.4, "특기적성지원",
                                                                                                                                                                                                                                                                                   ifelse(rs$중분류 == 7.5, "진로지도 및 상담",
                                                                                                                                                                                                                                                                                          ifelse(rs$중분류 == 7.6, "장애·특수교육",
                                                                                                                                                                                                                                                                                                 ifelse(rs$중분류 == 7.7, "평생교육",
                                                                                                                                                                                                                                                                                                        ifelse(rs$중분류 == 7.8, "보육 및 교육관련 비용지원",
                                                                                                                                                                                                                                                                                                               ifelse(rs$중분류 == 8.1, "공연 전시관람지원",
                                                                                                                                                                                                                                                                                                                      ifelse(rs$중분류 == 8.2, "체육활동 지원",
                                                                                                                                                                                                                                                                                                                             ifelse(rs$중분류 == 8.3, "체험 및 여행지원",
                                                                                                                                                                                                                                                                                                                                    ifelse(rs$중분류 == 8.4, "취미활동지원",
                                                                                                                                                                                                                                                                                                                                           ifelse(rs$중분류 == 8.5, "문화·여가 관련 비용지원",
                                                                                                                                                                                                                                                                                                                                                  ifelse(rs$중분류 == 9.1, "안전 및 인권교육",
                                                                                                                                                                                                                                                                                                                                                         ifelse(rs$중분류 == 9.2, "학대 및 폭력피해자 지원",
                                                                                                                                                                                                                                                                                                                                                                ifelse(rs$중분류 == 9.3, "법률 및 재무상담", "법률지원관련 비용지원"))))))))))))))))))))))))))))))))))))))))))))))))))
                                                                                                                                                                                                                              
head(소분류)

# 데이터프레임 생성 및 저장
resource <- data.frame(rs$No, rs$소재구, rs$소재동, rs$후원기관명, 대분류, 소분류,
                       rs$서비스유형,rs$자격기준,rs$이용가능.인원)

write.csv(resource, 'C:/Users/User/Desktop/Dataset/Resource_2.csv')



### want 데이터 전처리 (통합사례관리 욕구조사 현황)###
# 엑셀 전처리 데이터 불러오기
wt01 = read.csv('C:/Users/User/Desktop/Dataset/Want01.csv')
wt02 = read.csv('C:/Users/User/Desktop/Dataset/Want02.csv')
wt03 = read.csv('C:/Users/User/Desktop/Dataset/Want03.csv')
wt04 = read.csv('C:/Users/User/Desktop/Dataset/Want04.csv')
wt05 = read.csv('C:/Users/User/Desktop/Dataset/Want05.csv')
wt06 = read.csv('C:/Users/User/Desktop/Dataset/Want06.csv')
wt07 = read.csv('C:/Users/User/Desktop/Dataset/Want07.csv')
wt08 = read.csv('C:/Users/User/Desktop/Dataset/Want08.csv')
wt09 = read.csv('C:/Users/User/Desktop/Dataset/Want09.csv')
wt10 = read.csv('C:/Users/User/Desktop/Dataset/Want10.csv')

# 사례관리대상자 추출
wt01 <- wt01[(wt01$대상자.유형 == "사례관리대상자" ), ]
wt02 <- wt02[(wt02$대상자.유형 == "사례관리대상자" ), ]
wt03 <- wt03[(wt03$대상자.유형 == "사례관리대상자" ), ]
wt04 <- wt04[(wt04$대상자.유형 == "사례관리대상자" ), ]
wt05 <- wt05[(wt05$대상자.유형 == "사례관리대상자" ), ]
wt06 <- wt06[(wt06$대상자.유형 == "사례관리대상자" ), ]
wt07 <- wt07[(wt07$대상자.유형 == "사례관리대상자" ), ]
wt08 <- wt08[(wt08$대상자.유형 == "사례관리대상자" ), ]
wt09 <- wt09[(wt09$대상자.유형 == "사례관리대상자" ), ]
wt10 <- wt10[(wt10$대상자.유형 == "사례관리대상자" ), ]

# full join으로 데이터 병합
install.packages("tidyverse")
library(tidyverse)
library(dplyr)
wt = list(wt01,wt02,wt03,wt04,wt05,wt06,wt07,wt08,wt09,wt10) %>% reduce(full_join, by = "읍면동")

# 결측치 제거 및 데이터 정제
wt[is.na(wt)] = 0
wt <- select(wt, -starts_with("대상자"))
wt <- select(wt, -starts_with("합계"))

안전 <- wt %>% select(starts_with("안전")) %>% rowSums
건강 <- wt %>% select(starts_with("건강")) %>% rowSums
일상생활유지 <- wt %>% select(starts_with("일상")) %>% rowSums
가족관계<- wt %>% select(starts_with("가족")) %>% rowSums
사회적관계 <- wt %>% select(starts_with("사회")) %>% rowSums
경제 <- wt %>% select(starts_with("경제")) %>% rowSums
교육 <- wt %>% select(starts_with("교육")) %>% rowSums
고용 <- wt %>% select(starts_with("고용")) %>% rowSums
생활환경 <- wt %>% select(starts_with("생활")) %>% rowSums
법률및권익보장 <- wt %>% select(starts_with("법률")) %>% rowSums

# 데이터프레임 생성 및 저장
want <- data.frame(wt$읍면동, 안전, 건강,일상생활유지,가족관계,사회적관계,경제,교육,고용,생활환경,법률및권익보장)

write.csv(want, 'C:/Users/User/Desktop/Dataset/Want_3.csv')
   


### personal 데이터 전처리 (행복e음 사례관리 업무지원) ###
# 엑셀 전처리 데이터 불러오기
ps = read.csv('C:/Users/User/Desktop/Dataset/Personal.csv')


# 등본 주소가 수원인 사람 추출
install.packages("stringr")
library(stringr)
ps <- ps %>% filter(str_detect(ps$등본주소, "^경기도 수원시"))

# 주민등록번호 기반 성별 파생변수 생성
gender <- substr(ps$주민등록번호,8,8)
gender <- ifelse(gender == 1, "남성",
                 ifelse(gender == 3, "남성",
                        ifelse(gender == 2,"여성","여성")))

# 데이터프레임 생성 및 저장
personal <- data.frame(ps$소재행정동, ps$대상자유형, ps$상담결과, gender, ps$나이, ps$인적정보,
                       ps$욕구조사, ps$대상자선정, ps$서비스제공계획수립, ps$서비스점검, ps$종결여부, ps$사례회의)

personal <- personal[c(order(personal$ps.소재행정동)),]

write.csv(personal, 'C:/Users/User/Desktop/Dataset/Personal_2.csv')



### support, resource 중복 확인 ###
# Python을 이용한 교집합 데이터 전처리
its = read.csv('C:/Users/User/Desktop/Dataset/Intersection.csv')
its = gsub("\\]|'|\\[","", its$Name)
write.csv(its, 'C:/Users/User/Desktop/Dataset/Intersection_2.csv', row.names = FALSE)

# 차집합에 이용할 Resource 데이터 전처리
Resource_3 = read.csv('C:/Users/User/Desktop/Dataset/Resource_3.csv')
정제기관명 = gsub("\\s", "", Resource_3$후원기관명)
Resource_3$후원기관명 = 정제기관명

# 행복e음, 복지자원조사 교집합 제거
library(dplyr)
intersection_2 = read.csv('C:/Users/User/Desktop/Dataset/Intersection_2.csv')
기관명 = setdiff(Resource_3$후원기관명,intersection_2$x)
기관명 <- data.frame(기관명)

Resource_4 <- inner_join(Resource_3,기관명, by=c("후원기관명" = "기관명"))
write.csv(Resource_4, 'C:/Users/User/Desktop/Dataset/Resource_4.csv', row.names = FALSE)



### Welfare 집계
Welf = read.csv('C:/Users/User/Desktop/Dataset/Welfare_5.csv')

행정동 <- Welf %>%
  group_by(행정동) %>%
  summarize(count = n())


대분류 <- Welf %>%
  group_by(대분류) %>%
  summarize(count = n())

소분류 <- Welf %>%
  group_by(행정동,대분류,소분류) %>%
  summarize(count = n())

유형 <- Welf %>%
  group_by(유형) %>%
  summarize(count = n())

분류상세 <- Welf %>% 
  group_by(행정동, 대분류) %>%
  summarize(count = n())

write.csv(행정동, 'C:/Users/User/Desktop/Dataset/행정동.csv', row.names = FALSE)
write.csv(대분류, 'C:/Users/User/Desktop/Dataset/대분류.csv', row.names = FALSE)
write.csv(소분류, 'C:/Users/User/Desktop/Dataset/소분류.csv', row.names = FALSE)
write.csv(유형, 'C:/Users/User/Desktop/Dataset/유형.csv', row.names = FALSE)
write.csv(분류상세, 'C:/Users/User/Desktop/Dataset/분류상세.csv', row.names = FALSE)


### Personal 집계
Ps = read.csv('C:/Users/User/Desktop/Dataset/Personal_4.csv')
나이 = ifelse(Ps$나이 < 20, "미성년자",
             ifelse(Ps$나이 < 25, "20대 초반",
                    ifelse(Ps$나이 < 30, "20대 후반",
                           ifelse(Ps$나이 < 35, "30대 초반",
                                  ifelse(Ps$나이 < 40, "30대 후반",
                                         ifelse(Ps$나이 < 45, "40대 초반",
                                                ifelse(Ps$나이 < 50, "40대 후반",
                                                       ifelse(Ps$나이 < 55, "50대 초반",
                                                              ifelse(Ps$나이 < 60, "50대 후반",
                                                                     ifelse(Ps$나이 < 65, "60대 초반",
                                                                            ifelse(Ps$나이 < 70, "60대 후반",
                                                                                  ifelse(Ps$나이 < 75, "70대 초반",
                                                                                        ifelse(Ps$나이 < 80, "70대 후반",
                                                                                              ifelse(Ps$나이 < 85, "80대 초반",
                                                                                                     ifelse(Ps$나이 < 90, "80대 후반", "90세 이상")))))))))))))))
Ps$나이 <- 나이
write.csv(Ps, 'C:/Users/User/Desktop/Dataset/Personal_5.csv', row.names = FALSE)

행정동 <- Ps %>%
  group_by(행정동) %>%
  summarize(count = n())

성별 <- Ps %>%
  group_by(성별) %>%
  summarize(count = n())

나이 <- Ps %>%
  group_by(나이) %>%
  summarize(count = n())

전체난이도 <- Ps %>%
  group_by(행정동,나이,성별) %>%
  summarize(count = n())



write.csv(행정동, 'C:/Users/User/Desktop/Dataset/행정동_ps.csv', row.names = FALSE)
write.csv(성별, 'C:/Users/User/Desktop/Dataset/성별_ps.csv', row.names = FALSE)
write.csv(나이, 'C:/Users/User/Desktop/Dataset/나이_ps.csv', row.names = FALSE)
write.csv(전체난이도, 'C:/Users/User/Desktop/Dataset/전체난이도_ps.csv', row.names = FALSE)

고_행정동 <- Ps %>%
  group_by(행정동,상담결과) %>%
  summarize(count = n())

고_나이 <- Ps %>%
  group_by(행정동,나이,상담결과) %>%
  summarize(count = n())

고_성별 <- Ps %>%
  group_by(행정동,성별,상담결과) %>%
  summarize(count = n())

고난이도 <- Ps %>%
  group_by(행정동,나이,성별,상담결과) %>%
  summarize(count = n())

write.csv(고_행정동, 'C:/Users/User/Desktop/Dataset/고_행정동_ps.csv', row.names = FALSE)
write.csv(고_나이, 'C:/Users/User/Desktop/Dataset/고_나이_ps.csv', row.names = FALSE)
write.csv(고_성별, 'C:/Users/User/Desktop/Dataset/고_성별_ps.csv', row.names = FALSE)
write.csv(고난이도, 'C:/Users/User/Desktop/Dataset/고난이도_ps.csv', row.names = FALSE)


## 복지자원 분석 모델
seg = read.csv('C:/Users/User/Desktop/Dataset/seg.csv')

문화및여가 <- seg %>%
  filter(대분류 == "문화 및 여가")
  
보육및교육 <- seg %>%
  filter(대분류 == "보육 및 교육")

보호및돌봄요양 <- seg %>%
  filter(대분류 == "보호 및 돌봄요양")

신체건강및보건의료 <- seg %>%
  filter(대분류 == "신체건강 및 보건의료")

안전및권익보장 <- seg %>%
  filter(대분류 == "안전 및 권익보장")

일상생활 <- seg %>%
  filter(대분류 == "일상생활")

일자리 <- seg %>%
  filter(대분류 == "일자리")

정신건강및심리정서 <- seg %>%
  filter(대분류 == "정신건강 및 심리정서")

주거 <- seg %>%
  filter(대분류 == "주거")

기타 <- seg %>%
  filter(대분류 == "기타(대분류)")

세부_분류 <- full_join(문화및여가,보육및교육,by = "행정동") %>%
  full_join(.,보호및돌봄요양,by = "행정동") %>%
  full_join(.,신체건강및보건의료,by = "행정동") %>%
  full_join(.,안전및권익보장,by = "행정동") %>%
  full_join(.,일상생활,by = "행정동") %>%
  full_join(.,일자리,by = "행정동") %>%
  full_join(.,정신건강및심리정서,by = "행정동") %>%
  full_join(.,주거,by = "행정동") %>%
  full_join(.,기타,by = "행정동")

print(세부_분류)
write.csv(세부_분류, 'C:/Users/User/Desktop/Dataset/세부_분류.csv', row.names = FALSE)