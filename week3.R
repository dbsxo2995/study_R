#week3_1
getwd()
stud<-read.csv("stud_math.csv")
head(stud)
dim(stud)
str(stud)

attach(stud)
detach(stud)

#1.1 단일표본의 평균검정 (t-test)
t.test(stud$G3, mu = 10)                                 #H0 : G3의 평균은 10 G3는 최종점수
t.test(stud$G3, mu = 10, alternative = c("greater"))     #H1 : G3의 평균은 10이 아니다. Not H0
mean(stud$G3)
#일반적으로 P <= 0.05 이면 귀무가설을 기각한다. 
#반대로 P > 0.05 이면 귀무가설을 채택한다

#1.2 두집단 표본평균 비교검정 t.test(타겟변수~범주형변수, data=) where formula y ~ like factor
#거주지역(R, U)에 따라서 G3(최종성적) 평균에 차이가 있는가
t.test(G3 ~address, data= stud) #거주지역에 따라서 최종성적은 유의한 차이가 있다! 도시가 더높다
boxplot(stud$G3 ~stud$address)

t.test(G3 ~address, data = stud, alternantive = c("less")) #대립가설 : Ur < Ul -> 도시의 평균점수가 더 높다
#1.3. 두집단 표본평균 비교검정
#방과후 활동여부(yes, no)에 따라서 G3(최종성적) 평균에 차이가 있는가?
t.test(G3 ~activities, data = stud) #방과후 활동여부에 따라 최종성적에는 큰 차이가 없다
boxplot(stud$G3~ stud$activities)

#1.4 두 모집단의 비모수적 방법 wilcox.test(타겟변수~범주형변수)
#데이터의 분포가 특정한 분포를 따르지 않는다. 예컨데 주관적 척도(통증, 기분좋음 등)
#H0 :  정규분포를 따른다 H1 : Not H0
wilcox.test(stud$G3 ~ stud$address) #P <= 0.05이므로 귀무가설을 기각한다.

#1.5. 짝을 이룬 그룹간 비교(paired t-test)
#특정 효과를 비교 분석할 대 사용
#befor & after

bp <- read.csv("bp.csv")
t.test(bp$bp_pre, bp$bp_post, mu = 0, paired = T)                          #H0 기각 -> 투약전/후 혈압에 유의한 차이가 있다.                    
t.test(bp$bp_pre, bp$bp_post, mu = 0 , alternative = "greater", paired = T)#H0 기각 -> 투약전/후 혈압에 유의한 차이가 있다.

#1.6 짝을이룬 그룹간 비교
weight <- read.csv("weight.csv")
View(weight)

t.test(weight$wt_pre, weight$wt_post, mu = 0, paired = T) #H0 기각 -> 몸무게가 전/후에 유의한 차이가 있다.

#1.7 분산분석 아노바 aov(타겟변수 ~ 범주형변수)
#전체분산을 분할하여 어떤 요인(foactor)의 영향이 유의한지 검정하는방법
#목적
#1.7.1. 거주지역에 따른 학업성취도 : 거주쥐역(R/U),       학업성적 1부터20
#1.7.2. 통학시간에 따른 학업성취도 : 통학시간(factor 1:4) 학업성적 1부터20


a1 <- aov(G3 ~ address, data = stud) #거주지역에 따라 G3에 유의한 영향이 있나?
summary(a1)                          #유의수준이 0.05보다 작으므로 거주지역에 따라 G3에는 유의한 차이가 있다.

stud$traveltime <- as.factor(stud$traveltime)
a2 <- aov(G3 ~traveltime, data = stud)
summary(a2)                          #유의수준이 0.05보다 크므로 통학시간에 따라 G3에는 유의한 차이가 없다.
boxplot(stud$G3 ~ stud$traveltime)

#1.8 아노바 사후검정
#아노바에서 팩터의 유의성이 검정되면 그 다음단계에 하는 검정!
TukeyHSD(a2, "traveltime", ordered = T)
plot(TukeyHSD(a2, "traveltime", ordered = T))  #모든구간에 0을 포함하고있다! = 두그룹간 평균의 차이가 없다.

#1.9 아노바 사후검정
#연애 경험여부에 따른 학업성취도 연해경험, 학업성적
stud$romantic <- as.factor(stud$romantic)
a4 <- aov(G3 ~ romantic, data = stud)
summary(a4)

#2.1 상관계수 cor(x, y, method = c("pearson", "kendall", "spearman))
#2.1.1 pearson
cor(iris[,1:4])

install.packages("corrgram")
library(corrgram)

corrgram(iris, upper.panel = panel.conf) #상관계수의 신뢰구간까지


#2.2 샤피로 윌크 검정
#표본이 정규분포로부터 추출된 것인지 알아보기 위한 기법
shapiro.test(x ) #숫자 벡터

shapiro.test(rnorm(100))


#2.3 콜모고로프 스미르노프(KS 검정)
#누적분포 함수간의 최대 거리를 통계량으로 두 분포가 서로 같은지에 대한 검정

ks.test(x, y, ..., alternative = c("less","greater","two.sided"))

ks.test(rnorm(100), rnorm(100))
ks.test(rnorm(100), runif(100))

ks.test(rnorm(100), "pnorm", 0, 1) #표준정규분포로 뽑은건가?


#TIP1    options()
options(prompt = 'R> ')
options(stringsAsFactors = F)

options(scipen = 3)
print(1000000)
#TIP2   source
source('경로') #함수 불러오기

#TIP3  memory
memory.size()
memory.limit()

x <- rep(0, 300000000)
memory.size()

memory.limit("size add")

#3.1 KONLP
library(KoNLP)
library(stringr)
library(tm)


#3.2 기초 자연어 처리 
extractNoun('사과를 먹으러 산속으로 갑니다') #extractNoun 명사만 뽑는 함수.
pos <- (SimplePos22('사과를 먹으러 산속으로 갑니다'))
pos #동사 형용사 뽑고싶으면 NC으로끝나는 단어와 PV로 시작하는 단어를 뽑으면 동사 형용사

ko_words <- function(doc){
  d = as.character(doc)
  pos = SimplePos22(d)
  
  extracted = str_match(pos, pattern = "([가-힣]+)/[NC+]") #동사와 형용사는 NCPV 혹은 PV만
  keyword = extracted[, 2]
  keyword[!is.na(keyword)]
}

text <- c("사과를 먹으러 산속으로 갑니다")

cps <- VCorpus(VectorSource(text))
tdm <- TermDocumentMatrix(cps,
                          control = list(tokenize = ko_words,
                                         wordLengths = c(2, Inf),
                                         stopwords= c("갑니")))
as.matrix(tdm)

text1 <- c("예산에서 재배되는 사과가 제일 맛있다","사과는 맛있다")

cps <- VCorpus(VectorSource(text1))
tdm <- TermDocumentMatrix(cps,
                          control = list(tokenize = ko_words,
                                         wordLengths = c(2, Inf),
                                         stopwords= c("맛있")))
as.matrix(tdm)

library(slam)
library(doBy)
word_count <- as.array(rollup(tdm, 2)) #행합계
word_order <- order(word_count, decreasing = T)
freq_word <- word_order[1:2] 
row.names(tdm[freq_word, ])

word <- sort(slam::row_sums(tdm), decreasing = T)
data <- data.frame(X=names(word),freq=word)
#

#3.3 WORD CLOUD 
hiphop <- read.csv("hiphop.csv")
ko_words <- function(doc){
  d = as.character(doc)
  pos = SimplePos22(d)
  
  extracted = str_match(pos, pattern = "([가-힣]+)/[NC+]") #동사와 형용사는 NCPV 혹은 PV만
  keyword = extracted[, 2]
  keyword[!is.na(keyword)]
}

text <- hiphop[,2]
cps <- VCorpus(VectorSource(text))
tdm <- TermDocumentMatrix(cps,
                          control = list(tokenize = ko_words,
                                         wordLengths = c(2, Inf),
                                         stopwords= c("남아있","그것")))

(as.matrix(tdm))

library(slam)
library(doBy)
library(wordcloud2)


word <- sort(slam::row_sums(tdm), decreasing = T)
data <- data.frame(X=names(word),freq=word)
wordcloud2(data[1:200,])

library(wordcloud)
library(RColorBrewer)

display.brewer.all()
display.brewer.pal(n = 8, name = 'Spectral')
pal <- brewer.pal(n = 8, name = "Spectral")

wordcloud(words = data$X,
          freq = data$freq,
          colors = pal,
          min.freq = 2,              #최소빈도
          max.words = 300,           #구성할 단어의수
          random.order = F,          #고빈도 단어 중앙배치(F)
          rot.per = 0.1,             #0.1로하면 단어의 10%가 90도회전해서 나옴
          scale = c(4, 0.3),         #빈도가 가장큰 단어하고 가장 작은단어의 사이크기
          random.color = T,          #색이 랜덤하게 나옴(F는 빈도순)
          family= "malgun")          #headline, baedal, a한글사랑L






