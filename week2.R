#week2_1 update 11.09

#Q1 dataset은 다음과 같다. poptotal(전체인구)변수를 total로, popasian(아시아인구)변수를 asian으로 수정하라.
#Q2 total, asian변수를 이용해 percent_Asaian(아시아인/전체인구) 이라는 파생변수를 만들라.
#Q3.percent_Asian의 전체평균에서 평균이상이면 Large 그외는 small을 부여하는 mean_Asain이라는 파생변수를 만들어라

    
    midwest <- ggplot2::midwest
    
    library(dplyr)
    
    midwest <- rename(midwest,
                      total = poptotal,
                      asian = popasian)
    
    midwest$percent_Asian <- midwest$asian / midwest$total
    
    midwest$mean_Asain <- ifelse(midwest$percent_Asian >= mean(midwest$percent_Asian), "Large", "Small")
    
    table(midwest$mean_Asain)
    
#2.1.1. 외부데이터 처리하기
    exam <- read_excel("excel_exam.xlsx")
    
    #2.1.2 header & Col_names
    View(read_excel("excel_exam_novar.xlsx"))
    exam_novar <- read_excel("excel_exam_novar.xlsx", col_names = F)
    
    
    library(dplyr)
    exam_novar <- rename(exam_novar, First = X__1)
    
    #2.1.3. sheet number
    View(read_excel("excel_exam_sheet.xlsx", sheet = 3))
    
    #2.1.4. CSV file
    df_csv_exam <- read.csv("csv_exam.csv")
    
    #2.1.5. Csv file & straingAsFactors
    View(read.csv("csv_exam.csv",
                  header = F,
                  fileEncoding = "UTF-8",
                  stringsAsFactors = F,
                  skip = 3))
    
    #2.1.6
    df_midter <- data.frame(eng = c(10,20,30,40,50),
                            math = c(30,40,50,60,NA),
                            class = c(1,1,2,2,2))
    
    #write.csv(data, file = "저장할 파일이름.csv")
    write.csv(df_midter, file = "item.csv")
    
    #Note That TXT file
    txt <- readLines("hiphop.txt")
    txt_seq <- read.table("hiphop.txt", sep = ",")

    
#2.2 데이터처리를 위한 필수 함수 
#2.2.1 doBy::package
    install.packages("doBy")
    library(doBy)
    
    #2.2.1.1 summaryBy()
    summary(iris)
    
    #Sepal.Width & Sepal.Length를 Species로 요약
    summaryBy(Sepal.Width + Sepal.Length ~ Species, data= iris)
    #summaryBy(formula, data, FUN)
    
    #2.2.1.2 orderBy()
    order(iris$Sepal.Length)
    head(iris[order(iris$Sepal.Width),],10)
    head(iris[order(iris$Sepal.Width, iris$Sepal.Length),],10)
    
    View(orderBy(~ Sepal.Width, iris))
    View(orderBy(~ Sepal.Width + Sepal.Length, iris))
    
    #2.2.1.3 samply
    sample(
      x,                        #샘플을 뽑을 데이터 벡터
      size,                     #샘플의 크기
      replace =FALSE,           #복원추출 여부
      prob = c(1,1,2)           #데이터가 뽑힐확률
    )
    sample(1:10, 50, replace = TRUE, prob = c(1,1,1,1,1,1,1,1,1,100))
    iris[sample(NROW(iris), NROW(iris)), ]
    
    set.seed(1234)
    idx <- sample(x = 1:2, size = nrow(iris), prob = c(0.7,0.3), replace = TRUE)
    train_set <- iris[idx == 1, ]
    test_set <- iris[idx == 2, ]
    
#2.3.1 데이터 분리
    #split(x, f #f 는 분리할 기준)
    #subset(x, subset, select )
    
    split(iris, iris$Species)     #백터 분할하기 
    subset(iris, Species == "setosa")
    subset(iris,
           Species == "setosa" & 
             Sepal.Length > 5.0)
    
    subset(iris, select = c(Sepal.Length, Species))
    subset(iris, select = -c(Sepal.Length, Species))라
    
    subset(iris, Species %in% c("setosa","virginica")) # %in% 은 매칭확인 논리연산자
    
#2.3.2 데이터 지우기
    #데이터이름[!names(데이터) %in% c(“변수1”,”변수2”)]
    data_iris <- iris
    iris[!names(iris) %in% c("Species")]
    
#2.4.1 데이터 정렬
    #sort(x, #정렬할 벡터
    #    decreasing = FALSE, #내림차순 여부
    #   na.last = NA     #FALSE, NA값이 맨처음에, TRUE, NA값이 맨뒤에  )
    
    x <- c(20,11,33,50,47)     
    sort(x, decreasing = T)
    
#2.5.1 데이터 합치기
    test1 <- data.frame(id = c(1,2,3,4,5),
                        midtern = c(60,50,40,30,20))
    test2 <- data.frame(id = c(1,2,3,4,5),
                        final = c(90,100,20,60,70))
    
    left_join(test1, test2, by = "id")
    
    
    group_a <- data.frame(id = c(1,2,3,4,5),
                          grade = c("A","B","A","A","A"), stringsAsFactors = F)
    group_b <- data.frame(id = c(6,7,8,9,10),
                          grade = c("C","C","C","A","B"), stringsAsFactors = F)
    
    bind_rows(group_a, group_b)
    bind_cols(group_a, group_b)
    
#2.6.1 데이터 정제하기
    df <- data.frame(sex = c("M","M",NA,"F","M"),
                     score = c(7,5,3,8,NA),
                     money = c(NULL,NA,5,8,4,9))
    df
    table(is.na(df))
    table(is.na(df$money))
    
    na.omit(df)
    
#2.6.2 결측치 제거하기
    subset(df, !is.na(df$score)) #완전히 5행이 사라짐
    subset(df, subset = !is.na(df$score) & !is.na(df$sex))
    
    mean(df$score, na.rm = T)
    sum(df$money, na.rm = T)
    
#2.6.3 결측치 만들기
    exam <- read.csv("csv_exam.csv")
    exam[c(3,8,15), "math"] <- NA
    
    summaryBy(.~class, subset(exam, select = -id), na.rm = T)
    
#2.6.4 결측치 대체하기
    mean(exam$math, na.rm = T)
    exam$math <- ifelse(is.na(exam$math), mean(exam$math, na.rm = T), exam$math)
    table(is.na(exam$math))
    
    math1 <- summaryBy(.~class, subset(exam, select = -id), na.rm = T)[1, 2]
    math2 <- summaryBy(.~class, subset(exam, select = -id), na.rm = T)[2, 2]
    math3 <- summaryBy(.~class, subset(exam, select = -id), na.rm = T)[3, 2]
    math4 <- summaryBy(.~class, subset(exam, select = -id), na.rm = T)[4, 2]
    
    
    
    exam$math <- ifelse(exam$class == 1 & is.na(exam$math), math1,
                        ifelse(exam$class == 2 & is.na(exam$math), math2,
                               ifelse(exam$class == 3 & is.na(exam$math), math3, math4)))

    
#2.6.5 데이터처리를 위한 필수 함수 2
    #apply(x, margin, FUN) 
    mat <- matrix(1:9, ncol = 3); mat
    apply(mat, 1, sum); apply(mat, 2, sum)
    apply(iris[, 1:4], 2, mean)
    
    #lapply(x, FUN) => data -> list
    result <- lapply(1:3, function(x){x^2}); result
    result[[3]]
    unlist(result) # data <- list
    
    x <- list(a = seq(1:10), b = rep(1:5, 2)); x
    lapply(x, sum); lapply(x, mean); unlist(x)
    
    lapply(iris[,1:4], mean); lapply(iris[,1:4], sum)
    
    #sapply(X, FUN) => data -> vector or matrix
    sapply(iris[,1:4], mean)
    sapply(iris, class)
    
    y <- sapply(iris[,1:4], function(x) { x > 3}); y
    class(y)
    
#2.7.1. options MATH
    #2.7.1.1 조합
    #choose(n, k)
    choose(5,2); choose(3,2)
    
    #2.7.1.2 조합 생성
    combn(5, 2);
    combn(1:5, 3);
    
    #2.7.1.3 난수 생성
    runif(1); rnorm(1); rnorm(n = 10, 20, 30); rpois(1, lambda = 2)
    
    #2.7.1.4 누적분포함수
    punif(q = 0.75, 0, 1); pnorm(q = 1.645, 0, 1); ppois(q = 20, lambda = 30)
    
    #2.7.1.5 확률을 분위수로 나타내기
    qnorm(p = 0.05, mean =0, sd = 1); qnorm(p = c(0.025,0.975))

#2.7.2 문자열
    #2.7.2.1 문자열의 길이
    nchar("hello"); nchar(c("hello", "my","name","is","yoontae"))
    #2.7.2.2 문자열 연결하기
    paste("hello","my","name","is","yoontae")
    paste("hello","my","name","is","yoontae", sep = "")
    paste("hello","my","name","is","yoontae", sep = "-")

    name <- c("Yoontae", "R_BOOK", "SEOUL")
    paste("hello","my","name","is",name)
    paste("hello","my","name","is",name, sep = "-")
    paste("hello","my","name","is",name, collapse = ", ANDDDD ") #연결도 가능
    
    #2.7.2.3. 문자열 추출하기
    substr("Hello My name", 1, 4)
    substr("Hello My name", 10, 13)
    
    substr(name, nchar(name)-1, nchar(name))#마지막 2글자만 추출함 
    
    #2.7.2.4 문자열 분할
    path <- "C//home/my/R/study/week2.R"
    strsplit(path, split = "/") #//사이에는 비어있기 때문에 ""가 생김
    
    path_2 <- c("C//home/my/R/study/week2.R",
                "C//home/my/R/study/week3.R",
                "C//home/my/R/study/week4.R")
    strsplit(path_2, "/")
    
    #2.7.2.5 문자열 대체하기
    #sub(Old, New, string)  해당하는 첫번째 글자만 대체
    #gsub(old, new, string) 모든글자 대체
    
    a <- "My name is Yoon tae isis is"
    sub("is", "Isss", a)
    
    a <- "My name is is yoon tae isis is"
    gsub("is", "IS", a)

    #2.7.2.6 문자 조합 
    #outer(str1, str2, paste, sep="")
    name <- c("yoon", "seoul", "tae")
    age <- c(19, 20, 30)
    
    outer(name, age, FUN = paste, sep="-")

    
    #Q1 csv_exam.csv 파일을 읽고 class가 1인 행만을 출력하고, 클래스가 1이 아닌 행도 출력해보아라.
    exam <- read.csv("csv_exam.csv")
    subset(exam, exam$class == 1)
    subset(exam, exam$class != 1)
    
    #Q2수학점수가 50점을 초과하거나 영어점수가 60점 이상인 데이터만 뽑아와라.
    subset(exam, exam$math > 50 | exam$english >=60 )
    
    #Q3수학점수가 50점을 초과하면서 영어점수가 60점 이상인 데이터만 뽑아와라.
    subset(exam, exam$math > 50 & exam$english >=60)
    
    #Q4.exam에서 class라는 컬럼을 지워라.
    exam[!names(exam) %in% c("class")]
    
    #Q5exam에서 class가 1,3,5인 행들을 모두 출력하라
    subset(exam, exam$class %in% c(1,3,5))
    
    #Q5. m mpg데이터 셋에서 displ(배기량)이 4이하인 자동차와 5이상인 자동차 중 어떤 자동차의 hwy(연비)가 평균적으로 높은지 찾아보아라..
    mpg_data <- as.data.frame(ggplot2::mpg)
    
    mpg_a <- subset(mpg_data, mpg_data$displ <= 4)
    mpg_b <- subset(mpg_data, mpg_data$displ >= 5)
    mean(mpg_a$hwy) < mean(mpg_b$hwy)
    
    mean(subset(mpg_data, mpg_data$displ <= 4)[,9]) < mean(subset(mpg_data, mpg_data$displ >= 5)[,9])
    
    mean(subset(mpg_data, mpg_data$displ <= 4, select = hwy)[,1])
    
    
    #Q6.  audi와 toyota중 어느 manufacturer(자동차 제조회사)의 cty(도시연비)가 평균적으로 높은지 알아보라.
    mean(subset(mpg_data, mpg_data$manufacturer == "audi")[,8]) < mean(subset(mpg_data, mpg_data$manufacturer == "toyota")[,8])
    
    mpg_a <- subset(mpg_data, mpg_data$manufacturer == "audi")
    mpg_b <- subset(mpg_data, mpg_data$manufacturer == "toyota")
    
    mean(mpg_a$cty) < mean(mpg_b$cty)
    
    #Q7. manufacturer 컬럼에서 chevrolet, ford, honda의 hwy(고속도로연비) 평균을 알아보고자 한다. 이들데이터를
    #  모두 출력후에 hwy의 전체 평균을 구해보아라
    data_MF<- subset(mpg_data, mpg_data$manufacturer %in% c("chevrolet","ford","honda"))
    mean(data_MF$hwy)
    
    #Q8. exam에서 클래스가 1인 english 칼럼들만 뽑아와라
    subset(exam,
           subset = exam$class == 1,
           select = english) 
    
    #Q9. exam에서 id컬럼과 math컬럼을 행 앞부분 6개까지만 뽑아와라
    head(subset(exam,
                select = c("id","math")),6)
    
    #10.  mpg데이터에서 class(자동차종류) 칼럼과 cty(도시연비) 칼럼만을 출력하라..
    subset(mpg_data, select = c('class','cty'))
    
    #11.  exam데이터를 math를 기준으로 작은값부터 정렬하라
    library(doBy)
    orderBy(~math, exam)
    
    #12.exam데이터에서 class와 math를 기준으로 정렬하되, class는 작은것부터 math는 큰 값부터 나오도록 출력하라..
    orderBy(~class -math, exam)
    
    #13. mpg데이터에서 audi에서 생산한 자동차중 어떤 model(모델)이 hwy(고속도로연비) 높은지 알아보라. 단, 최상위 5개의 데이터만 출력하라
    head(
      orderBy(~-hwy, 
              subset(mpg_data, mpg_data$manufacturer == "audi")),5)
    
    #14. exam데이터에서 수학점수 평균을 구하라(단, summaryBy를 이용해 math.mean이라는 칼럼에 값이 57.45가 나오도록 해야한다)
    summaryBy(math~., exam)
    
    #15.exam데이터에서 class별로 모든 평균을 구하라. 단 id컬럼은 제외하라
    summaryBy(.~class, data = subset(exam, select = -id))
    
    #16. exam데이터에서 수학점수 합계를 구하라(단, summaryBy를 이용해 math.mean이라는 칼럼에 값이 1149가 나오도록 하라)
    summaryBy(math~., exam, FUN = sum)
    
    #17. exam데이터에서 class별로 모든 표준편차를 구하라. 단 id컬럼은 제외하라
    summaryBy(.~class, data = subset(exam, select = -id), FUN = sd)
    
    #18.exam데이터에서 class를 기준으로 math성적의 평균과 표준편차를 가져와라
    summaryBy(.~class, data = subset(exam, select = math), FUN = c(mean,sd))
    
    #19.function을 다음과 같이 정의한다.
    sq <- function(x){
      x = sin(x)
    }
    #이때 exam데이터에서 class별로 id는 제외한 컬럼에서 sin(x)의 값들이 나오도록 하라
    summaryBy(.~class, data = subset(exam, select =-id), FUN=sq)
    
    #20. #drv와 hwy변수에 결측치가 몇개인지 알아보라
    
    mpg <- as.data.frame(ggplot2::mpg)
    mpg[c(65, 124, 131, 153, 212), "hwy"] <- NA          
    
    table(is.na(mpg$hwy))
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    