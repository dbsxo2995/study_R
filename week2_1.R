#week2_1

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


install.packages("readxl")
library(readxl)
#1.1. 외부데이터 처리하기
    exam <- read_excel("excel_exam.xlsx")
    
    #1.1.1 header & Col_names
    View(read_excel("excel_exam_novar.xlsx"))
    exam_novar <- read_excel("excel_exam_novar.xlsx", col_names = F)
    
    
    library(dplyr)
    exam_novar <- rename(exam_novar, First = X__1)
    
    #1.1.2. sheet number
    View(read_excel("excel_exam_sheet.xlsx", sheet = 3))
    
    #1.1.3. CSV file
    df_csv_exam <- read.csv("csv_exam.csv")
    
    #1.1.4. Csv file & straingAsFactors
    View(read.csv("csv_exam.csv",
                  header = F,
                  fileEncoding = "UTF-8",
                  stringsAsFactors = F,
                  skip = 3))
    
    #1.1.5
    df_midter <- data.frame(eng = c(10,20,30,40,50),
                            math = c(30,40,50,60,NA),
                            class = c(1,1,2,2,2))
    
    #write.csv(data, file = "저장할 파일이름.csv")
    write.csv(df_midter, file = "item.csv")
    
    #Note That TXT file
    txt <- readLines("hiphop.txt")
    

    #2.1 데이터처리를 위한 필수 함수 
    #2.1.1 doBy::package
    install.packages("doBy")
    library(doBy)
    
        #2.1.1.1 summaryBy()
        summary(iris)
        
        #Sepal.Width & Sepal.Length를 Species로 요약
        summaryBy(Sepal.Width + Sepal.Length ~ Species, data= iris)
        #summaryBy(formula, data, FUN)
        
        #2.1.1.2 orderBy()
        order(iris$Sepal.Length)
        head(iris[order(iris$Sepal.Width),],10)
        head(iris[order(iris$Sepal.Width, iris$Sepal.Length),],10)
        
        View(orderBy(~ Sepal.Width, iris))
        View(orderBy(~ Sepal.Width + Sepal.Length, iris))
        
        #2.1.1.3 samply
        sample(
          x,                        #샘플을 뽑을 데이터 벡터
          size,                     #샘플의 크기
          replace =FALSE,           #복원추출 여부
          prob = c(1,1,2)           #데이터가 뽑힐확률
        )
        sample(1:10, 50, replace = TRUE, prob = c(1,1,1,1,1,1,1,1,1,100))
        iris[sample(NROW(iris), NROW(iris)), ]
        
  
    #2.2.1 데이터 분리
        #split(x, f #f 는 분리할 기준)
        #subset(x, subset, select )
  
        split(iris, iris$Species)     
        subset(iris, Species == "setosa")
        subset(iris,
               Species == "setosa" & 
                 Sepal.Length > 5.0)
        
        subset(iris, select = c(Sepal.Length, Species))
        subset(iris, select = -c(Sepal.Length, Species))라
        
        subset(iris, Species %in% c("setosa","virginica")) # %in% 은 매칭확인 논리연산자
        
    #2.2.2 데이터 지우기
        #데이터이름[!names(데이터) %in% c(“변수1”,”변수2”)]
        data_iris <- iris
        iris[!names(iris) %in% c("Species")]
        
    #2.3.1 데이터 정렬
        #sort(x, #정렬할 벡터
         #    decreasing = FALSE, #내림차순 여부
          #   na.last = NA     #FALSE, NA값이 맨처음에, TRUE, NA값이 맨뒤에  )
             
        x <- c(20,11,33,50,47)     
        sort(x, decreasing = T)
        
    #2.4.1 apply계열 함수(optional)
        #apply(X, 
        #      MARGIN = 1, #행방향 2는 열방향 c(1,2)는 행열 전부
        #      FUN #적용할 함수)
        
        d <- matrix(1:9, ncol = 3)
        apply(d, 2, max)
        
        apply(iris[, 1:4], 2, sum)
        
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
?mice
        