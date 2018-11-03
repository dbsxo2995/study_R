#Week1_3 

#1.1 iris Data 탐색
    data(iris) 
    head(iris)
    
    str(iris)
    dim(iris)
    summary(iris)
    
    head(iris)
    tail(iris)
    
    max(iris$Sepal.Length)
    min(iris$Sepal.Length)
    which.max(iris$Sepal.Length)
    which.min(iris$Sepal.Length)
    
    fivenum(iris$Sepal.Length)
    mean(iris$Sepal.Length)
    median(iris$Sepal.Length)
    sd(iris$Sepal.Length)
    
#1.2 데이터프레임 생성
    x <- data.frame(id = c("1","2","3","4","5"),
                   math = c(10,20,30,40,50),
                   eng = c(60,70,80,90,100),
                   stringsAsFactors = FALSE)
    
    id_1 <- c("1","2","3","4","5")
    math_score <- c(10,20,30,40,50)
    y <- data.frame(id = id_1, math = math_score)
    
    #1.2.1 데이터프레임 탐색
    x <- data.frame(a = c(1,2,3),
                    b = c("A", NA, "c"),
                    c = c("a","B", NULL, 3))
    
    x <- data.frame(a = c(1,2,3),
                    b = c("A", NA, "c"),
                    c = c("a","B", NA))
    x
    na.fail(x)
    
    na.omit(x)
    na.exclude(x)
    
    na.pass(x)
    
        #1.2.1.1 na.omit(x) vs na.excude(x)
            df <- data.frame(x = 1:5,
                             y = seq(2, 10, 2))
            df
            df[3,2] = NA
        
            resid(lm(y ~ x, 
                     data = df,
                     na.action = na.omit))
            resid(lm(y ~ x,
                     data = df,
                     na.action = na.exclude))
   
    #1.3.1 데이터프레임에서의 타입변환
    d <- data.frame(x = c(1,2,3,4,5),
                    y = c(6,7,8,"9",10),
                    z = c("M","F","M","M","F"))
    is.factor(d$z)
    d$z <- as.character(d$z)
    d$y <- as.numeric(d$y)
    
    
    #1.3.2 데이터프레임에서 변수명 바꾸기 #교과서 110P
    df_raw <- data.frame(var1 = c(1,2,3),
                         var2 = c(4,5,6))
    
    install.packages("dplyr")
    library(dplyr)
    
    df_new <- df_raw
    
    #rename(data, new = old)
    
    df_new <- rename(df_new, v2 = var2)
    df_new
    
    #1.3.3 데이터프레임에서 파생변수 만들기
    df_raw
    
    #var1 + var2의 합을 만들고싶다.
    df_raw$var_sum <- df_raw$var1 + df_raw$var2
    df_raw
    
    
install.packages("ggplot2")
library(ggplot2)
mpg <- as.data.frame(ggplot2::mpg)

#Q1. MPG 데이터를 탐색해보아라.

str(mpg)
View(mpg)
summary(mpg)
dim(mpg)

#Q2 cty를 city로 hwy를 highway로 변수명을 수정하라
mpg <- rename(mpg, city = cty)
mpg <- rename(mpg, highway = hwy)

#Q3 (city + highway) / 2 = total이라는 파생변수를 추가하라
mpg$total <- (mpg$city + mpg$highway) / 2
View(mpg)    

#Q4 total이 30이상이면 A등급 25이상이면 B등급 
#   20이상이면 C등급 그외는 D등급으로 구분한
#   gread라는 파생변수를 만들어라


mpg$grade <- ifelse(mpg$total >= 30, "A",
                    ifelse(mpg$total >= 25 ,"B",
                           ifelse(mpg$total >= 20, "C","D")))    
View(mpg)
table(mpg$grade)
