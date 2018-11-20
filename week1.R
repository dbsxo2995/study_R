#week 1-1. Last updeate. 11/03

#1.1 Variable
#1.1.1 Vector
  x1 <- c(1,2,3,4,5) #x1 = c(1,2,3,4,5)
  x2 <- c("Hello","world")

#1.1.2 Vector exploration 1
  class(x1); class(x2)
  is.numeric(x1); is.numeric(x2)
  is.character(x1); is.character(x2); is.element(1, x1)
  length(x1); length(x2)

#1.1.3. calculation
  x3 <- c(1, 3, 5)  #(x1 - x3) isn't Multiple
  x4 <- c(5,4,3,2,1) 
  
  union(x1,x2); intersect(x1,x2); setdiff(x1,x3) #set cacluation is possible
  
  sqrt(x3); min(x3); max(x3); mean(x3); class(x3); median(x3); abs(x3); 
  log10(10); log(10); exp(log(10)); sin(pi / 2)
  tolower(x2); toupper(x2)
  sort(x4); sort(x1, decreasing = T); range(x1);
  
  replace(x4, c(4,5), c(100,100))
  append(x1,x4); append(x1, x4, after = 2)
  
#tip 1
  ls() #we get variable in global Environment
  rm() #remove Variable in global Environment  
  #install.packages("packages name")
  #library(packages name)
  
#tip 2
  3 %% 2   #나눈 나머지
  3 %/% 2  #정수 나머지
  cat(" hello world \n", "nice meet you \t", "Thank you")
  

  #1.1.4. Vector exploration 2
  x6 <- c(1, 3, 5, 7, 9)
  x6[1]; x6[-1]; x6[1:3]; x6[-1:-2]; x6[1:length(x6)]; x6[min(x6):3]
  
  y <- seq(0, 10, by = 2); y        
  y1 <- seq(0, 10, length = 20); y1 #interval 
  
  y2 <- rep(1:4, times = 5); y2
  y3 <- rep(1:4, each = 2); y3
  y4 <- rep(0:4, times = 2, each = 2); y4
  
#1.1.5 Matrix 1
  c1 <- c(1,2,3)
  c2 <- c(4,5,6)
  c3 <- c(7,8,9)
  
  matrix_c <- cbind(c1,c2,c3); matrix_c
  matrix_r <- rbind(c1,c2,c3); matrix_r
  
  colnames(matrix_c) <- c("FIRST","SECOND","THIRD"); #rownames()
  
#1.1.5 Matrix 2 
  matrix_1 <- matrix(1:10, nrow= 2); matrix_1 #col standard
  matrix_2 <- matrix(1:6, ncol = 2); matrix_2
  matrix_3 <- matrix(1:12,
                     nrow = 3,
                     dimnames = list(c("row1","row2","row3"),
                                     c("col1","col2","col3","col4"))); matrix_3
  matrix_3 <- matrix(1:12,
                     nrow = 3,
                     dimnames = list(c("row1","row2","row3"))); matrix_3
  
#1.1.5 Matrix 3 
  vector <- c("ID",1,2,3,"abc","5")
  matrix_4 <- matrix(vector, nrow = 2, byrow = T); matrix_4 #row standard
  
  #1.1.6. Matrix calculation
  dim(matrix_3);
  matrix_3
  
  #apply(X = , MARGIN = , FUN = )
  apply(matrix_3, 1, max); apply(matrix_3, 2, max) #MARGIN = 1 is row and 2 is col
  apply(matrix_3, 1, sum); apply(matrix_1, 1, mean)
  
  
  
  matrix_3 * 3; 
  matrix_3[1,2]; #matrix_3[row, col]
  
  
 
 "Q1 ~ Q2. 다음 행렬을 만들라.
  
  1 2 3
  4 5 6
  7 8 9
  
  1 4 7
  2 5 8
  3 6 9"
  
  
  rbind(c(1,2,3), c(4,5,6), c(7,8,9))
  cbind(c(1,2,3), c(4,5,6), c(7,8,9))
  
  "
  Q3. 다음 행렬에서 7번째 값을 확인하라.
  첫번째 행을 확인하라.
  두번째에서 4번째 열을 출력하라.
  2번째열만 빼고 출력하라"
  
  m1 <- matrix(1:12,
  nrow = 3,
  dimnames = list(c("R1","R2","R3"), c("C1","C2","C3","C4"))
  )
  
  
  m1[7]
  m1[1,]
  m1[,4]
  m1[,-2]
  
#1.1.7 List (key, value) format
  
  x10 <- list(name = "foo", height = 70)
  x10 <- list(name = "foo", height = c(1,2,3))
  
  x10 <- list(
    a = list(name = "foo", height = 70),
    b = list(name = "tow", height = 50)
  )
  
  x10$a$name
  
"  

Q4.  리스트를 활용해서 이름은 민호, 주소는 서울, 전화번호는
     1234, 능력치는 30인 자료를 생성해보라. 단 전화번호는 숫자가 아니라 문자다.

"
  
  number <- list(name = "minho", address = "seoul", tel = "1234", ablity = 30)  


#1.1.8 Vector names
  gender <- c(0, 1)
  names(gender) <- c("female","male"); gender; length(gender)


#1.1.9 Categorical Variables : Factor in R
  factor(x = ,levels = , ordered = )
  
  size <- c("S","M","L","XL"); class(size)
  
  size_factor <- factor(size); size_factor #define size as a factor
  class(size_factor); nlevels(size_factor);
  
  size_factor <- factor(size, levels = c("S", "M","L","XL")); size_factor
  size_factor <- factor(size, levels = c("S", "M","L","XL"), ordered = T); size_factor
  
  
#1.1.10 One Variable Function 
  #Suqare Function
  square_function <- function(x){
    cat("Answer is\n")
    return(x * x)
  }
  
  square_function(3); square_function(1:3)
  
  #Plus Function
  Plus_function <- function(x){
    return(x + x + 1)
  }
  Plus_function(1:3)
  
#1.1.11. Two variable Function
  two_funcntion <- function(x, y){
    xx <- x
    yy <- y
    return(sum(xx, yy))
  }
  two_funcntion(3,5)


#1.1.12. control statemen 1.... if
  #if (conditions) {True order statement} else {False order statement}
  
  x <- runif(1); x
  if(x < 0.5) {cat(1 - x)} else {cat(x)} #print
  
  y <- 1
  if (y == 1 | y == 2) print(y + 1) else print(y + 10)
"  
  ifelse(conditions, TOS,
         ifelse(condtion, TOS, FOS))
  "
  ifelse(x >= 0.7, "A",
          ifelse(x >= 0.6, "B", "C"))
  
#Q3. fibonachi 수열 만들기 1 1 2 3 5 8
  
  fibo <- function(n){
    ifelse (n == 1 | n == 2, return(1),
            ifelse(n > 1, return(fibo(n-1) + fibo(n-2))))
  }
  fibo(8)
  
  
#1.1.13. control statemen 1.... for, while
  #for(i in 1:K){statement about i}
  #while(condition){TOS}
  
  for(i in 1:10){
    print(i)
  }
  
  for(i in 1:10){
    i = i + 0
    j = i
    print(j)
  }
  
  x <- c("a","b","c","d")
  for(i in seq(x)){
    print(x[i])
  }
  
  mat <- matrix(1:9, ncol = 3); mat
  sum <- 0
  for(i in seq(nrow(mat))){
    for(j in seq(ncol(mat))){
      sum <- sum + mat[i,j]
      print(sum)
    }
  }
  
  i = 0
  for(i in 1:30){
    if(i < 8){print(i)} else {print(i) 
    break
    }
  }

    
  i = 0
  while(i <= 10){
    print(i)
    i = i + 1
  }
  
  i <- 0
  while (i < 10){
    i = i + 1
    if (i %% 2 != 0){
      next
    }
    print(i)
  }
  
#Q4. 1부터 100까지 홀수만 출력해보자. for문을 사용해서
  for (i in 1:100){
    if (i %% 2 == 0){
      next
    }
    print(i)
  }  
  
  
#Q5. 합계가 55을 만드는 함수를 출력하라. for, while, repeat 사용하는경우
  sum <- 0
  for(i in seq(1, 10, 1)){
    sum <- sum + i
  }
  sum
  
  
  sum <-0
  i <- 1
  while(i <= 10){
    sum <- sum + i
    i = i + 1
  }
print(sum)

#1.1.14 iris data EDA
   data(iris)
   head(iris); tail(iris,5) #head(iris, 5); tail(iris, 5)
   str(iris); dim(iris);
   summary(iris)
   
   max(iris$Sepal.Length); min(iris$Sepal.Length)
   which.max(iris$Sepal.Length); which.min(iris$Sepal.Length)
   
   fivenum(iris$Sepal.Length)
   mean(iris$Sepal.Length); median(iris$Sepal.Length); sd(iris$Sepal.Length)
   
#1.1.15 data frame span
   x  <- data.frame(id = c("1","2","3","4","5"),
                    math = c(10,20,30,40,50),
                    eng = c(60,70,80,90,100), stringsAsFactors = F); x
   
   id <- c("1","2","3","4","5")
   math_score <- c(10,20,30,40,50)
   y <- data.frame(id = id, math = math_score ); y
   
#1.1.16. data frame 1
   x <- data.frame(a = c(1,2,3),
                   b = c("A", NA, "c"),
                   c = c("a","B", NULL, 3)); x
   
   na.fail(x)
   na.omit(x); na.exclude(x);
   na.pass(x)

#1.1.16-1 na.omit(x) vs na.exclude(x)
   df <- data.frame(x = 1:5,
                    y = seq(2, 10, 2)); df
   df[3,2] = NA; df
   
   resid(lm(y ~ x, 
            data = df,
            na.action = na.omit))
   resid(lm(y ~ x,
            data = df,
            na.action = na.exclude))
   
#1.1.17. data frame 2
   d <- data.frame(x = c(1,2,3,4,5),
                   y = c(6,7,8,"9",10),
                   z = c("M","F","M","M","F"))
   is.factor(d$z)
   d$z <- as.character(d$z)
   d$y <- as.numeric(d$y); class(d$y)

#1.1.18. data frame 3
   library(dplyr)
   df <- data.frame(var1 = c(1,2,3),
                    var2 = c(4,5,6))
   names(df) <- c("k",1); df
   
   rename(df, v1 = var1, v2= var2) #dplyr
   
   df$var_sum <- df$var1 + df$var2; df$var_sum
   
   

#Q6. MPG 데이터를 탐색해보아라.
   library(ggplot2)
   mpg <- as.data.frame(ggplot2::mpg)
   
   str(mpg)
   View(mpg)
   summary(mpg)
   dim(mpg)
   
#Q7 cty를 city로 hwy를 highway로 변수명을 수정하라
   mpg <- rename(mpg, city = cty)
   mpg <- rename(mpg, highway = hwy)
   
#Q8 (city + highway) / 2 = total이라는 파생변수를 추가하라
   mpg$total <- (mpg$city + mpg$highway) / 2
   View(mpg)    
   
#Q9 total이 30이상이면 A등급 25이상이면 B등급 
   #   20이상이면 C등급 그외는 D등급으로 구분한
   #   gread라는 파생변수를 만들어라
   
   
   mpg$grade <- ifelse(mpg$total >= 30, "A",
                       ifelse(mpg$total >= 25 ,"B",
                              ifelse(mpg$total >= 20, "C","D")))    
   table(mpg$grade)   

#단일품목 판매 통계적 모의실험
##################################################################
#한 신물팔이 소년이 신문 1부에 150원으로 사서 300원에 판매한다.###
#팔지못한 신문은 1부당 100원에 환불받으며, 돈을 벌고 있다. 이 때##
#10부 단위로 신문을 받아온다고 할 때, 매일 몇부씩 받아오는것이 ###
#최대의 이익이 될것인가? 다음은 100일동안 매일의 판매 부수를 조사#
#하여 확률 표로 나타내었다.                                      #
#X : 10 20 30 40 50 60 70 80 90 판매부수 #
#P(x) 4 6  12 20 25 18 10  3  2 단위(%)  #


result <- matrix(0, nrow = 9, ncol = 2)
sum_profit <- matrix(0, nrow = 9, ncol = 1)
colnames(result) <- c("구입","이익")
F <- c(0,00, 0,04, 0.10, 0.22, 0.42, 0.67, 0.85, 0.95, 0.98, 1.00) #누적확률
set.seed(12345)

for(i in 1:9){
  buy = 10 * i
  for(day in 1:1000){
    u = runif(1, 0, 1)
    for(j in 1:9){
      sale = 10 * j
      if(u >= F[j]){if(buy >= sale) profit = ((150 * sale) - (50 * (buy - sale))) else profit = 150 * buy}
    }
  }
  sum_profit[i] = sum_profit[i] + profit
  result[i, 1] <- buy
  result[i, 2] <- sum_profit[i]
}
result
