# Week1-1 07.10

#1.1 백터 이름 넣어주기
    
    gender<-c(0,1)
    names(gender)<-c("female", "male")
    gender
    length(gender)

#1.2 categorical varaiables : factor 범주형 데이터
    
    size<-c("S", "M","L","XL")
    class(size)

    # define size as a factor (categorical variable)
    size_factor<-factor(size)
    class(size_factor)
    nlevels(size_factor)


    size_factor2 <- factor(size, 
                           levels = c("S", "M","L","XL")) 
    size_factor2

    # give order for categorical variable
    size_factor3 <- factor(size, ordered = TRUE, 
                           levels = c("S", "M","L","XL")) 
    size_factor3


#1.2 Foctor 2
    
    sex <- factor("f", c("m", "f"))
    nlevels(sex)
    levels(sex) <- c("male", "female")
    
    
    a <- factor(c("f","f","g"),
                levels = c("f","g","h"),
                ordered = TRUE)


#1.3 Function
    
    #Square Function
    square <- function(x){
      return(x * x)
    }
    square(3)
    square(1:3)
    
    #Plus Function
    Plus <- function(x){
      return(x + 3)
    }

    #two variable
    two <- function(x, y){
      xx <- x
      yy <- y
      return(sum(xx, yy))
    }
    two(3, 5)
    
    #No variable
    zero <- function(){
      x <- seq(1, 10, by = 2)
      y <- square(x)
      return(x * y)
    }
    zero()

    #No result
    result <- function(){
      x <- 1
      y <- 2
      return(invisible(x * y))
    }
    solution <- result()
    solution
    
    #1.3.1 if문
        #if (조건) 명령문 else 명령문2
        #ifelse(조건, 명령문, ifelse(조건, 명령문, 명령문))
    
    x <- runif(1) #0과 1사이에 난수 생성
    set.seed(12345)
    
    if(x < 0.5) print(1 - x) else print(x)
    
    ifelse(x > 0.7, "A",
           ifelse(x >= 0.6, "B", "C"))
    
    y <- 1
    if (y == 1 | y == 2) print(y + 1) else print(y + 10)

    
#Q1. fibonachi 수열 만들기 1 1 2 3 5 8
    fibo <- function(n){
      if (n == 1 | n == 2){
        return(1)
      }
      return(fibo(n - 1) + fibo(n - 2))
    }
    fibo(6)
    

#1.4 반복문
    #for (i in data){ i를 사용한 문장}
    #while(조건문){조건이 참이면 수행할 문장}
    #repeat{반복해서 수행할 문장}
    
    #1.4.1 for문 1부터 10까지 출력
    for (i in 1:10){
      print(i)
    }
    
    #1.4.2 while문을 사용해서 1부터 10까지 출력
    i = 0
    while(i < 10){
      i = i + 1
      print(i)
    }
    
    #1.4.3. repeat문을 사용해서 1부터 10까지 출력
    i <- 1
    repeat{
      print(i)
      if (i == 10){
        break
      }
      i = i + 1
    }
    
    #1.4.4 while문을 짝수만 출력해보자
    i <- 0
    while (i < 10){
      i = i + 1
      if (i %% 2 != 0){
        next
      }
      print(i)
    }

#Q2. 1부터 100까지 홀수만 출력해보자. for문을 사용해서
    for (i in 1:100){
      if (i %% 2 == 0){
        next
      }
      print(i)
    }
#Q3. 합계가 55을 만드는 함수를 출력하라. for, while, repeat 사용하는경우
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
    
    
    sum <- 0
    i <- 1
    repeat{
      sum <- sum + i
      i <- i + 1
      if (i > 10) break
    }
    sum
    
    
    