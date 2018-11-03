# Week1-1 07.10

#1.1. 변수

    x1 <- c(1,2,3,4,5)
    x1 =  c(1,2,3,4,5)
    
    x2 <- c("안녕하세요")


#1.2 x1, x2 벡터 탐색
    
    class(x1)
    class(x2)
    
    is.numeric(x1)
    is.character(x2)
    is.vector(x1)
    
    length(x1)
    length(x2)
    
#1.3 기본연산
    x3 <- c(1,3,10)
    
    sqrt(x3)
    min(x3)
    max(x3)
    mean(x3)
    
    y3 <- -2 + x3
    plot(x3, y3)
    
#1.4 함수 및 벡터제어
    
    log10(10)
    log(10)
    exp(log(10))
    sin(pi / 2)
    
    
    ls()
    rm(x1)
    
    x4 <- "hEllO WoRLD"
    tolower(x4)
    toupper(x4)
    
#1.5 패키지 설치
    install.packages("ggplot2")
    install.packages('e1071')
    
#1.6 벡터 생성 및 탐색2
    x6 <- c(1,3,5,7,8)
    x6[3]
    x6[-1]
    x6[1:2]
    
    
    y6 <- seq(0, 10, by = 2)
    y6 <- seq(0, 10, length = 20)
    
    k1 <- rep(1:4, times = 2)
    k1
    k1 <- rep(1:4, times = 5, each = 2)
    k1
#1.7 백터 결합(행과 열)
    x7 <- c(1,3,5,7,9)
    c7 <- c(2,4,7,8,10)
    
    c <- cbind(x7, c7)
    r <- rbind(x7, c7)
    
    colnames(c) <- c("A","b")
    c

    #1.8 행렬의 생성
    m1 <- matrix(1:10, nrow = 2)
    m1 <- matrix(1:6, ncol = 3)
    m1 <- matrix(1:6, nrow = 2, byrow = T)
    
    m1 <- matrix(1:12,
                 nrow = 3,
                 dimnames = list(c("R1","R2","R3"), c("C1","C2","C3","C4"))
                 )
    m1
  
  
#1.9 NA(Not Available) & NULL
    one <- 30
    two <- 40
    three <- NA
    
    is.na(three)
    is.null(NA)
    
    
'''
Q1 ~ Q2. 다음 행렬을 만들라.

1 2 3
4 5 6
7 8 9

1 4 7
2 5 8
3 6 9
'''
rbind(c(1,2,3), c(4,5,6), c(7,8,9))
cbind(c(1,2,3), c(4,5,6), c(7,8,9))

'''
Q3. 다음 행렬에서 7번째 값을 확인하라.
                  첫번째 행을 확인하라.
                  두번째에서 4번째 열을 출력하라.
                  2번째열만 빼고 출력하라
    m1 <- matrix(1:12,
                 nrow = 3,
                 dimnames = list(c("R1","R2","R3"), c("C1","C2","C3","C4"))
                 )
'''

m1[7]
m1[1,]
m1[,4]
m1[,-2]

#1.10 List (key, value) format

    x10 <- list(name = "foo", height = 70)
    x10 <- list(name = "foo", height = c(1,2,3))
    
    x10 <- list(
      a = list(name = "foo", height = 70),
      b = list(name = "tow", height = 50)
    )
    
    x10$a$name


'''
Q4.  리스트를 활용해서 이름은 민호, 주소는 서울, 전화번호는
     1234, 능력치는 30인 자료를 생성해보라. 단 전화번호는 숫자가 아니라 문자다.

'''

list(name = "minho", address = "seoul", tel = "1234", ablity = 30)


