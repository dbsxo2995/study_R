#SVM
library(e1071)
library(kernlab) #SVM 피쳐선택을 위한 패키지

#step1. data load & data handlings
pima <- read.csv("pima.csv")
str(pima); summary(pima)

class <- pima$type
library(dplyr)
pima_scale <- scale(pima[,2:8], center = T, scale = T) %>% data.frame(); pima_scale$class <- pima$type
pima_scale %>% summary()

#step2. train & test set
idx <- sample(1:2,
              size = nrow(pima_scale),
              prob = c(0.7,0.3), replace = T)
train_set <- pima_scale[idx == 1, ]
test_set <- pima_scale[idx == 2, ]

#step3. model
#3.1 polynomial
poly_svm <- tune.svm(class ~., data = train_set, 
                     kernel = "polynomial",
                     degree = seq(2, 5),           #polynomial 커널함수의 차수
                     coef0 = seq(0.1,2, by = 0.2)) #polynomial 커널함수의 계수

best_poly_svm <- poly_svm$best.model

#3.2 radial
radial_svm <- tune.svm(class ~., data = train_set,
                       kernel = "radial", gamma = seq(0.1, 4, 0.3))

best_radial_svm <- radial_svm$best.model

#3.3 sigmoid
sigmoid_svm <- tune.svm(class ~., data = train_set,
                        kernel = "sigmoid", gamma = c(seq(0.1, 4, 0.1)), coef0 = c(0.1, 4, 0.1))

best_sigmoid_svm <- sigmoid_svm$best.model

#step4. model test
#poly
poly_test <- predict(best_poly_svm, newdata = test_set)
table(poly_test, test_set$class)
library(caret)
confusionMatrix(data = poly_test, 
                reference = test_set$class, positive = "Yes")

#radial
radial_test <- predict(best_radial_svm, newdata = test_set)
table(radial_test, test_set$class)
confusionMatrix(data = radial_test, 
                reference = test_set$class, positive = "Yes")
#sigmoid
sigmoid_test <- predict(best_sigmoid_svm, newdata = test_set)
table(sigmoid_test, test_set$class)

confusionMatrix(data = sigmoid_test, 
                reference = test_set$class, positive = "Yes")


#step5. 칼럼 선택하기(options)
library(caret)
control <- rfeControl(functions = lrFuncs, #back
                      method = "cv",
                      number = 10
                      )
svm_col <- rfe(x = train_set[, 1:7], 
               y = train_set[, 8],
               sizes = c(4,5,6,7),
               rfeControl = control, 
               method = "svmsigmoid") #svmLinear, svmPloy....


#svm2
#step1. data load & data handlings
library(e1071)
cancer <- read.csv("cancer.csv")
cancer <- cancer[, -1] #ID remove
str(cancer)

#step2. train & test set
idx <- sample(x = 1:2,
              size = nrow(cancer),
              prob = c(0.7,0.3),
              replace = T)
train_set <-cancer[idx == 1, ]
test_set <- cancer[idx == 2, ]

#step3. home work



###############################
#########의사결정나무##########
##############################

bank <- read.csv("UniversalBank.csv")
bank <- bank[, -c(1,5)] #id, zip

#step1. 목표변수 설정
bank$PersonalLoan <- as.factor(bank$PersonalLoan)
table(bank$PersonalLoan)

#step1.1 목표변수 레이블 변경 
bank$PersonalLoan <- relevel(bank$PersonalLoan, ref = "1")
table(bank$PersonalLoan)


#step2. train & test set
idx <- sample(x = 1:2,
              size = nrow(bank),
              prob = c(0.7, 0.3),
              replace = T)

train_set <- bank[idx == 1,]
test_set <- bank[idx ==2,]

table(train_set$PersonalLoan)
table(test_set$PersonalLoan)

#step3. 의사결정나무
library(rpart)
d_tree <- rpart(formula = PersonalLoan~.,                #목표변수와 입력변수 간 관계식
                data = train_set,                        #훈련데이터
                method = "class",                        #class(분류모형)
                parms = list(split = "gini"),            #gini를 기준
                control = rpart.control(minsplit = 20,   #특정 마디의 관측값이 20개 이하가 되면 분리안함
                                        cp = 0.01,       #비용복잡도 값 기본디폴트는 0.01
                                        mmaxdepth = 10)) #뿌리마디의 깊이 최대값(디폴트는 30)

summary(d_tree)
#CP가 0.01일때 분리된 개수는 7이므로 끝마디는 8개
#교육, 소득, 가족 순으로 오분류율이 낮은 모형을 만드는데 크게 기여함
#첫뿌리에서는 3511관측값을 나누는ㄱ
print(d_tree)


library(rpart.plot)
rpart.plot(d_tree)


#step4. 가지치기 판단
printcp(d_tree) #비용복잡도 find cp
#xerror가 가장 작은 행이 어떤것인지 찾는다
#이예제에서는 0.01일때 nsplit 7이므로 적절하다
plotcp(d_tree)


#step5 모델평가
pred <- predict(object = d_tree,
                newdata = test_set,
                type = "class")

library(caret)
(tree_caret <-confusionMatrix(data = pred,
                              reference = test_set$PersonalLoan))

library(gmodels)
CrossTable(x = test_set$PersonalLoan,
           y = pred,
           prop.chisq = FALSE)

#step6 ROC
library(ROCR)

pred_obj <- prediction(predictions = as.numeric(pred),
                       labels = as.numeric(test_set$PersonalLoan))

perfrom <- performance(prediction.obj = pred_obj,
                       measure = "tpr",
                       x.measure = 'fpr')
plot(perfrom, main = "ROC curve")

library(pROC)
auc(as.numeric(test_set$PersonalLoan),  as.numeric(pred))



