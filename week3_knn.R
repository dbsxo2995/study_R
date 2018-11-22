#KNN 1
library(kknn)

#step1. data load & scale
data <- read.csv("wdbc.csv", header = F)

class <- data[,31]; input_data <- data[,-31]
scale_input_data <- scale(input_data, center = T, scale = T)

#step2. Divide the dataset train & test
set.seed(123)
idx <- sample(1:2, 
              size = nrow(input_data), prob = c(0.7,0.3), replace = T)

train_set <- input_data[idx == 1, ]; train_set$class <- class[idx == 1]
test_set <- input_data[idx == 2, ]; test_set$class <- class[idx == 2]


#step3. KNN
kknn_model <- kknn(class ~.,                              #입력변수와 목표변수 간의 관계
                   train = train_set,
                   test = test_set,
                   k = 1,                                 #1, 맨하탄거리 2. 유클리디안
                   distance = 2,
                   kernel = "rectangular")                #커널함수 가중치

table(test_set$class, kknn_model$fitted.values)

#step4. Model test
library(caret)
confusionMatrix(data = test_set$class,                         #예측값 할당
                reference =  kknn_model$fitted.values)         #실제값 할당

#Sensitivity : 제대로 예측한 비율 긍정-긍정(best-bset) = Recall 
#Specificity : 제대로 예측한 비율 부정-부정(good-good) 
#Pos Pred Value = Precision : 긍정중에서 긍정으로 예측한 비율 TP / (TP + FP)

#step4.1 Model Test(optional)
library(ROCR)
pred_obj <- prediction(predictions = as.numeric(test_set$class), #예측값(단 숫자벡터) 
                       labels = kknn_model$fitted.values)        #실제값

perfrom <- performance(prediction.obj = pred_obj,
                       measure = "tpr",     #민감도
                       x.measure = "fpr")   #특이도
plot(perfrom, main = "ROC curve")
library(pROC)
auc(kknn_model$fitted.values, as.numeric(test_set$class))

#step 5.3 F1 score
library(MLmetrics)
F1_Score(y_pred =kknn_model$fitted.values, y_true = test_set$class)
(2*0.9516*0.9365 ) / (0.9516 + 0.9365)


#knn2
#step1 data road & scale
wine <- read.csv("winequality-white.csv", sep = ";")
str(wine)
summary(wine)
class(wine)

#step1.1 Traget Variable is Continous!!! then changed
#step1.1 목표변수 만들기
wine$grade <- ifelse(wine$quality >= 3 & wine$quality <= 6, "good","best")
table(wine$grade)

#step1.2 scale
library(dplyr)
wine_scale <- scale(x = wine[,1:11], center = T,scale = T) %>% as.data.frame()

#step2. train & test
idx <- sample(1:2, nrow(wine_scale), replace = T, prob = c(0.7,0.3))
train_set <- wine_scale[idx == 1, ]; train_set$class <- wine$grade[idx == 1]
test_set <- wine_scale[idx == 2,]; test_set$class <- wine$grade[idx == 2]

table(train_set$class); table(test_set$class)

#step3. 훈련데이터 비율 맞춰주기 (options)
library(ROSE)
train_set_bal <- ovun.sample(formula = class ~.,    #목표변수
                             data = train_set,      #훈련데이터
                             method = "both",       #over, under, both
                             p = 0.5,               #동일한 비중을 얻기 위해
                             seed =123)$data  
table(train_set_bal$class); table(test_set$class)
train_set_bal$class <- as.factor(train_set_bal$class)
test_set$class <- as.factor(test_set$class)
class(train_set_bal$class); class(test_set$class)

#step4. knn
library(kknn)
kknn_model <- kknn(class ~., train = train_set_bal, test = test_set, k = round(sqrt(nrow(train_set_bal))), kernel = "rectangular")
table(test_set$class, kknn_model$fitted.values)

#step5. Model test
library(caret)
confusionMatrix(data = test_set$class,                         #예측값 할당
                reference =  kknn_model$fitted.values)         #실제값 할당

#Sensitivity : 제대로 예측한 비율 긍정-긍정(best-bset) = Recall 
#Specificity : 제대로 예측한 비율 부정-부정(good-good) 
#Pos Pred Value = Precision : 긍정중에서 긍정으로 예측한 비율 TP / (TP + FP)

#step5.1 Model Test
library(ROCR)
pred_obj <- prediction(predictions = as.numeric(test_set$class), #예측값(단 숫자벡터) 
                       labels = kknn_model$fitted.values)        #실제값

perfrom <- performance(prediction.obj = pred_obj,
                       measure = "tpr",     #민감도
                       x.measure = "fpr")   #특이도
plot(perfrom, main = "ROC curve")
library(pROC)
auc(kknn_model$fitted.values, as.numeric(test_set$class))

#step 5.3 F1 score
library(MLmetrics)
F1_Score(y_pred =kknn_model$fitted.values, y_true = test_set$class)
(2*0.3994*0.8317 ) / (0.3994 + 0.8317)
