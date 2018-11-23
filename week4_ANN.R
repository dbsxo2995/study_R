#ANN
#step1 data load & scale
set.seed(12345)
con <- read.csv("concrete.csv")
str(con)
summary(con)

min_max_function <- function(x){
  return((x - min(x)) / (max(x) - min(x)))
}

con_norm <- as.data.frame(lapply(con, FUN = min_max_function))

summary(con_norm$strength) #[0,1] mim-max
summary(con$strength)

#step2 train & test
#idx <- sample(1:2, size = nrow(con), replace = T, prob = c(0.7,0.3))
#train_set <- con[idx == 1, ]
#test_set <- con[idx == 2, ]
train_set <- con_norm[1:773,]
test_set <- con_norm[774:1030,]

#step3 ANN
library(neuralnet)

#ann <- neuralnet(formula, data, hidden) 
#predict <- comput(ann, test)
head(train_set)
ann_model <- neuralnet(strength ~ cement + slag +  water+ash + superplastic + coarseagg + fineagg + age,
                       data = train_set,
                       hidden =1)
#plot(ann_model) #하나의 은닉노드와 콘크리트 강도를 예측하는 하나의 출력노드로 구성, 숫자1은 각각의 bias

predict_ann <- compute(ann_model, test_set[,1:8])$net.result
cor(predict_ann, as.matrix(test_set$strength))

#step4 모델 개선
ann_model_new <- neuralnet(strength ~ cement + slag +  water+ash + superplastic + coarseagg + fineagg + age,
                       data = train_set,
                       hidden =10)

plot(ann_model_new) #MSE 1.08 모델 복잡도 56000
predict_ann_new <- compute(ann_model_new, test_set[,1:8])$net.result
cor(predict_ann_new, as.matrix(test_set$strength))

#ANN2 
#step1 data load & scale
ctgs_data <- read.csv("ctgs.csv")
str(ctgs_data)
ctgs_data$NSP %>% table()
summary(ctgs_data)

nrow_data <- dim(ctgs_data)[1]; ncol_data <- dim(ctgs_data)[2]
table(ctgs_data$NSP)

ctgs_input <- ctgs_data[,-ncol_data]; ctgs_input <- scale(ctgs_input, center = T, scale = T)
ctgs_class <- ctgs_data[, ncol_data ]#target var is NSP
ctgs_class <- as.factor(ctgs_class)

full_data_scale <- data.frame(ctgs_input, class = ctgs_class)

#step2. train & test
set.seed(12345)
idx <- sample(1:2, 
              size = nrow_data,replace = T, prob = c(0.7,0.3))
train_set <- full_data_scale[idx == 1, ]
test_set <- full_data_scale[idx == 2, ]

#step3 ann : class 3개이므로 데이터 변형 
library(nnet)
library(neuralnet)

ann_train_input <- train_set[, -ncol_data]
ann_train_target <- class.ind(train_set[,ncol_data]) #class.ind 역할 확인 nnet

ann_test_input <- test_set[, -ncol_data]
ann_test_taget <- class.ind(test_set[,ncol_data])


#step3.1  최적 은닉 노드 탐색 - 5-fold cross Validation 
n_hidden_node <- seq(5, 30, 5)
validation_idx <- sample(1:5, 
                   size =  nrow(ann_train_input),
                   replace = T,
                   prob = c(0.2, 0.2, 0.2, 0.2, 0.2))
#sample(1:5, size =  dim(ann_train_input)[1], replace = T, prob = rep(0.2,5)) 5-fold coross_validations
validation_performance <- matrix(0,
                                  nrow = length(n_hidden_node),
                                  ncol = 3) #colnames(n_hiden, ACC, BCR)
performance_evaluate_multi <- function(x){
  ACC = sum(diag(x))/sum(x) 
  # Balanced Correction Rate
  BCR = 1
  for (i in 1:nrow(x)){
    BCR = BCR * (x[i,1] / sum(x[i,]))
  }
  
  BCR = BCR^(1/dim(x)[1])
  
  return(c(ACC, BCR)) 
}

for(i in 1:length(n_hidden_node)){
  evaluate_fold <- c()
  for(j in 1:5){
    #train k fold
    train_input_fold <- ann_train_input[validation_idx != j, ]
    train_taget_fold <- ann_train_target[validation_idx !=j, ]
    train_nnet <- nnet(x = train_input_fold,
                       y = train_taget_fold, size = n_hidden_node[i], decay = 5e-4, maxit = 10)
    
    #train k fold SAVE
    evaluate_fold <- rbind(evaluate_fold, 
                           cbind(max.col(train_taget_fold),
                                 max.col(predict(train_nnet, train_input_fold))))
  }
  confusion_matrix <- table(evaluate_fold[,1], evaluate_fold[,2])
  validation_performance[i, 1] <- n_hidden_node[i]
  validation_performance[i, 2:3] <- performance_evaluate_multi(confusion_matrix)
}
validation_performance <- validation_performance[order(validation_performance[,3], decreasing = T), ]
colnames(validation_performance) <- c("hidden", "ACC"," BCR")
validation_performance #size =5

#4. TEST ANN
ann <- nnet(ann_train_input, ann_train_target, size =5, dacay = 5e-4, maxit = 20)


#5. Performance
predict_obj <- predict(ann, ann_test_input)
test_confusion <- table(max.col(ann_test_taget), max.col(predict_obj))
fianl <- performance_evaluate_multi(test_confusion)
names(fianl) <- c("ACC","BCR")
fianl
