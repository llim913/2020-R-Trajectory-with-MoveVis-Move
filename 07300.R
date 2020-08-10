library(nnet) #전방 전달 신경망 + 다향 로그선형모형
library(neuralnet)
library(tensorflow)
library(deepnet)
library(RcppDL)
library(rnn)
library(DEEPR)
library(devtools)
library(rlang)

# 1. 평택항 5월1일 데이터 -------------------------------------
# 1.1. 평택항 5월1일 데이터 가져오기 --------------------------
df <- read_csv("C:/Users/GDLSYSTEM/2890/df_0531_stpye.csv", col_names = TRUE) 
head(df)
#iris <- subset(df, select = -c(X1,X3,X4,X7,X10,X11,X13,X14))
#iris <- iris[-1,]
iris <- subset(df, select = -c(X1,stype,RECV_DT,ShipName,AREA_ID ,TRGT_SENSOR_KIND,Length,Beam,MSG_ID,LOC_ACCRCY))
head(iris)
iris$SHIP_ID  <- as.character(iris$SHIP_ID )
d <- table(iris$SHIP_ID)
head(d)
#440008090 - 8475,440022680 - 9958, 440022690 - 8189, 440041250 - 8394, 440012290 - 8626,
#,440022680 ,440022690 , 440041250 , 440012290
iris <- iris %>% filter(SHIP_ID ==  440008090 | SHIP_ID == 440022680  )# | SHIP_ID == 440022690 |SHIP_ID == 440041250 | SHIP_ID == 440012290 
#iris <- subset(iris,SHIP_ID == 440008090 )
#RECV_DT,ShipName,stype,TRGT_SENSOR_KIND,,MSG_ID,LOC_ACCRCY

d <- table(iris$SHIP_ID)
d
head(iris)







# 0. 환경설치 -------------------------------------https://statkclee.github.io/deep-learning/r-keras-iris.html
#devtools::install_github("rstudio/keras")
#install.packages('keras')
library(vctrs)
library(tidyverse)
library(dplyr)


# 1. 붓꽃 데이터 -------------------------------------
# 1.1. 붓꽃 데이터 가져오기 --------------------------
iris <- read_csv("http://archive.ics.uci.edu/ml/machine-learning-databases/iris/iris.data", col_names = FALSE) 
iris
d <- table(iris$X5)
d
# 1.2. 데이터프레임을 행렬로 변환 -------------------
iris[,5] <- as.numeric(as.factor(unlist(iris[,5]))) -1
iris <- as.matrix(iris)
dimnames(iris) <- NULL 


# 2. 데이터 전처리 ----------------------------------
## 2.1. 데이터 정규화 -------------------------------
iris_x <- normalize(iris[,1:4])

iris_mat <- cbind(iris_x, iris[,5])
head(iris_mat)


# 3. 딥러닝 모형 -----------------------------------
## 3.1. 훈련표본과 검증표본 ------------------------- 
ind <- sample(2, nrow(iris_mat), replace=TRUE, prob=c(0.67, 0.33))

### 모형 설계행렬
iris.training <- iris_mat[ind==1, 1:4]
iris.test <- iris_mat[ind==2, 1:4]

### 모형 예측변수
iris.trainingtarget <- iris_mat[ind==1, 5]
iris.testtarget <- iris_mat[ind==2, 5]

### One-Hot 인코딩: 훈련예측변수
iris.trainLabels <- to_categorical(iris.trainingtarget)

### One-Hot 인코딩: 검증예측변수
iris.testLabels <- to_categorical(iris.testtarget)

## 3.2. 모형 개발 ------------------------- 
set.seed(777)
### 3.2.1. 모형 초기화
model <- keras_model_sequential()

### 3.2.2. 모형에 계층 추가

# 4 inputs -> [8 hidden nodes] -> 3 outputs
model %>% 
  layer_dense(units = 8, activation = 'relu', input_shape = c(4)) %>% 
  layer_dropout(rate = 0.5) %>%   
  layer_dense(units = 3, activation = 'softmax') 

### 3.2.3. 모형 살펴보기
summary(model)

# get_config(model)
# get_layer(model, index = 1)
# model$layers
# model$inputs
# model$outputs

### 3.2.4. 모형 컴파일
model %>% compile(
  loss = 'categorical_crossentropy',
  optimizer = 'adam',  
  metrics = 'accuracy' 
)

### 3.2.5. 모형 적합
model %>% fit(       
  iris.training, 
  iris.trainLabels, 
  epochs = 500,  
  batch_size = 5, 
  validation_split = 0.1  
)

#----------------------------------------------------------------- 
### 3.2.6. 적합된 모형 시각화
history <- model %>% fit(
  iris.training, 
  iris.trainLabels, 
  epochs = 500,
  batch_size = 5,
  validation_split = 0.1
)

listviewer::jsonedit(history, model="view")

### 모형 수렴
plot(history$metrics$loss, main="Model Loss", xlab = "epoch", ylab="loss", col="blue", type="l",
     ylim=c(0,1))
lines(history$metrics$val_loss, col="green")
legend("topright", c("train","test"), col=c("blue", "green"), lty=c(1,1))

### 모형 정확성
plot(history$metrics$acc, main="Model Accuracy", xlab = "epoch", ylab="accuracy", col="blue", type="l",
     ylim=c(0,1))
lines(history$metrics$val_acc, col="green")
legend("bottomright", c("train","test"), col=c("blue", "green"), lty=c(1,1))

#----------------------------------------------------------------------------
### 3.2.7. 검증표본을 통한 평가
#tibble::as_tibble()

pred_mat <- model %>% predict(iris.test, batch_size = 5) %>% as_tibble()

pred_mat <- pred_mat %>% 
  mutate(max_prob = max.col(., ties.method = "last")-1)

### 오차 행렬(Confusion Matrix)
table(iris.testtarget, pred_mat$max_prob)


### 모형 정확도 평가
score <- model %>% 
  evaluate(iris.test, iris.testLabels, batch_size = 128)

print(score)






