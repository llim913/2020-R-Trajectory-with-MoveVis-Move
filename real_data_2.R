library(nnet) #전방 전달 신경망 + 다향 로그선형모형
library(neuralnet)
#install_tensorflow(version = "2.0")
library(tensorflow)
library(deepnet)
library(RcppDL)
library(rnn)
library(DEEPR)
library(devtools)
library(rlang)
library(sys)
#install.packages('reticulate')
library(reticulate)
sessionInfo()


# 0. 환경설치 -------------------------------------https://statkclee.github.io/deep-learning/r-keras-iris.html
#devtools::install_github("rstudio/keras")
#install.packages('keras')
library(keras)
library(vctrs)
library(tidyverse)
library(dplyr)


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
#440008090 - 8475
#440022680 - 9958
#440022690 - 8189
#440041250 - 8394
#440012290 - 8626
#,440022680 ,440022690 , 440041250 , 440012290
iris <- iris %>% filter(SHIP_ID ==  440008090 | SHIP_ID == 440022680  )# | SHIP_ID == 440022690 |SHIP_ID == 440041250 | SHIP_ID == 440012290 
#iris <- subset(iris,SHIP_ID == 440008090 )
#iris$X5 <- as.numeric(iris$X5)
#iris$X12  <- as.numeric(iris$X12 )
#iris$X6   <-as.numeric(iris$X6   )
#iris$X8 <-as.numeric(iris$X8 )
#iris$X9   <-as.numeric(iris$X9  )
#RECV_DT,ShipName,stype,TRGT_SENSOR_KIND,,MSG_ID,LOC_ACCRCY

d <- table(iris$SHIP_ID)
d
head(iris)


# 1.2. 데이터프레임을 행렬로 변환 -------------------
iris[,1] <- as.numeric(as.factor(unlist(iris[,1]))) -1
iris <- as.matrix(iris)
dimnames(iris) <- NULL #일부터 null값을 집어 넣는다.

head(iris)
#행렬의 2/3를 훈련데이터, 1/3을 검증데이터로 활용한다.
#nomalize함수로 정규화하고, 통계에서는 가변수, dummy variable 통해 범주형 데이터(원핫인코딩 방법)로 0,1의 벡터 변환시킨다.
# 2. 데이터 전처리 ----------------------------------
## 2.1. 데이터 정규화 -------------------------------
iris_x <- normalize(iris[,2:4])
head(iris_x)
iris_mat <- cbind(iris_x, iris[,1])
head(iris_mat)


# 3. 딥러닝 모형 -----------------------------------
## 3.1. 훈련표본과 검증표본 ------------------------- 
ind <- sample(2, nrow(iris_mat), replace=TRUE, prob=c(0.67, 0.33))
head(iris_mat)
### 모형 설계행렬
iris.training <- iris_mat[ind==1, 1:3]
iris.test <- iris_mat[ind==2, 1:3]
head(iris.training)
### 모형 예측변수
iris.trainingtarget <- iris_mat[ind==1, 4]
iris.testtarget <- iris_mat[ind==2, 4]

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
  layer_dense(units = 6, activation = 'relu', input_shape = c(3)) %>% #4개가 들어가도록 설정 , relu 는 활성화 함수
  layer_dropout(rate = 0.5) %>%   # 모형 단순화
  layer_dense(units = 2, activation = 'softmax') # 입력계층 하나 출력계층 하나, softmax는 는 활성화 함수

### 3.2.3. 모형 살펴보기
summary(model)

#get_config(model)
#get_layer(model, index = 1)
#model$layers
#model$inputs
#model$outputs

### 3.2.4. 모형 컴파일
model %>% compile(
  loss = 'categorical_crossentropy', # 신경망 아키텍처 지정후 손실
  optimizer = 'adam',  # 최적화
  metrics = 'accuracy') #측도
#model %>% compile(
#  loss = 'sparse_categorical_crossentropy', # 신경망 아키텍처 지정후 손실
#  optimizer = 'adam',  # 최적화
#  metrics = 'accuracy') #측도
#y_binary = to_categorical(y_int)

#devtools::install_github("rstudio/reticulate")
#distributed_function()
#os.environ["CUDA_VISIBLE_DEVICES"] = '1'
### 3.2.5. 모형 적합
model %>% fit(       # 모형 학습
  iris.training, 
  iris.trainLabels, 
  epochs = 500,  # 몇회 학습?
  batch_size = 32,  # 배치 크기
  validation_split = 0.1  #검증은 어떻게 나눌것 인지
)

#----------------------------------------------------------------- 
#과대학습, 과소 학습, 훈련표본 검증표본의 소실과 정확도 시각적으로 확인하기
### 3.2.6. 적합된 모형 시각화
history <- model %>% fit(
  iris.training, 
  iris.trainLabels, 
  epochs = 500,
  batch_size = 32,
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
#실제 성능 검사에 앞서 검증표본으로 남겨놓은 iris.test 데이터로 정확도를 추정한다.
### 3.2.7. 검증표본을 통한 평가
tibble::as_tibble()

pred_mat <- model %>% predict(iris.test, batch_size = 5) %>% as_tibble()

pred_mat <- pred_mat %>% 
  mutate(max_prob = max.col(., ties.method = "last")-1)

### 오차 행렬(Confusion Matrix)
table(iris.testtarget, pred_mat$max_prob)


### 모형 정확도 평가
score <- model %>% 
  evaluate(iris.test, iris.testLabels, batch_size = 128)

print(score)




