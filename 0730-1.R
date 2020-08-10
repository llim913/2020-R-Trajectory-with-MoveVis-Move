#install.packages('nnet')
#install.packages('neuralnet')
#install.packages('tensorflow')
#install.packages('deepnet')
#install.packages('RcppDL')
#install.packages('rnn')
#install.packages('DEEPR')
#install.packages('devtools')
#install.packages('tidyverse')
#install.packages('vctrs')
#install.packages('rlang')
library(nnet) #전방 전달 신경망 + 다향 로그선형모형
library(neuralnet)
library(tensorflow)
library(deepnet)
library(RcppDL)
library(rnn)
library(DEEPR)
library(devtools)
library(rlang)
#remove.packages('vctrs')
#install.packages("vctrs",dependencies = TRUE)
#remove.packages('tidyverse')
#install.packages("tidyverse",dependencies = TRUE)

#update.packages(cheackBuilt = TRUE,ask = FALSE)
sessionInfo()

#remove.packages('rlang')
#install.packages("tidyverse",dependencies = TRUE)



# 0. 환경설치 -------------------------------------https://statkclee.github.io/deep-learning/r-keras-iris.html
#devtools::install_github("rstudio/keras")
install.packages('keras')
library(keras)
library(vctrs)
library(tidyverse)
library(dplyr)
cc <-read_csv("http://archive.ics.uci.edu/ml/machine-learning-databases/iris/iris.data", col_names = FALSE) 

# 1. 붓꽃 데이터 -------------------------------------
# 1.1. 붓꽃 데이터 가져오기 --------------------------
iris <- read_csv("http://archive.ics.uci.edu/ml/machine-learning-databases/iris/iris.data", col_names = FALSE) 
iris

# 1.2. 데이터프레임을 행렬로 변환 -------------------
iris[,5] <- as.numeric(as.factor(unlist(iris[,5]))) -1
iris <- as.matrix(iris)
dimnames(iris) <- NULL #일부터 null값을 집어 넣는다.
iris

#행렬의 2/3를 훈련데이터, 1/3을 검증데이터로 활용한다.
#nomalize함수로 정규화하고, 통계에서는 가변수, dummy variable 통해 범주형 데이터(원핫인코딩 방법)로 0,1의 벡터 변환시킨다.
# 2. 데이터 전처리 ----------------------------------
## 2.1. 데이터 정규화 -------------------------------
iris_x <- normalize(iris[,1:4])
iris_x
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
  layer_dense(units = 8, activation = 'relu', input_shape = c(4)) %>% #4개가 들어가도록 설정 , relu 는 활성화 함수
  layer_dropout(rate = 0.5) %>%   # 모형 단순화
  layer_dense(units = 3, activation = 'softmax') # 입력계층 하나 출력계층 하나, softmax는 는 활성화 함수

### 3.2.3. 모형 살펴보기
summary(model)

# get_config(model)
# get_layer(model, index = 1)
# model$layers
# model$inputs
# model$outputs

### 3.2.4. 모형 컴파일
model %>% compile(
  loss = 'categorical_crossentropy', # 신경망 아키텍처 지정후 손실
  optimizer = 'adam',  # 최적화
  metrics = 'accuracy' #측도
)

### 3.2.5. 모형 적합
model %>% fit(       # 모형 학습
  iris.training, 
  iris.trainLabels, 
  epochs = 500,  # 몇회 학습?
  batch_size = 5,  # 배치 크기
  validation_split = 0.1  #검증은 어떻게 나눌것 인지
)

#----------------------------------------------------------------- 
#과대학습, 과소 학습, 훈련표본 검증표본의 소실과 정확도 시각적으로 확인하기
### 3.2.6. 적합된 모형 시각화
history <- model %>% fit(
  iris.training, 
  iris.trainLabels, 
  epochs = 500,
  batch_size = 5,
  validation_split = 0.1
)

install.packages('listviewer')
library(listviewer)
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






