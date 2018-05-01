1 단계 : 데이터 수집
 Pregnancies             : int  4 5 1 2 1 1 1 5 1 2 ...
 Glucose                 : int  99 88 114 91 88 172 144 124 97 89 ...
 BloodPressure           : int  72 78 66 62 62 68 82 74 66 90 ...
 SkinThickness           : int  17 30 36 0 24 49 46 0 15 30 ...
 Insulin                 : int  0 0 200 0 44 579 180 0 140 0 ...
 BMI                     : num  25.6 27.6 38.1 27.3 29.9 42.4 46.1 34 23.2 33.5 ...
 DiabetesPedigreeFunction: num  0.294 0.258 0.289 0.525 0.422 0.702 0.335 0.22 0.487 0.292 ...
 Age                     : int  28 37 21 22 23 28 46 38 22 42 ...
 Outcome                 : int  0 0 0 0 0 1 1 1 0 0 ...


2 단계  : 데이터 준비

diabets <- read.csv("c:/r/diabetes.csv", header = T, stringsAsFactors = F)
str(diabets)

## 난수
set.seed(0)
diabets <- diabets[order(runif(768)), ]

## train data, test data 분류
nrow(diabets)*0.8
nrow(diabets)*0.2

d_train <- diabets[1:614, -9]
d_test <- diabets[615:768, -9]

d_train_label <- diabets[1:614, 9]
d_test_label <- diabets[615:768, 9]

## k값 지정
sqrt(nrow(diabets))    # k = 28


3단계 : 데이터로 모델 훈련

pred <- knn(d_train, d_test, d_train_label, k= 28, prob = TRUE)
pred

4 단계 : 모델 성능 평가 

install.packages("gmodels")
library(gmodels)

real <- diabets[615:768, 9]

CrossTable(x = pred, y = real, prop.chisq=T)                ## 예측값과 실제값의 교차표 생성

######################################## 표준화 #########################################

1 단계 : 데이터 수집
Pregnancies             : int  4 5 1 2 1 1 1 5 1 2 ...
Glucose                 : int  99 88 114 91 88 172 144 124 97 89 ...
BloodPressure           : int  72 78 66 62 62 68 82 74 66 90 ...
SkinThickness           : int  17 30 36 0 24 49 46 0 15 30 ...
Insulin                 : int  0 0 200 0 44 579 180 0 140 0 ...
BMI                     : num  25.6 27.6 38.1 27.3 29.9 42.4 46.1 34 23.2 33.5 ...
DiabetesPedigreeFunction: num  0.294 0.258 0.289 0.525 0.422 0.702 0.335 0.22 0.487 0.292 ...
Age                     : int  28 37 21 22 23 28 46 38 22 42 ...
Outcome                 : int  0 0 0 0 0 1 1 1 0 0 ...


2 단계  : 데이터 준비

diabets <- read.csv("c:/r/diabetes.csv", header = T, stringsAsFactors = F)
str(diabets)

## 난수
set.seed(0)
diabets <- diabets[order(runif(768)), ]

## train data, test data 분류
nrow(diabets)*0.8
nrow(diabets)*0.2


d_train <- diabets[1:614, -9]
d_test <- diabets[615:768, -9]


##정규화
normalize <- function(x) { 
                return ((x - min(x)) / (max(x) - min(x))) }

d_train_n <-as.data.frame(lapply(d_train, normalize))
d_test_n <- as.data.frame(lapply(d_test, normalize))

##표준화
d_train_z <- scale(d_train)
d_test_z <- scale(d_test)


##라벨 지정
d_train_label <- diabets[1:614, 9]
d_test_label <- diabets[615:768, 9]

## k값 지정
sqrt(nrow(diabets))    # k = 28


3단계 : 데이터로 모델 훈련

pred <- knn(d_train_n, d_test_n, d_train_label, k= 28, prob = TRUE)
pred


4 단계 : 모델 성능 평가 

install.packages("gmodels")
library(gmodels)

real <- diabets[615:768, 9]

CrossTable(x = pred, y = real, prop.chisq=T)             ## 예측값과 실제값의 교차표 생성


################################# SVM #####################################

library(e1071)

# 선형SVM 훈련
d_train <- diabets[1:614, ]    # train data
d_test <- diabets[615:768, ]   # test data

d_svm <- svm(Outcome~., data = d_train, kernel="linear")   
d_svm

# 모델 테스트
p <- predict(d_svm, d_test, type="class")
p

table(p, d_test[,9])

# 분류 결과 확인
mean(p == d_test[, 9])
