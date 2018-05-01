body.csv

성별  
나이 
키 
가슴둘레 
허리둘레 
배둘레 
엉덩이둘레 
겨드랑둘레 
얼굴수직길이 
머리둘레 
골격근량 
체지방량 
체수분 
단백질 
무기질 
체지방률 
복부지방률 
기초대사량 
복부지방률평가 
대사량평가 

body <- read.csv("c:/r/body.csv", header = F)
body
str(body1)
# 라이브러리

library(e1071)              # 이 라이브러리 안에 svm이 있음

# na값 제거
body1 <- na.omit(body)

# 컬럼명 
colnames(body1) <- c("gender", "age", "height", "chest", "heory", "bae", "ass", "kyeo", 
                     "face_vertical", "head", "bone", "body_fat", "body_water", 
                     "protein", "mineral", "body_fat_per", "bae_fat_per", "work", "bae_fat_test",
                     "work_test")
str(body1)

# 랜덤시드 생성
set.seed(12345)

# 셔플(Shuffle)
body_ran <- body1[order(runif(12894)), ]


# 트레이닝셋 80%

body_train <- body_ran[1:10314, ]

# 테스트셋 20%

body_test <- body_ran[10315:12894, ]

# 선형SVM 훈련

body_svm <- svm(work_test~., data = body_train, kernel="linear")   

body_svm

# 모델 테스트
p <- predict(body_svm, body_test, type="class")

p

table(p, body_test[, 20])

# 분류 결과 확인
mean(p == body_test[, 20])


######################## knn ###############################
library(class)
str(kbody)
kbody <- na.omit(read.csv("c:/r/body.csv", stringsAsFactors = FALSE, h = F))
kbody$V1 <- ifelse(kbody$V1 == '³²', 0, 1)

normalize <- function(x) {
  return((x - min(x)) / (max(x) - min(x)))}

kbody_n[,1:18] <- as.data.frame(lapply(kbody[ , 1:18], normalize))

str(kbody_n[, 1:18])

nrow(kbody)

train_data <- kbody_n[-nrow(kbody),]

train_label <- kbody[-nrow(kbody), 19]

test_label <- kbody[nrow(kbody), 19]

test_data <- kbody_n[nrow(kbody), ]

k <- round(sqrt(nrow(kbody)))

knn(train_data, test_data, train_label, k, prob = T)

knn(train_data, test_data, train_label, k, prob = T)



################################파이썬에서 실행#####################################

## http://scikit-learn.org/stable/modules/svm.html#

from sklearn import svm, metrics
from sklearn.model_selection import train_test_split
import matplotlib.pyplot as plt
import pandas as pd

tbl = pd.read_csv("c:/data/bmi.csv")

label = tbl["label"]
w = tbl["weight"] / 100 # 최대 100kg라고 가정
h = tbl["height"] / 200 # 최대 200cm라고 가정
wh = pd.concat([w, h], axis=1)

# 학습 전용 데이터와 테스트 전용 데이터로 나누기 
data_train, data_test, label_train, label_test = train_test_split(wh, label)

# 데이터 학습하기 
clf = svm.SVC()
clf.fit(data_train, label_train)

# 데이터 예측하기 
predict = clf.predict(data_test)

# 결과 테스트하기 
ac_score = metrics.accuracy_score(label_test, predict)
cl_report = metrics.classification_report(label_test, predict)
print("정답률 =", ac_score)
print("리포트 =\n", cl_report)