body.csv

����  
���� 
Ű 
�����ѷ� 
�㸮�ѷ� 
��ѷ� 
�����̵ѷ� 
�ܵ���ѷ� 
�󱼼������� 
�Ӹ��ѷ� 
��ݱٷ� 
ü���淮 
ü���� 
�ܹ��� 
������ 
ü����� 
��������� 
���ʴ�緮 
����������� 
��緮�� 

body <- read.csv("c:/r/body.csv", header = F)
body
str(body1)
# ���̺귯��

library(e1071)              # �� ���̺귯�� �ȿ� svm�� ����

# na�� ����
body1 <- na.omit(body)

# �÷��� 
colnames(body1) <- c("gender", "age", "height", "chest", "heory", "bae", "ass", "kyeo", 
                     "face_vertical", "head", "bone", "body_fat", "body_water", 
                     "protein", "mineral", "body_fat_per", "bae_fat_per", "work", "bae_fat_test",
                     "work_test")
str(body1)

# �����õ� ����
set.seed(12345)

# ����(Shuffle)
body_ran <- body1[order(runif(12894)), ]


# Ʈ���̴׼� 80%

body_train <- body_ran[1:10314, ]

# �׽�Ʈ�� 20%

body_test <- body_ran[10315:12894, ]

# ����SVM �Ʒ�

body_svm <- svm(work_test~., data = body_train, kernel="linear")   

body_svm

# �� �׽�Ʈ
p <- predict(body_svm, body_test, type="class")

p

table(p, body_test[, 20])

# �з� ��� Ȯ��
mean(p == body_test[, 20])


######################## knn ###############################
library(class)
str(kbody)
kbody <- na.omit(read.csv("c:/r/body.csv", stringsAsFactors = FALSE, h = F))
kbody$V1 <- ifelse(kbody$V1 == '����', 0, 1)

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



################################���̽㿡�� ����#####################################

## http://scikit-learn.org/stable/modules/svm.html#

from sklearn import svm, metrics
from sklearn.model_selection import train_test_split
import matplotlib.pyplot as plt
import pandas as pd

tbl = pd.read_csv("c:/data/bmi.csv")

label = tbl["label"]
w = tbl["weight"] / 100 # �ִ� 100kg��� ����
h = tbl["height"] / 200 # �ִ� 200cm��� ����
wh = pd.concat([w, h], axis=1)

# �н� ���� �����Ϳ� �׽�Ʈ ���� �����ͷ� ������ 
data_train, data_test, label_train, label_test = train_test_split(wh, label)

# ������ �н��ϱ� 
clf = svm.SVC()
clf.fit(data_train, label_train)

# ������ �����ϱ� 
predict = clf.predict(data_test)

# ��� �׽�Ʈ�ϱ� 
ac_score = metrics.accuracy_score(label_test, predict)
cl_report = metrics.classification_report(label_test, predict)
print("����� =", ac_score)
print("����Ʈ =\n", cl_report)