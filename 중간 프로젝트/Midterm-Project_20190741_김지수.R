# 필요한 패키지를 불러오기
library(dplyr)
library(MASS)
library(corrplot)
library(nnet)
library(olsrr)
library(car)
library(caret)

# 와인 정보 데이터를 읽어오기
file_path = "C:/Users/admin/Desktop/wine.csv"
data = read.csv(file_path, h=T)
data = as.data.frame(data)

#데이터 서머리
head(data)
summary(data)

# 종속변수의 분포
hist(data[,12], main = "Distribution of Dependent Variable", xlab = "Dependent Variable", ylab = "Frequency", col = "skyblue")

#데이터에서 독립변수들 사이의 상관관계 확인(다중 공선성 점검)
cor_mat = cor(data[,-12])
corrplot(cor_mat, method = "number", type = "upper", 
         tl.cex = 0.8, tl.col = "black", 
         addCoef.col = "black", number.cex = 0.8)

#3등급(min) ~ 8등급(max)을 등급을 나타내는 factor로 바꿈. 등급이 높을 수록 좋은 것이라는 순서를 부여
data$quality <- factor(data$quality, ordered = TRUE)

# 모델링에 필요한 변수를 선택
selected_vars <- c("fixed_acidity", "volatile_acidity", "citric_acid", "residual_sugar", "chlorides", "free_sulfur_dioxide", "total_sulfur_dioxide", "density", "pH", "sulphates", "alcohol", "quality")
data_selected <- data[, selected_vars]


#선형모델은 와인의 품질이 이산점수로 나타나 있어 불가능하므로 순서형 로지스틱 회귀분석을 실시
#모델의 정확성 측정을 위해 학습 데이터와 테스트 데이터를 분리

index = 2:1200
train_set = data[index, ]
test_set = data[-index, ]
dim(train_set) 
dim(test_set)

train_set = as.data.frame(train_set)
test_set = as.data.frame(test_set)
quality_numeric = as.numeric(test_set$quality)

#Ordinal regression model (순서형 로지스틱 모델) method에 logistic, probit, loglog, cauchit, cloglog 방법이 있다. 그 중 probit 모델을 선정함.
ordinal_model = polr(quality ~ ., method = "probit", data = train_set, Hess=TRUE)
ordinal_model = stepAIC(ordinal_model, direction="both", trace = FALSE)
summary(ordinal_model)
Anova(ordinal_model, type = "II")

ordinal_model_revised = polr(quality ~ volatile_acidity + total_sulfur_dioxide + sulphates + alcohol, method = "probit", data = train_set, Hess=TRUE)
result2 = predict(ordinal_model, test_set) 
result3 = predict(ordinal_model_revised, test_set) 

# 예측된 클래스와 실제 클래스를 비교하여 정확도를 계산하고 출력
print(paste("Ordinal Model Accuracy:", mean(as.numeric(result2) == quality_numeric)))
print(paste("Revised Ordinal Model Accuracy:", mean(as.numeric(result3) == quality_numeric)))
summary(ordinal_model_revised)
Anova(ordinal_model_revised, type = "II")
print(vif(ordinal_model))
print(vif(ordinal_model_revised))


# 혼동행렬을 통한 정확도 확인
create_confusion_matrix <- function(actual, predicted) {
  
  all_classes <- sort(unique(c(actual, predicted)))
  tab <- table(factor(actual, levels = all_classes), factor(predicted, levels = all_classes))
  tab <- as.matrix(tab)
  colnames(tab) <- paste("Predicted", colnames(tab))
  rownames(tab) <- paste("Actual", rownames(tab))
  return(tab)
}

# Confusion Matrix 출력
conf_matrix <- create_confusion_matrix(quality_numeric, as.numeric(result3))
print("Confusion Matrix:")
print(conf_matrix)

# Accuracy 계산
accuracy <- sum(diag(conf_matrix)) / sum(conf_matrix)
print(paste("Accuracy:", accuracy))

# 다중 클래스 AUC
library(pROC)
roc_curve <- multiclass.roc(as.factor(quality_numeric), as.numeric(result3))
auc_values <- auc(roc_curve)
print(paste("Multiclass AUC:", auc_values))

# 클래스 3 vs 나머지 클래스
class_combined_3 <- ifelse(quality_numeric == 3, "Class 3", "Other")
roc_curve_3 <- roc(class_combined_3, as.numeric(result3), levels = c("Class 3", "Other"))
auc_value_3 <- auc(roc_curve_3)

# 클래스 4 vs 나머지 클래스
class_combined_4 <- ifelse(quality_numeric == 4, "Class 4", "Other")
roc_curve_4 <- roc(class_combined_4, as.numeric(result3), levels = c("Class 4", "Other"))
auc_value_4 <- auc(roc_curve_4)

print(paste("AUC for Class 3 vs. Other:", auc_value_3))
print(paste("AUC for Class 4 vs. Other:", auc_value_4))

# ROC 곡선 그리기
plot(roc_curve_3, main = "ROC Curve for Class 3 vs. Other", col = "blue")
plot(roc_curve_4, add = TRUE, col = "red")
legend("bottomright", legend = c("Class 3 vs. Other", "Class 4 vs. Other"), col = c("blue", "red"), lty = 1)
