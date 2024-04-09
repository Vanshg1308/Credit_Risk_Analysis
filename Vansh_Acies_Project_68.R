# Libraries
library(dplyr)
library(Amelia)
library(corrplot)
library(ROSE)
library(car)
library(lmtest)

# Importing the data

credit_data= readxl::read_xlsx(file.choose())
str(credit_data)

# Data Cleaning

"Missingness Graph
"
missmap(credit_data)

# Data Cleaning

"number_of_instalments"
credit_data$number_of_instalments = as.numeric(gsub("\\D", "", credit_data$number_of_instalments))

"Grade and Sub Grade"
unique(credit_data$grade)
unique(credit_data$sub_grade)

"length_of_employment"
unique(credit_data$length_of_employment)
credit_data$length_of_employment[credit_data$length_of_employment=="Missing"]=NA
credit_data$length_of_employment=c("< 1"=0.5,"1"=1,"10+"=10,"2"=2,"3"=3,"4"=4,"5"=5,"6"=6,"7"=7,"8"=8,"9"=9)[credit_data$length_of_employment]
credit_data$length_of_employment = as.numeric(credit_data$length_of_employment)
credit_data$length_of_employment[is.na(credit_data$length_of_employment)] = median(credit_data$length_of_employment,na.rm=TRUE)

"home_ownership"
unique(credit_data$home_ownership)
third_quartile_income <- quantile(credit_data$annual_income, 0.75)
credit_data$home_ownership[credit_data$home_ownership=="NONE"] = ifelse(credit_data$annual_income > third_quartile_income, 
                                                                        'OWN', 'RENT')
credit_data$home_ownership[credit_data$home_ownership=="OTHER"]= ifelse(credit_data$annual_income > third_quartile_income, 
                                                                        'OWN', 'RENT')

"total_current_bal"
credit_data$total_current_bal[is.na(credit_data$total_current_bal)]= median(credit_data$total_current_bal, na.rm = TRUE)

"Removing Redundant Variable Columns"

credit_data =credit_data[,-c(2,4,20)]
missmap(credit_data)

"Coercing Catergorical Variables as Factors"

categorical_values=c(1,6,7,9,11)
credit_data[, categorical_values] = lapply(credit_data[, categorical_values], as.factor)
str(credit_data)
 -------------------------------------------------------------------------------------------------------------------

"Exploratory Data Analysis:
"
"Boxplot for all numeric variables"

boxplot(credit_data)
--------------------------------------------------------------------------------------------------------------
"Plotting correlation Chart for Predictor Variable"

correlation=  cor(credit_data[, unlist(lapply(credit_data, is.numeric))])
corrplot(correlation)
----------------------------------------------------------------------------------------------------------------
"Plotting Graphs for Numeric Variables"

##par(mfrow = c(4,5))

"loan_amount"
hist(credit_data$loan_amount, probability = TRUE, 
     col = "lightgreen", main = "loan_amount", xlab = "Values")
lines(density(credit_data$loan_amount), col = "red", lwd = 2)

"number_of_instalments"
hist(credit_data$number_of_instalments, probability = TRUE,
     col = "lightgreen", main = "number_of_instalments", xlab = "Values")
lines(density(credit_data$number_of_instalments), col = "red", lwd = 2)

"interest_rate"
hist(credit_data$interest_rate, probability = TRUE, 
     col = "lightgreen", main = "interest_rate", xlab = "Values")
lines(density(credit_data$interest_rate), col = "red", lwd = 2)

"installment_amount "
hist(credit_data$installment_amount , probability = TRUE, 
     col = "lightgreen", main = "installment_amount ", xlab = "Values")
lines(density(credit_data$installment_amount ), col = "red", lwd = 2)

"annual_income"
hist(credit_data$annual_income , probability = TRUE, 
     col = "lightgreen", main = "annual_income", xlab = "Values")
lines(density(credit_data$annual_income), col = "red", lwd = 2)

" total_current_bal "
hist(credit_data$total_current_bal , probability = TRUE, 
     col = "lightgreen", main = " total_current_bal ", xlab = "Values")
lines(density(credit_data$ total_current_bal ), col = "red", lwd = 2)

"total_payment_recvd"
hist(credit_data$total_payment_recvd , probability = TRUE, 
     col = "lightgreen", main = "total_payment_recvd", xlab = "Values")
lines(density(credit_data$total_payment_recvd ), col = "red", lwd = 2)

"last_payment_recvd"
hist(credit_data$last_payment_recvd , probability = TRUE, 
     col = "lightgreen", main = "last_payment_recvd", xlab = "Values")
lines(density(credit_data$last_payment_recvd ), col = "red", lwd = 2)

***************************************************************************
  
"Log-Transformation of Numeric Variables"

credit_data$annual_income = log(credit_data$annual_income)
credit_data$total_current_bal = log(credit_data$total_current_bal)
credit_data$length_of_employment = log(credit_data$length_of_employment)
credit_data$loan_amount = log(credit_data$loan_amount)

************************************************************************
  
"annual_income"
hist(credit_data$annual_income, probability = TRUE, 
     col = "lightgreen", main = "annual_income", xlab = "Values")
lines(density(credit_data$annual_income), col = "red", lwd = 2)

"total_current_bal"
hist(credit_data$total_current_bal, probability = TRUE, 
     col = "lightgreen", main = "total_current_bal", xlab = "Values")
lines(density(credit_data$total_current_bal), col = "red", lwd = 2)

"length_of_employment"
hist(credit_data$length_of_employment, probability = TRUE, 
     col = "lightgreen", main = "length_of_employment", xlab = "Values")
lines(density(credit_data$length_of_employment), col = "red", lwd = 2)

"loan_amount "
hist(credit_data$loan_amount , probability = TRUE, 
     col = "lightgreen", main = "loan_amount ", xlab = "Values")
lines(density(credit_data$loan_amount ), col = "red", lwd = 2)

-----------------------------------------------------------------------------------------------------------------------------------
"Boxplot for all numeric variables after log-transformation"

boxplot(credit_data)

***************************************************************
"Plotting Histogram for Categorical Variables"

"home_ownership"
barplot(table(credit_data$loan_status, credit_data$home_ownership),
        col = c("lightgreen", "red"),
        main = "Loan Status & Home Ownership",
        xlab = "Home Ownership",
        ylab = "Loan Status",
        legend.text = levels(credit_data$loan_status),
        args.legend = list(title = "Loan Status", cex = 0.8),
        beside = TRUE  
)

"purpose"
barplot(table(credit_data$loan_status, credit_data$purpose),
        col = c("lightgreen", "red"),
        main = "Loan Status & Purpose",
        xlab = "loan_status",
        ylab = "purpose",
        legend.text = levels(credit_data$loan_status),
        args.legend = list(title = "Loan Status", cex = 0.8),
        beside = TRUE 
)

"grade"
barplot(table(credit_data$loan_status, credit_data$grade),
        col = c("lightgreen", "red"),
        main = "Loan Status & grade",
        xlab = "loan_status",
        ylab = "grade",
        legend.text = levels(credit_data$loan_status),
        args.legend = list(title = "Loan Status", cex = 0.8),
        beside = TRUE 
)

"sub_grade"
barplot(table(credit_data$loan_status, credit_data$sub_grade),
        col = c("lightgreen", "red"),
        main = "Loan Status & sub_grade",
        xlab = "loan_status",
        ylab = "sub_grade",
        legend.text = levels(credit_data$loan_status),
        args.legend = list(title = "Loan Status", cex = 0.8),
        beside = TRUE  
)
------------------------------------------------------------------------------------------------------------------

"Building the model"

model0 = glm(credit_data$loan_status ~ . , family = binomial(link="logit"), data = credit_data)
summary(model0)
model0$fitted.values

credit_data=credit_data[,-c(2,7,8,9,11,13,14,18)]

model1 = glm(credit_data$loan_status ~ . , family = binomial(link="logit"), data = credit_data)
summary(model1)

vif_values <- vif(model1)
print(vif_values)

----------------------------------------------------------------------------------------------------------------------------------------
"Splitting data into Train And Test Sets"

set.seed(99)

index=sample(1:nrow(credit_data),
             size=0.3*nrow(credit_data))

credit_test = credit_data[index,]
credit_train = credit_data[-index,]

dim(credit_test)
dim(credit_train)

credit_over = ovun.sample(loan_status ~ .,data = credit_train,p = 0.6, method = "over")$data

----------------------------------------------------------------------------------------------------------------------

"Building Model based on Train Set"

model_train <- glm(loan_status ~ . , family = binomial(link = "logit"), data = na.omit(credit_train))
summary(model_train)
D
"Building Model based on Over-Sampled Data"

model_over = glm(loan_status ~ . - installment_amount, family = binomial(link="logit"), data = credit_over)
summary(model_over)
vif(model_over)

"Building a Model without annual income & grade"

model2 = glm(loan_status ~ . - grade - annual_income - installment_amount  , family = binomial(link="logit"), data = credit_over)
summary(model2)
vif(model2)

-------------------------------------------------------------------------------------------------------------------------------------------------------------
  
"Selecting the best model using STEPAIC"

model.low= glm(credit_over$loan_status ~ 1,family=binomial(link="logit"),
               data = credit_over)

model.upp = glm(credit_over$loan_status ~ .,family=binomial(link="logit"),
                data = credit_over)

stepAIC(model.low,direction = "both",scope = list(lower = model.low,upper = model.upp))

model_final = glm(credit_over$loan_status ~ last_payment_recvd + os_principal + 
                      installment_amount + total_payment_recvd + number_of_instalments + 
                      interest_rate + grade + inquiries_last_6mths + annual_income,family=binomial(link="logit"),
                  data = credit_over)
summary(model_final)

------------------------------------------------------------------------------------------------------------------------------------------

"Fitting the Model"

"Predicted or Fitted Values"

"out-sample"
prediction_1 = predict(model_over, type='response', newdata=credit_test)
head(prediction_1)
summary(prediction_1)

"in-sample"
prediction_2 = predict(model_train, type='response', newdata=credit_test)
head(prediction_2)
summary(prediction_2)

"final"
prediction_3 = predict(model_final, type='response', newdata=credit_test)
head(prediction_3)
summary(prediction_3)

----------------------------------------------------------------------------------------------------------------------
  
"Scatterplot for Fitted Values"

"out-sample"
plot(prediction_1)

"in-sample"
plot(prediction_2)

"final"
plot(prediction_3)

--------------------------------------------------------------------------------------------------------------------------------------
  
"Binary Outcome after setting Threshold"

"out-sample"
pred_val_label1 <- as.factor(ifelse(prediction_1 > 0.5, "1", "0"))
head(pred_val_label1)
summary(pred_val_label1)


"in-sample"
pred_val_label2 <- as.factor(ifelse(prediction_2 > 0.5, "1", "0"))
head(pred_val_label2)
summary(pred_val_label2)

"final"
pred_val_label3 <- as.factor(ifelse(prediction_3 > 0.5, "1", "0"))
head(pred_val_label3)
summary(pred_val_label3)

----------------------------------------------------------------------------------------------------------------------------------

"Model Performance"

"out-sample"
pred1 = prediction(prediction_1,credit_test$loan_status)

"in-sample"
pred2 = prediction(prediction_2,credit_test$loan_status)

"final"
pred3 = prediction(prediction_3,credit_test$loan_status)


"Performance Object"

"out-sample"
roc.perf1=performance(pred1,measure = "tpr",x.measure = "fpr")

"in-sample"
roc.perf2=performance(pred2,measure = "tpr",x.measure = "fpr")

"final"
roc.perf3=performance(pred3,measure = "tpr",x.measure = "fpr")


"Plot ROC(Reciever Operating Characteristics) Curve"

"out-sample"
plot(roc.perf1)
abline(a=0, b=1)

"in-sample"
plot(roc.perf2)
abline(a=0, b=1)

"final"
plot(roc.perf3)
abline(a=0, b=1)

-----------------------------------------------------------------------------------------------------------------

"Get Area under the curve"

"out-sample"
auc.perf1 = performance(pred1, measure = "auc")
auc.perf1@y.values

"in-sample"
auc.perf2 = performance(pred2, measure = "auc")
auc.perf2@y.values


"final"
auc.perf3 = performance(pred3, measure = "auc")
auc.perf3@y.values

-------------------------------------------------------------------------------------------------------------------
"Accuracy"

"out-sample"
acc.perf1 = performance(pred1, measure="acc")
plot(acc.perf1)

"in-sample"
acc.perf2 = performance(pred2, measure="acc")
plot(acc.perf2)

"final"
acc.perf3 = performance(pred3, measure="acc")
plot(acc.perf3)

------------------------------------------------------------------------------------------------------------------------------------
"Building the Confusion Matrix of Oversampled Set on Test Set"

conf_matrix1=confusionMatrix(credit_test$loan_status,pred_val_label1)
conf_matrix1

# Extract metrics from the confusion matrix1
accuracy1 <- conf_matrix1$overall["Accuracy"]
sensitivity1 <- conf_matrix1$byClass["Sensitivity"]
specificity1 <- conf_matrix1$byClass["Specificity"]
precision1 <- conf_matrix1$byClass["Pos Pred Value"]
false_positive_rate1 <- 1 - specificity1
false_negative_rate1 <- 1 - sensitivity1
f_score1 <- conf_matrix1$byClass["F1"]

# Print the metrics
print(paste("Accuracy1:", accuracy1))
print(paste("Sensitivity1:", sensitivity1))
print(paste("Specificity1:", specificity1))
print(paste("Precision1:", precision1))
print(paste("False Positive Rate1:", false_positive_rate1))
print(paste("False Negative Rate1:", false_negative_rate1))
print(paste("F-Score1:", f_score1))

----------------------------------------------------------------------------------------------------------------
"Building Confusion Matrix of Train Set on Test Set"

conf_matrix2=confusionMatrix(credit_test$loan_status,pred_val_label2)
conf_matrix2

# Extract metrics from the confusion matrix2
accuracy2 <- conf_matrix2$overall["Accuracy"]
sensitivity2 <- conf_matrix2$byClass["Sensitivity"]
specificity2 <- conf_matrix2$byClass["Specificity"]
precision2 <- conf_matrix2$byClass["Pos Pred Value"]
false_positive_rate2 <- 1 - specificity2
false_negative_rate2 <- 1 - sensitivity2
f_score2 <- conf_matrix2$byClass["F1"]

# Print the metrics
print(paste("Accuracy2:", accuracy2))
print(paste("Sensitivity2:", sensitivity2))
print(paste("Specificity2:", specificity2))
print(paste("Precision2:", precision2))
print(paste("False Positive Rate2:", false_positive_rate2))
print(paste("False Negative Rate2:", false_negative_rate2))
print(paste("F-Score2:", f_score2))

----------------------------------------------------------------------------------------------------------------------------
  
"Building the Confusion Matrix of Final Model from StepAIC on Test Set"

conf_matrix3=confusionMatrix(credit_test$loan_status,pred_val_label3)
conf_matrix3

# Extract metrics from the confusion matrix3
accuracy3 <- conf_matrix3$overall["Accuracy"]
sensitivity3 <- conf_matrix3$byClass["Sensitivity"]
specificity3 <- conf_matrix3$byClass["Specificity"]
precision3 <- conf_matrix3$byClass["Pos Pred Value"]
false_positive_rate3 <- 1 - specificity3
false_negative_rate3 <- 1 - sensitivity3
f_score3 <- conf_matrix3$byClass["F1"]

# Print the metrics
print(paste("Accuracy3:", accuracy3))
print(paste("Sensitivity3:", sensitivity3))
print(paste("Specificity3:", specificity3))
print(paste("Precision3:", precision3))
print(paste("False Positive Rate3:", false_positive_rate3))
print(paste("False Negative Rate3:", false_negative_rate3))
print(paste("F-Score3:", f_score3))

-----------------------------------------------------------------------------------------------------------------------------------------
"Choosing Optimal Threshold for the Model"

roc_curve1 <- roc(credit_test$loan_status, prediction_1)
optimal_cutoff1 <- coords(roc_curve1, "best")$threshold

roc_curve2 <- roc(credit_test$loan_status, prediction_2)
optimal_cutoff2 <- coords(roc_curve2, "best")$threshold

roc_curve3 <- roc(credit_test$loan_status, prediction_3)
optimal_cutoff3 <- coords(roc_curve3, "best")$threshold

-------------------------------------------------------------------------------------------------------------------
  
"Assumption Testing"

"Checking Autocorrelation"

residuals_upp <- residuals(model.upp)
residuals_low <- residuals(model.low)

dw_test_upp <- dwtest(residuals_upp ~ lag(residuals_upp), alternative = "two.sided")
dw_test_low <- dwtest(residuals_low ~ lag(residuals_low), alternative = "two.sided")
dw_test_upp
dw_test_low
******************************************************************

"Checking Multicollinearity"

vif_values_upp <- vif(model.upp)
print(vif_values_upp)
****************************************************************
"Checking  Homocedascity"

bp_test_up=bptest(model.upp)
bp_test_up
-----------------------------------------------------------------------------------------------------------------------------