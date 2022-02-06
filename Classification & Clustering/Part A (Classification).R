library(caTools)
library(readr)
library(dplyr)
library(plyr)
library(readr)
library(ggplot2)
library(naniar)
library(caret)
library(rpart)
library(rpart.plot)
library(quantmod)
library(TTR)
library(xgboost)
library(CatEncoders)
library(neuralnet)
library(corrplot)
library(ROCR)
df <- read_csv("Churn Dataset.csv")
View(df)

# Check the format of the data
unique(df$MultipleLines)
unique(df$OnlineSecurity)
unique(df$OnlineBackup)
unique(df$DeviceProtection)
unique(df$StreamingMovies)
unique(df$StreamingTV)
unique(df$TechSupport)
#Transform the data
df$MultipleLines <- revalue(df$MultipleLines, replace= c("No phone service" = "No"))
df$OnlineSecurity <- revalue(df$OnlineSecurity, replace= c("No internet service" = "No"))
df$OnlineBackup <- revalue(df$OnlineBackup, replace= c("No internet service" = "No"))
df$DeviceProtection<- revalue(df$DeviceProtection, replace= c("No internet service" = "No"))
df$StreamingMovies <- revalue(df$StreamingMovies, replace= c("No internet service" = "No"))
df$StreamingTV <- revalue(df$StreamingTV, replace= c("No internet service" = "No"))
df$TechSupport <- revalue(df$TechSupport, replace= c("No internet service" = "No"))
df$SeniorCitizen <- as.factor(df$SeniorCitizen)
# drop customerID column
df<- select(df, -customerID)

# Check missing values
sum(is.na(df))
gg_miss_var(df) + labs(y = "Look at all the missing ones")
# handle missing values by replacing NA with mean value of TotalCharges
df$TotalCharges <- replace(df$TotalCharges,is.na(df$TotalCharges),mean(df$TotalCharges,na.rm=TRUE))
sum(is.na(df))

# Generate a scatterplot matrix
pairs(select_if(df, is.numeric), pch = 19)
# correlation plot
corrplot(cor(select_if(df, is.numeric)),        # Correlation matrix
         method = "circle",                # Correlation plot method (method = number, circle, pie, or color)
         type = "full",                   # Correlation plot style (also "upper" and "lower")
         diag = TRUE,                     # If TRUE (default), adds the diagonal
         tl.col = "black",                # Labels color
         bg = "white",                    # Background color
         title = "",                      # Main title
         col = NULL,                      # Color palette
         tl.cex =0.7,
         cl.ratio =0.2)  


# split data into 80% training and 20% testing
set.seed(42)
sample_split <- sample.split(Y = df$Churn, SplitRatio = 0.80)
train_set <- subset(x = df, sample_split == TRUE)
test_set <- subset(x = df, sample_split == FALSE)

# fit training data on the decision tree (Gini)
dt_model <- rpart(Churn ~ ., data = train_set, method = "class")
# Plot the decision tree
rpart.plot(dt_model)
rpart.rules(dt_model, roundint=FALSE, clip.facs=TRUE)
# make predictions
preds <- predict(dt_model, newdata = test_set, type = "class")
#Print the confusion Matrix
confusionMatrix(as.factor(test_set$Churn),preds)

#cost-complexity pruning
printcp(dt_model)
# get index of CP with lowest xerror
opt <- which.min(dt_model$cptable[,"xerror"])
#get its value
cp <- dt_model$cptable[opt, "CP"]
#prune tree
pruned_model1 <- prune(dt_model,cp)
# make predictions
preds1 <- predict(pruned_model1, newdata = test_set, type = "class")
#Print the confusion Matrix
confusionMatrix(as.factor(test_set$Churn),preds1)

# using information gain spiting strategy instead of Gini 
dt_model2 <- rpart(Churn ~ ., data = train_set, parms=list(split= "information"))
#cost-complexity pruning
printcp(dt_model2)
# get index of CP with lowest xerror
opt2 <- which.min(dt_model2$cptable[,"xerror"])
#get its value
cp2 <- dt_model2$cptable[opt2, "CP"]
#prune tree
pruned_model2 <- prune(dt_model2,cp2)
# make predictions
preds2 <- predict(pruned_model2, newdata = test_set, type = "class")
#Print the confusion Matrix
confusionMatrix(as.factor(test_set$Churn),preds2)


# XGboost model using 10-fold cross-validation
X_data = select(train_set, -Churn)
y_label = select(train_set, Churn)

# label encoding for the trsin data data
X_data$gender = as.integer(factor(X_data$gender))
X_data$Partner = as.integer(factor(X_data$Partner))
X_data$SeniorCitizen = as.integer(factor(X_data$SeniorCitizen))
X_data$Dependents = as.integer(factor(X_data$Dependents))
X_data$PhoneService = as.integer(factor(X_data$PhoneService))
X_data$MultipleLines = as.integer(factor(X_data$MultipleLines))
X_data$InternetService = as.integer(factor(X_data$InternetService))
X_data$OnlineSecurity = as.integer(factor(X_data$OnlineSecurity))
X_data$OnlineBackup = as.integer(factor(X_data$OnlineBackup))
X_data$DeviceProtection = as.integer(factor(X_data$DeviceProtection))
X_data$TechSupport = as.integer(factor(X_data$TechSupport))
X_data$StreamingTV = as.integer(factor(X_data$StreamingTV))
X_data$StreamingMovies = as.integer(factor(X_data$StreamingMovies))
X_data$Contract = as.integer(factor(X_data$Contract))
X_data$PaperlessBilling = as.integer(factor(X_data$PaperlessBilling))
X_data$PaymentMethod = as.integer(factor(X_data$PaymentMethod))
y_label$Churn = ifelse(y_label$Churn == "Yes",1,0)


X_data_test = select(test_set, -Churn)
y_label_test = select(test_set, Churn)

# label encoding for test data
X_data_test$gender = as.integer(factor(X_data_test$gender))
X_data_test$Partner = as.integer(factor(X_data_test$Partner))
X_data_test$SeniorCitizen = as.integer(factor(X_data_test$SeniorCitizen))
X_data_test$Dependents = as.integer(factor(X_data_test$Dependents))
X_data_test$PhoneService = as.integer(factor(X_data_test$PhoneService))
X_data_test$MultipleLines = as.integer(factor(X_data_test$MultipleLines))
X_data_test$InternetService = as.integer(factor(X_data_test$InternetService))
X_data_test$OnlineSecurity = as.integer(factor(X_data_test$OnlineSecurity))

X_data_test$OnlineBackup = as.integer(factor(X_data_test$OnlineBackup))
X_data_test$DeviceProtection = as.integer(factor(X_data_test$DeviceProtection))
X_data_test$TechSupport = as.integer(factor(X_data_test$TechSupport))
X_data_test$StreamingTV = as.integer(factor(X_data_test$StreamingTV))
X_data_test$StreamingMovies = as.integer(factor(X_data_test$StreamingMovies))
X_data_test$Contract = as.integer(factor(X_data_test$Contract))
X_data_test$PaperlessBilling = as.integer(factor(X_data_test$PaperlessBilling))
X_data_test$PaymentMethod = as.integer(factor(X_data_test$PaymentMethod))
y_label_test$Churn = ifelse(y_label_test$Churn == "Yes",1,0)


# train XGBoost with cross validation and grid search
xgb_params <- expand.grid(
  eta = c(0.01, 0.1, 1),
  lambda = c(0.1, 0.5, 1),
  alpha =  c(0.1, 0.5, 1))

xgb_model_cv = xgb.cv(params = as.list(xgb_params),
                      data = as.matrix(X_data),
                      label = y_label$Churn,
                      showsd = T,
                      stratified = T,
                      print_every_n = 1,
                      maximize = F,
                      prediction = TRUE,
                      nround = 3,
                      nfold = 10,
                      objective = "binary:logistic",
                      eval_metric='logloss')

print(xgb_model_cv)
xgb_model = xgboost(data = as.matrix(X_data),
                    label = y_label$Churn,
                    nround = 3,
                    objective = "binary:logistic",
                    eval_metric = 'logloss')

# make predictions using XGBoost
xgb_preds_prop <- predict(xgb_model, as.matrix(X_data_test))
xgb_preds = as.numeric(xgb_preds_prop > 0.5)
#Print the confusion Matrix
confusionMatrix(as.factor(y_label_test$Churn), as.factor(xgb_preds))

# Build a multilayer perceptron

# standardize the numeric column in the dataset
train_scaled_df = X_data
train_scaled_df$Churn = y_label$Churn
train_scaled_df$tenure = (train_scaled_df$tenure - mean(train_scaled_df$tenure)) / sd(train_scaled_df$tenure)
train_scaled_df$MonthlyCharges = (train_scaled_df$MonthlyCharges - mean(train_scaled_df$MonthlyCharges)) / sd(train_scaled_df$MonthlyCharges)
train_scaled_df$TotalCharges = (train_scaled_df$TotalCharges - mean(train_scaled_df$TotalCharges)) / sd(train_scaled_df$TotalCharges)


NN_model = neuralnet(Churn ~ gender + SeniorCitizen + Partner + Dependents + tenure +
                       PhoneService + MultipleLines + InternetService + OnlineSecurity +
                       OnlineBackup + DeviceProtection + TechSupport + StreamingTV + Contract +
                       PaperlessBilling + PaymentMethod + MonthlyCharges + TotalCharges,
                     data= train_scaled_df, hidden = 5 , act.fct = "logistic", linear.output = F )

plot(NN_model)

# Make prediciton on NN model
test_scaled_df = X_data_test
test_scaled_df$Churn = y_label_test$Churn
test_scaled_df$tenure = (test_scaled_df$tenure - mean(test_scaled_df$tenure)) / sd(test_scaled_df$tenure)
test_scaled_df$MonthlyCharges = (test_scaled_df$MonthlyCharges - mean(test_scaled_df$MonthlyCharges)) / sd(test_scaled_df$MonthlyCharges)
test_scaled_df$TotalCharges = (test_scaled_df$TotalCharges - mean(test_scaled_df$TotalCharges)) / sd(test_scaled_df$TotalCharges)

nn_preds_prop = neuralnet::compute(NN_model,test_scaled_df)
nn_preds = ifelse(nn_preds_prop$net.result>0.5, 1, 0)
confusionMatrix(as.factor(test_scaled_df$Churn), as.factor(nn_preds))

# change hyperparamters of the model
NN_model_tanh = neuralnet(Churn ~ gender + SeniorCitizen + Partner + Dependents + tenure +
                            PhoneService + MultipleLines + InternetService + OnlineSecurity +
                            OnlineBackup + DeviceProtection + TechSupport + StreamingTV + Contract +
                            PaperlessBilling + PaymentMethod + MonthlyCharges + TotalCharges,
                          data= train_scaled_df, hidden = c(2, 2), act.fct = 'tanh', linear.output = F,
                          learningrate = 0.2, threshold = 0.1, stepmax=1e7)

plot(NN_model_tanh)

# make predictions on NN model with tanh activation function
nn_preds_prop2 = neuralnet::compute(NN_model_tanh,test_scaled_df)
nn_preds2 = ifelse(nn_preds_prop2$net.result>0.5, 1, 0)
confusionMatrix(as.factor(test_scaled_df$Churn), as.factor(nn_preds2))

# plot ROC curve for the DT model
Predictions_dt <- prediction(as.numeric(preds), as.numeric(as.factor(test_set$Churn)))
pref_dt <- performance(Predictions_dt, "tpr","fpr")
plot(pref_dt,main = "ROC Curve",col = 2,lwd = 2)
abline(a = 0,b = 1,lwd = 2,lty = 3,col = "black")
auc_dt <- performance(Predictions_dt, measure = "auc")
auc_dt <- auc_dt@y.values[[1]]
print(auc_dt)

# plot ROC curve for the XGBoost model
Predictions_xgb <- prediction(xgb_preds, y_label_test$Churn)
pref_xgb<- performance(Predictions_xgb, "tpr","fpr")
plot(pref_xgb,main = "ROC Curve",col = 2,lwd = 2)
abline(a = 0,b = 1,lwd = 2,lty = 3,col = "black")
auc_xgb <- performance(Predictions_xgb, measure = "auc")
auc_xgb <- auc_xgb@y.values[[1]]
print(auc_xgb)

# plot ROC curve for the XGBoost model
Predictions_nn <- prediction(as.numeric(nn_preds2), as.numeric(test_scaled_df$Churn))
pref_nn<- performance(Predictions_nn, "tpr","fpr")
plot(pref_nn,main = "ROC Curve",col = 2,lwd = 2)
abline(a = 0,b = 1,lwd = 2,lty = 3,col = "black")
auc_nn <- performance(Predictions_nn, measure = "auc")
auc_nn <- auc_nn@y.values[[1]]
print(auc_nn)
