# install packages required
# Load libraries
library(readxl) # For reading Excel files
library(psych)  # for pairs.panels
library(randomForest) # For random forest
library(e1071) # For SVM
library(caret) # For logistic regression and data partitioning
library(dplyr) # for data manipulation
library(tidyr) # data tidying
library(pROC) # ROC curve analysis

# 1. Introduction:
# This project aims to improve the accuracy of appendicitis diagnosis through 
# the development of an ensemble predictive model that utilizes clinical features. 
# By combining Logistic Regression, Random Forest and SVM machine learning methods, 
# we seek to create a robust tool that enhances diagnostic precision.

# 2. Data Loading
# Download the file from the URL into a temporary file
url <- "https://zenodo.org/records/7669442/files/app_data.xlsx?download=1"
temp <- tempfile(fileext = ".xlsx")
download.file(url, temp, mode = "wb")

# Now read from the downloaded file
data <- read_excel(temp)

# Clean up the temporary file

# 3. Exploratory Data Analysis (EDA)
head(data)  # view structure of the data set
summary(data)  # summary of each column

# Calculate the number of missing values in each column
missing_values <- colSums(is.na(data))
missing_values

# Select numeric columns for correlation
numeric_data <- data[, sapply(data, is.numeric)]

# Calculate the correlation matrix, handling missing values by considering only 
# pairwise complete observations
cor_matrix <- cor(numeric_data, use = "pairwise.complete.obs")
print(cor_matrix)

# convert BMI column to numeric column
data$BMI <- as.numeric(data$BMI) 


# 4. Data Pre-processing
# Calculate the percentage of NA values in each column
na_percentage <- colMeans(is.na(data)) * 100

# Get the names of columns with more than 50% missing values
columns_to_drop <- names(na_percentage[na_percentage > 50])

# Drop columns with more than 50% missing values
data_clean <- data[, !names(data) %in% columns_to_drop]

# Dropping other class variables 'Management' and 'Severity' columns 
# from the data frame
data_clean <- data_clean %>% 
  dplyr::select(-Management, -Severity)

# Convert Diagnosis column to binary format
data_clean$Diagnosis <- ifelse(data_clean$Diagnosis == "appendicitis", 1, 0)

# Identify categorical variables
categorical_columns <- c(
  "Sex", "Appendix_on_US", "Migratory_Pain", "Lower_Right_Abd_Pain", 
  "Contralateral_Rebound_Tenderness", "Coughing_Pain", "Nausea", 
  "Loss_of_Appetite", "Neutrophilia", "Ketones_in_Urine", "RBC_in_Urine", 
  "WBC_in_Urine", "Dysuria", "Stool", "Peritonitis", "Psoas_Sign",
  "Ipsilateral_Rebound_Tenderness", "US_Performed", "Free_Fluids",
  "Diagnosis_Presumptive"
)

# Function to calculate the mode
get_mode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}

# Impute numerical columns with median values
numerical_columns <- setdiff(names(data_clean), categorical_columns)
data_clean[numerical_columns] <- lapply(data_clean[numerical_columns], function(x) ifelse(is.na(x), median(x, na.rm = TRUE), x))

# Impute categorical columns with mode values
data_clean[categorical_columns] <- lapply(data_clean[categorical_columns], function(x) ifelse(is.na(x), get_mode(x), x))

# Check if there are missing values in the 'Diagnosis' column and impute if necessary
if (any(is.na(data_clean$Diagnosis))) {
  data_clean$Diagnosis <- ifelse(is.na(data_clean$Diagnosis), get_mode(data_clean$Diagnosis), data_clean$Diagnosis)
}

# Calculate the number of missing values in each column
missing_values <- colSums(is.na(data_clean))
missing_values

# Select numeric columns for outlier detection
numeric_data_clean <- data_clean[, sapply(data_clean, is.numeric)]

# Adjusting the plot margins
par(mar = c(5, 4, 4, 2) + 0.1)

# Use pairs.panels on the numeric columns
pairs.panels(numeric_data_clean, 
             method = "pearson",  # for correlation coefficients
             hist.col = "#00AFBB",  # color for histograms
             density = FALSE,       # to show density plots along with histograms
             ellipses = FALSE)      # to show correlation confidence ellipses

# Calculate skewness for each numeric variable
skewed_vars <- sapply(numeric_data_clean, skewness)

# Print the skewness values to check each variable
print(skewed_vars)
# Function to detect outliers based on the IQR method
detect_outliers <- function(data) {
  for (col in names(data)) {
    if (is.numeric(data[[col]])) {
      Q1 <- quantile(data[[col]], 0.25, na.rm = TRUE)
      Q3 <- quantile(data[[col]], 0.75, na.rm = TRUE)
      IQR <- Q3 - Q1
      lower_bound <- Q1 - 1.5 * IQR
      upper_bound <- Q3 + 1.5 * IQR
      outliers <- data[[col]][data[[col]] < lower_bound | data[[col]] > upper_bound]
      if (length(outliers) > 0) {
        cat("\nOutliers in", col, ":\n", outliers)
      }
    }
  }
}

# Apply the function to the data
detect_outliers(numeric_data_clean)

# Normality test for numeric columns
# Apply Shapiro-Wilk test to each numeric variables and exclude columns where all values are identical
valid_numeric_columns <- sapply(data_clean[, numerical_columns], function(x) length(unique(x)) > 1)

# Apply Shapiro-Wilk test to each numeric variable that has more than one unique value
results <- lapply(data_clean[, numerical_columns][, valid_numeric_columns], shapiro.test)

# Performing one-hot encoding on Categorical variables
# Convert categorical variables to factors
for (col in categorical_columns) {
  data_clean[[col]] <- factor(data_clean[[col]])
}

# Create dummy variables 
dummy_variables <- model.matrix(~., data = data_clean[, categorical_columns])

# Convert the resulting matrix back to a data frame
dummy_df <- as.data.frame(dummy_variables)

# Add column names to the dummy data frame
colnames(dummy_df) <- gsub("[-.]", "_", colnames(dummy_df)) # remove any special characters in column names

# Combine the dummy data frame with the original data frame
data_clean <- cbind(data_clean, dummy_df)

# Remove the original categorical columns from the data frame
data_clean <- data_clean[, !(names(data_clean) %in% categorical_columns)]

# Train the logistic regression model
# This process enhances the model's efficiency by focusing on significant 
# predictors and ensures the target variable is suitably prepared for binary classification
# Train the logistic regression model
logistic_model <- glm(Diagnosis ~ ., data = data_clean, family = "binomial")

# Get the p-values for each feature
p_values <- summary(logistic_model)$coefficients[, "Pr(>|z|)"]

# Find features with p-values greater than 0.05
features_to_drop <- names(p_values[p_values > 0.05])

# Remove features with high p-values
refined_data <- data_clean[, !(names(data_clean) %in% features_to_drop)]

# Ensure the Diagnosis column is a factor
refined_data$Diagnosis <- factor(refined_data$Diagnosis)

# Check the levels of the 'Diagnosis' factor
levels(refined_data$Diagnosis)

# Convert numbers to factor 
refined_data$Diagnosis <- factor(refined_data$Diagnosis, levels = c(0, 1), labels = c("Non_appendicitis", "Appendicitis"))

# Verify the new levels
levels(refined_data$Diagnosis)

# Ensure only columns present in refined_data are included
numerical_columns <- numerical_columns[numerical_columns %in% colnames(refined_data)]
numerical_columns <- numerical_columns[sapply(refined_data[numerical_columns], is.numeric)]

# view numerical_columns
numerical_columns

# Normalize only numeric columns using box-cox 
preProcValues_numeric <- preProcess(refined_data[, numerical_columns], method = c("BoxCox"))

# Transform the numeric columns using the defined pre-processing object
data_scaled_numeric <- predict(preProcValues_numeric, refined_data[, numerical_columns])

# Create a logical vector where TRUE means the column is NOT in numerical_columns
columns_to_exclude <- !colnames(refined_data) %in% numerical_columns

# Subset refined_data to get only non-numeric columns based on the logical vector
data_non_numeric <- refined_data[, columns_to_exclude]

# Combine the non-numeric data with the scaled numeric data
data_scaled <- cbind(data_non_numeric, data_scaled_numeric)

# standardized values of data
print(head(data_scaled))

# 5. Model Building 
# Split the data into training and testing sets using the scaled data
set.seed(123)  # for reproducibility
trainIndex <- createDataPartition(data_scaled$Diagnosis, p = 0.8, list = FALSE)
trainData <- data_scaled[trainIndex, ]
testData <- data_scaled[-trainIndex, ]

# Implement k-fold cross-validation with grid search to fine-tune hyperparameters 
# for the models, to enhance their predictive accuracy and generalization capability for appendicitis diagnosis

# Grid of hyperparameters for glmnet (Logistic Regression)
gridLogistic <- expand.grid(
  .alpha = seq(0, 1, by = 0.25),  # Mix ratio of L1 (Lasso) and L2 (Ridge)
  .lambda = 10^seq(-3, 1, by = 0.5)  # Regularization strength
)

# TrainControl setup for CV
controlLogistic <- trainControl(
  method = "cv",
  number = 10,
  savePredictions = "final",
  classProbs = TRUE
)

# Train the model
logistic_model_cv <- train(
  Diagnosis ~ .,
  data = trainData,
  method = "glmnet",
  tuneGrid = gridLogistic,
  trControl = controlLogistic
)

# Grid of hyperparameters for Random Forest
# 'mtry' is the number of variables randomly sampled as candidates at each split
gridRF <- expand.grid(
  mtry = seq(2, sqrt(ncol(trainData)), by = 2)  
)

# TrainControl setup for CV
controlRF <- trainControl(
  method = "cv",
  number = 10,
  savePredictions = "final",
  classProbs = TRUE
)

# Train the model using 'rf' method
rf_model_cv <- train(
  Diagnosis ~ .,
  data = trainData,
  method = "rf",
  tuneGrid = gridRF,
  trControl = controlRF,
  ntree = 500  # Number of trees to grow in the forest 
)

# Grid of hyperparameters for SVM
gridSVM <- expand.grid(
  .sigma = seq(0.005, 0.1, length = 10),
  .C = 10^seq(-1, 2, length = 10)
)

# TrainControl setup for CV
controlSVM <- trainControl(
  method = "cv",
  number = 10,
  savePredictions = "final",
  classProbs = TRUE
)

# Train the model
svm_model_cv <- train(
  Diagnosis ~ .,
  data = trainData,
  method = "svmRadial",
  tuneGrid = gridSVM,
  trControl = controlSVM
)
# Evaluate each model on the test set
# Logistic Regression
predictions_log <- predict(logistic_model_cv, newdata = testData)
conf_matrix_log <- confusionMatrix(predictions_log, testData$Diagnosis)
print("Logistic Regression Model Evaluation")
print(conf_matrix_log)

# Random Forest
predictions_rf <- predict(rf_model_cv, newdata = testData)
conf_matrix_rf <- confusionMatrix(predictions_rf, testData$Diagnosis)
print("Random Forest Model Evaluation")
print(conf_matrix_rf)

# SVM
predictions_svm <- predict(svm_model_cv, newdata = testData)
conf_matrix_svm <- confusionMatrix(predictions_svm, testData$Diagnosis)
print("SVM Model Evaluation")
print(conf_matrix_svm)

# Evaluate models using additional metrics like AUC, F1 Score etc.
# AUC for Logistic Regression
auc_log <- pROC::roc(response = testData$Diagnosis, predictor = as.numeric(predictions_log))
print(paste("AUC for Logistic Regression:", auc_log$auc))

# AUC for Random Forest
auc_rf <- pROC::roc(response = testData$Diagnosis, predictor = as.numeric(predictions_rf))
print(paste("AUC for Random Forest:", auc_rf$auc))

# AUC for SVM
auc_svm <- pROC::roc(response = testData$Diagnosis, predictor = as.numeric(predictions_svm))
print(paste("AUC for SVM:", auc_svm$auc))

# Evaluate each model on the test set
# Logistic Regression
predictions_log <- predict(logistic_model_cv, newdata = testData)
conf_matrix_log <- confusionMatrix(predictions_log, testData$Diagnosis)
print("Logistic Regression Model Evaluation")
print(conf_matrix_log)

# Random Forest
predictions_rf <- predict(rf_model_cv, newdata = testData)
conf_matrix_rf <- confusionMatrix(predictions_rf, testData$Diagnosis)
print("Random Forest Model Evaluation")
print(conf_matrix_rf)

# SVM
predictions_svm <- predict(svm_model_cv, newdata = testData)
conf_matrix_svm <- confusionMatrix(predictions_svm, testData$Diagnosis)
print("SVM Model Evaluation")
print(conf_matrix_svm)

# Evaluate models using additional metrics like AUC, F1 Score etc.
# AUC for Logistic Regression
auc_log <- pROC::roc(response = testData$Diagnosis, predictor = as.numeric(predictions_log))
print(paste("AUC for Logistic Regression:", auc_log$auc))

# AUC for Random Forest
auc_rf <- pROC::roc(response = testData$Diagnosis, predictor = as.numeric(predictions_rf))
print(paste("AUC for Random Forest:", auc_rf$auc))

# AUC for SVM
auc_svm <- pROC::roc(response = testData$Diagnosis, predictor = as.numeric(predictions_svm))
print(paste("AUC for SVM:", auc_svm$auc))

# Generate base model predictions on the training set for the meta-model
predictions_log_train <- predict(logistic_model_cv, trainData, type = "prob")[, "Appendicitis"]
predictions_rf_train <- predict(rf_model_cv, trainData, type = "prob")[, "Appendicitis"]
predictions_svm_train <- predict(svm_model_cv, trainData, type = "prob")[, "Appendicitis"]

# Create a data frame with these predictions as features
ensemble_train <- data.frame(
  Logistic = predictions_log_train,
  RandomForest = predictions_rf_train,
  SVM = predictions_svm_train,
  Diagnosis = trainData$Diagnosis
)

# Train the meta-model on these predictions
meta_model <- train(
  Diagnosis ~ .,
  data = ensemble_train,
  method = "glm",
  family = "binomial",
  trControl = trainControl(method = "cv", number = 10)
)

# Evaluate each model on the test set
# Logistic Regression
predictions_log <- predict(logistic_model_cv, newdata = testData)
conf_matrix_log <- confusionMatrix(predictions_log, testData$Diagnosis)
print("Logistic Regression Model Evaluation")
print(conf_matrix_log)

# Random Forest
predictions_rf <- predict(rf_model_cv, newdata = testData)
conf_matrix_rf <- confusionMatrix(predictions_rf, testData$Diagnosis)
print("Random Forest Model Evaluation")
print(conf_matrix_rf)

# SVM
predictions_svm <- predict(svm_model_cv, newdata = testData)
conf_matrix_svm <- confusionMatrix(predictions_svm, testData$Diagnosis)
print("SVM Model Evaluation")
print(conf_matrix_svm)

# Also evaluate models using AUC metric
# AUC for Logistic Regression
auc_log <- pROC::roc(response = testData$Diagnosis, predictor = as.numeric(predictions_log))
print(paste("AUC for Logistic Regression:", auc_log$auc))

# AUC for Random Forest
auc_rf <- pROC::roc(response = testData$Diagnosis, predictor = as.numeric(predictions_rf))
print(paste("AUC for Random Forest:", auc_rf$auc))

# AUC for SVM
auc_svm <- pROC::roc(response = testData$Diagnosis, predictor = as.numeric(predictions_svm))
print(paste("AUC for SVM:", auc_svm$auc))

# Function to predict outcome class based on logistic regression, random forest, and SVM models
predict_outcome_class <- function(logistic_model_cv, rf_model_cv, svm_model_cv, data) {
  # Predict using logistic regression model
  logistic_predictions <- predict(logistic_model_cv, newdata = data, type = "prob")[, "Appendicitis"]
  logistic_predictions <- ifelse(logistic_predictions > 0.5, "Appendicitis", "Non_appendicitis")
  
  # Predict using random forest model
  rf_predictions <- predict(rf_model_cv, newdata = data, type = "prob")[, "Appendicitis"]
  rf_predictions <- ifelse(rf_predictions > 0.5, "Appendicitis", "Non_appendicitis")
  
  # Predict using SVM model
  svm_predictions <- predict(svm_model_cv, newdata = data, type = "prob")[, "Appendicitis"]
  svm_predictions <- ifelse(svm_predictions > 0.5, "Appendicitis", "Non_appendicitis")
  
  # Combine predictions into a data frame
  predictions_df <- data.frame(Logistic = logistic_predictions, Random_Forest = rf_predictions, SVM = svm_predictions)
  
  # Use row-wise majority vote to determine the final prediction
  final_prediction <- apply(predictions_df, 1, function(x) {
    tab <- table(x)
    as.character(names(tab)[which.max(tab)])
  })
  
  return(final_prediction)
}

# Ensemble predictions on testData
ensemble_predictions <- predict_outcome_class(logistic_model_cv, rf_model_cv, svm_model_cv, testData)

# Convert the predictions to factor to align with the actual outcomes
ensemble_predictions_factor <- factor(ensemble_predictions, levels = c("Non_appendicitis", "Appendicitis"))

# Evaluate the ensemble model using the confusion matrix
conf_matrix_ensemble <- confusionMatrix(ensemble_predictions_factor, testData$Diagnosis)
print("Ensemble Model Evaluation")
print(conf_matrix_ensemble)

# function to return probabilities for AUC calculation
predict_ensemble_probabilities <- function(logistic_model_cv, rf_model_cv, svm_model_cv, data) {
  # Get probabilities from each model
  prob_logistic <- predict(logistic_model_cv, newdata = data, type = "prob")[, "Appendicitis"]
  prob_rf <- predict(rf_model_cv, newdata = data, type = "prob")[, "Appendicitis"]
  prob_svm <- predict(svm_model_cv, newdata = data, type = "prob")[, "Appendicitis"]
  
  # Calculate the average probability from all three models
  ensemble_probabilities <- (prob_logistic + prob_rf + prob_svm) / 3
  return(ensemble_probabilities)
}

# Calculate ensemble probabilities on the test data
ensemble_probs <- predict_ensemble_probabilities(logistic_model_cv, rf_model_cv, svm_model_cv, testData)

# Compute AUC for the ensemble model
roc_ensemble <- roc(testData$Diagnosis, ensemble_probs)
auc_ensemble <- auc(roc_ensemble)
print(paste("AUC for Ensemble Model:", auc_ensemble))
