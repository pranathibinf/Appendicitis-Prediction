# An Ensemble Model to Predict Appendicitis: Development and Evaluation

## Project Overview:
This project focuses on enhancing the accuracy of diagnosing appendicitis by developing an ensemble model that utilizes clinical features. The ensemble model combines Logistic Regression, Random Forest, and SVM machine learning methods to improve diagnostic precision. The data used in this study was sourced from Zenodo's Clinical Data Repository.

## Table of Contents:
Introduction
Data Loading
Exploratory Data Analysis (EDA)
Data Pre-processing
Model Building
Model Evaluation
Inference
References

## Introduction:
The project's aim is to develop a robust tool to increase the accuracy of appendicitis diagnosis by integrating various machine learning techniques that leverage clinical data effectively.

## Setup and Installation:
R Studio: For statistical computing
dplyr, tidyr, ggplot2: For data manipulation and visualization
randomForest, e1071, caret: For building machine learning models
pROC: For AUC and ROC curve analysis

## Data Loading:
Data is loaded from the UCI machine learning repository. Details on how the data is accessed and prepared for analysis are included.

## Exploratory Data Analysis (EDA):
We perform an initial analysis to understand the data structure, check for missing values, and observe the distribution of variables.

## Data Pre-processing:
This section details the steps taken to clean the data and prepare it for modeling. This includes handling missing values, encoding categorical variables, and normalizing data.

## Model Building:
Description of how the Logistic Regression, Random Forest, and SVM models are built, including parameter tuning and validation approaches.

## Model Evaluation:
We evaluate each model using accuracy, AUC (Area Under the Curve), and confusion matrices to assess their performance. An ensemble model approach is also described and evaluated here.

## Inference:

To enhance predictive accuracy, implemented an ensemble model that integrates the individual predictions from the Logistic Regression, Random Forest, and SVM models. This ensemble approach leveraged the strengths of each model to provide a more robust prediction. The ensemble model achieved an accuracy of approximately 93.6%, demonstrating that it is more effective and reliable than relying on a single model, thereby improving our ability to accurately diagnose appendicitis in clinical settings.

The ensemble model, which integrates outputs from multiple models, provided the best accuracy, demonstrating its effectiveness in clinical decision-making for appendicitis. Accuracy alone doesn’t tell the full story, the ensemble model also had a high area under the curve (AUC) score of about 0.97. This score reflects a well-balanced sensitivity, crucial for correctly identifying true cases of appendicitis, and specificity, important for accurately excluding false positives, thereby providing a reliable tool in medical diagnostics.

## References:
Clinical Data Repository: Zenodo. Access the data here[   ](https://zenodo.org/records/7669442/files/app_data.xlsx?download=1) Accessed on [04/15/2024].

## Files:
LimmalaP.DA5030.Project.rmd

## How to Use:
To replicate or further develop this project, you can clone the repository and follow the steps in the R studio. Ensure you meet the requirements listed in requirements.txt to set up your environment.

## Licence: Pranathi Limmala at Northeastern University.

## Contact:
Name: Pranathi Limmala 
email: pranathilimmala@gmail.com
