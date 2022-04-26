library(shiny)
library(shinythemes)
library(shinyWidgets)
library(ggplot2)
library(stringr)
library(stringi)
library(rpart)
library(rpart.plot)
library(formattable)
library(kableExtra)
# library(DT)
library(dplyr)
library(purrr)
library(RColorBrewer)


stroke_data <- read.csv("../data/stroke_data_cleanedJB.csv")
# Remove id column
stroke_data <- stroke_data[, 2:12]
# list_features <- stri_replace_all_regex(colnames(stroke_data),
#                                   pattern=c('age', 'hypertension', 'heart_disease', 'ever_married',
#                                             'work_type', 'Residence_type', 'avg_glucose_level', 'bmi', ''),
#                                   replacement=c('Age', 'Hypertension', 'Heart disease', 'Ever married',
#                                                 'Work type', 'Residence type', 'Average glucose level', 'BMI',
#                                                 'Smoking status'),
#                                   vectorize=FALSE)
list_features <- colnames(stroke_data)
list_features_categorical <- c("gender", "hypertension", "heart_disease", "ever_married",
                               "work_type", "Residence_type", "smoking_status", "stroke")
list_features_continuous <- c("age", "avg_glucose_level", "bmi")
list_features_formatted <- str_to_title(list_features)
list_features_formatted <- gsub("_", " ", 
                      gsub("Avg", "Average", 
                           gsub("Bmi", "BMI", list_features_formatted)))
# list_features_categorical_formatted <- c("Gender", "Hypertension", "Heart Disease", "Ever Married",
#                                "Work Type", "Residence Type", "Smoking Status", "Stroke")
# list_features_continuous_formatted <- c("Age", "Average Glucose Level", "BMI")


stroke_data_formatted <- stroke_data
for (binary_column in c("hypertension", "heart_disease", "stroke")) {
  stroke_data_formatted[binary_column][stroke_data_formatted[binary_column] == 0] <- "No"
  stroke_data_formatted[binary_column][stroke_data_formatted[binary_column] == 1] <- "Yes"
}

stroke_data_formatted$work_type <- str_replace_all(stroke_data_formatted$work_type, c("children" = "Child",
                                                                                      "Govt_job" = "Government job",
                                                                                      "Never_worked" = "Never worked"))
stroke_data_formatted$smoking_status <- str_replace_all(stroke_data_formatted$smoking_status, c("never smoked" = "Never smoker",
                                                                                                "smokes" = "Current smoker",
                                                                                                "formerly smoked" = "Former smoker"))
