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


# Modify inputs in data
# stroke_data_formatted <- stri_replace_all_regex(

