library(testthat)
dataset_path <- "C:/Users/User/OneDrive/Documents/KD24203 DMW/Assignment Project/heart_disease_dataset.csv"

#Unit testing
test_that("Data loading works correctly", {
  heart_disease <- read.csv(dataset_path)
  expect_s3_class(heart_disease, "data.frame")
  expect_false(nrow(heart_disease) == 0)
})

test_that("Summary statistics are calculated correctly", {
  heart_disease <- read.csv(dataset_path)
  summary_stats_age <- summary(heart_disease$Age)
  expect_true(is.numeric(summary_stats_age[["Min."]]))
  expect_true(is.numeric(summary_stats_age[["Max."]]))
  expect_true(is.numeric(summary_stats_age[["Mean"]]))
})

test_that("Frequency tables are created correctly", {
  heart_disease <- read.csv(dataset_path)
  gender_table <- table(heart_disease$Gender)
  expect_true(is.table(gender_table))
  expect_true(all(names(gender_table) %in% c("Male", "Female")))
})

#Integration testing
test_that("Full data processing pipeline works correctly", {
  # Load data
  heart_disease <- read.csv(dataset_path)
  
  # Summary statistics
  summary_stats_age <- summary(heart_disease$Age)
  summary_stats_chol <- summary(heart_disease$Cholesterol)
  
  # Check that summary statistics are numeric and valid
  expect_true(is.numeric(summary_stats_age[["Min."]]))
  expect_true(is.numeric(summary_stats_chol[["Min."]]))
  
  # Counts and proportions
  gender_table <- table(heart_disease$Gender)
  heart_disease_table <- table(heart_disease$Heart.Disease)
  heart_disease_proportion <- prop.table(heart_disease_table)
  
  # Check that tables are valid
  expect_true(is.table(gender_table))
  expect_true(is.table(heart_disease_table))
  expect_true(is.numeric(heart_disease_proportion))
  
  # Correlation
  corr_matrix <- cor(heart_disease[, c("Age", "Cholesterol", "Blood.Pressure", "Heart.Rate", "Blood.Sugar")])
  
  # Check that correlation matrix is numeric and not NA
  expect_true(is.matrix(corr_matrix))
  expect_false(any(is.na(corr_matrix)))
  
  # Group-wise statistics
  mean_age_by_hd <- aggregate(Age ~ Heart.Disease, data = heart_disease, FUN = mean)
  median_chol_by_gender <- aggregate(Cholesterol ~ Gender, data = heart_disease, FUN = median)
  
  # Check that group-wise statistics are numeric
  expect_true(is.numeric(mean_age_by_hd$Age))
  expect_true(is.numeric(median_chol_by_gender$Cholesterol))
  
  # Frequency tables
  gender_hd_table <- table(heart_disease$Gender, heart_disease$Heart.Disease)
  smoking_hd_table <- table(heart_disease$Smoking, heart_disease$Heart.Disease)
  
  # Check that frequency tables are valid
  expect_true(is.table(gender_hd_table))
  expect_true(is.table(smoking_hd_table))
})


##To do the testing
#To run the tests, you can execute the script directly in RStudio or use the test_file() function from the testthat package in R console:
testthat::test_file("C:/Users/User/OneDrive/Documents/KT24602 SE/Software Project/test_data_processing.R")
